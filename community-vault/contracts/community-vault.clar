;; CommunityVault - Fixed Decentralized Mutual Aid Platform

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-NEED-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-FULFILLED (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-COMMUNITY-NOT-FOUND (err u105))
(define-constant ERR-NOT-MEMBER (err u106))
(define-constant ERR-INVALID-INPUT (err u107))
(define-constant ERR-ALREADY-WITHDRAWN (err u108))
(define-constant ERR-CANNOT-CONTRIBUTE-TO-OWN-REQUEST (err u109))
(define-constant ERR-ALREADY-MEMBER (err u110))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-NEED-AMOUNT u100000000) ;; 100 STX (in micro-STX)
(define-constant MIN-REPUTATION u10)
(define-constant INITIAL-REPUTATION u100)
(define-constant NEW-MEMBER-REPUTATION u50)
(define-constant REPUTATION-PER-MICROSTX u100) ;; 1 reputation point per 100 micro-STX

;; Data Variables
(define-data-var next-need-id uint u1)
(define-data-var next-community-id uint u1)

;; Data Maps
(define-map Communities
    { community-id: uint }
    {
        name: (string-ascii 50),
        creator: principal,
        member-count: uint,
        total-helped: uint,
        is-active: bool,
        created-at: uint
    }
)

(define-map Members
    { community-id: uint, member: principal }
    {
        reputation: uint,
        contributions: uint,
        requests-made: uint,
        joined-at: uint
    }
)

(define-map NeedRequests
    { need-id: uint }
    {
        community-id: uint,
        requester: principal,
        title: (string-ascii 100),
        amount: uint,
        fulfilled: bool,
        withdrawn: bool,
        created-at: uint,
        total-contributed: uint
    }
)

(define-map Contributions
    { need-id: uint, contributor: principal }
    {
        amount: uint,
        contributed-at: uint
    }
)

(define-map ContributorTotals
    { need-id: uint, contributor: principal }
    { total-amount: uint }
)

;; Read-only Functions
(define-read-only (get-community (community-id uint))
    (map-get? Communities { community-id: community-id })
)

(define-read-only (get-member (community-id uint) (member principal))
    (map-get? Members { community-id: community-id, member: member })
)

(define-read-only (get-need-request (need-id uint))
    (map-get? NeedRequests { need-id: need-id })
)

(define-read-only (get-contribution (need-id uint) (contributor principal))
    (map-get? Contributions { need-id: need-id, contributor: contributor })
)

(define-read-only (get-contributor-total (need-id uint) (contributor principal))
    (default-to 
        { total-amount: u0 }
        (map-get? ContributorTotals { need-id: need-id, contributor: contributor })
    )
)

(define-read-only (is-member (community-id uint) (member principal))
    (is-some (map-get? Members { community-id: community-id, member: member }))
)

(define-read-only (get-next-need-id)
    (var-get next-need-id)
)

(define-read-only (get-next-community-id)
    (var-get next-community-id)
)

;; Private Functions
(define-private (validate-member (community-id uint) (member principal))
    (if (is-member community-id member)
        (ok true)
        ERR-NOT-MEMBER
    )
)

(define-private (validate-community-exists (community-id uint))
    (let (
        (community-opt (get-community community-id))
    )
        (if (is-some community-opt)
            (let (
                (community-data (unwrap-panic community-opt))
            )
                (if (get is-active community-data)
                    (ok community-data)
                    ERR-COMMUNITY-NOT-FOUND)
            )
            ERR-COMMUNITY-NOT-FOUND
        )
    )
)

(define-private (update-member-reputation (community-id uint) (member principal) (points uint))
    (let (
        (member-opt (get-member community-id member))
    )
        (if (is-some member-opt)
            (let (
                (member-data (unwrap-panic member-opt))
            )
                (map-set Members
                    { community-id: community-id, member: member }
                    (merge member-data { 
                        reputation: (+ (get reputation member-data) points)
                    })
                )
                (ok true)
            )
            ERR-NOT-MEMBER
        )
    )
)

;; Public Functions

;; Create a new community
(define-public (create-community (name (string-ascii 50)))
    (let (
        (community-id (var-get next-community-id))
    )
        (asserts! (> (len name) u0) ERR-INVALID-INPUT)
        (asserts! (<= (len name) u50) ERR-INVALID-INPUT)
        
        (map-set Communities
            { community-id: community-id }
            {
                name: name,
                creator: tx-sender,
                member-count: u1,
                total-helped: u0,
                is-active: true,
                created-at: block-height
            }
        )
        
        (map-set Members
            { community-id: community-id, member: tx-sender }
            {
                reputation: INITIAL-REPUTATION,
                contributions: u0,
                requests-made: u0,
                joined-at: block-height
            }
        )
        
        (var-set next-community-id (+ community-id u1))
        (ok community-id)
    )
)

;; Join an existing community
(define-public (join-community (community-id uint))
    (let (
        (community-data (try! (validate-community-exists community-id)))
    )
        (asserts! (not (is-member community-id tx-sender)) ERR-ALREADY-MEMBER)
        
        (map-set Members
            { community-id: community-id, member: tx-sender }
            {
                reputation: NEW-MEMBER-REPUTATION,
                contributions: u0,
                requests-made: u0,
                joined-at: block-height
            }
        )
        
        (map-set Communities
            { community-id: community-id }
            (merge community-data { 
                member-count: (+ (get member-count community-data) u1)
            })
        )
        
        (ok true)
    )
)

;; Submit a request for help
(define-public (request-help 
    (community-id uint) 
    (title (string-ascii 100)) 
    (amount uint))
    (let (
        (need-id (var-get next-need-id))
        (member-data (unwrap! (get-member community-id tx-sender) ERR-NOT-MEMBER))
    )
        (asserts! (> (len title) u0) ERR-INVALID-INPUT)
        (asserts! (<= (len title) u100) ERR-INVALID-INPUT)
        (asserts! (and (> amount u0) (<= amount MAX-NEED-AMOUNT)) ERR-INVALID-AMOUNT)
        (try! (validate-community-exists community-id))
        (try! (validate-member community-id tx-sender))
        (asserts! (>= (get reputation member-data) MIN-REPUTATION) ERR-NOT-AUTHORIZED)
        
        (map-set NeedRequests
            { need-id: need-id }
            {
                community-id: community-id,
                requester: tx-sender,
                title: title,
                amount: amount,
                fulfilled: false,
                withdrawn: false,
                created-at: block-height,
                total-contributed: u0
            }
        )
        
        (map-set Members
            { community-id: community-id, member: tx-sender }
            (merge member-data { 
                requests-made: (+ (get requests-made member-data) u1)
            })
        )
        
        (var-set next-need-id (+ need-id u1))
        (ok need-id)
    )
)

;; Contribute to a help request
(define-public (contribute-help (need-id uint) (amount uint))
    (let (
        (need-data (unwrap! (get-need-request need-id) ERR-NEED-NOT-FOUND))
        (current-total (get total-contributed need-data))
        (contributor-current (get total-amount (get-contributor-total need-id tx-sender)))
        (community-id (get community-id need-data))
    )
        (asserts! (not (get fulfilled need-data)) ERR-ALREADY-FULFILLED)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (not (is-eq tx-sender (get requester need-data))) ERR-CANNOT-CONTRIBUTE-TO-OWN-REQUEST)
        (try! (validate-member community-id tx-sender))
        
        ;; Transfer STX to contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Update contribution record
        (map-set Contributions
            { need-id: need-id, contributor: tx-sender }
            {
                amount: amount,
                contributed-at: block-height
            }
        )
        
        ;; Update contributor's total contributions for this need
        (map-set ContributorTotals
            { need-id: need-id, contributor: tx-sender }
            { total-amount: (+ contributor-current amount) }
        )
        
        ;; Update need request total
        (let (
            (new-total (+ current-total amount))
            (is-now-fulfilled (>= new-total (get amount need-data)))
        )
            (map-set NeedRequests
                { need-id: need-id }
                (merge need-data { 
                    total-contributed: new-total,
                    fulfilled: is-now-fulfilled
                })
            )
        )
        
        ;; Update contributor reputation
        (try! (update-member-reputation 
            community-id 
            tx-sender 
            (/ amount REPUTATION-PER-MICROSTX)))
        
        ;; Update contributor's contribution count
        (let (
            (member-data (unwrap! (get-member community-id tx-sender) ERR-NOT-MEMBER))
        )
            (map-set Members
                { community-id: community-id, member: tx-sender }
                (merge member-data { 
                    contributions: (+ (get contributions member-data) u1)
                })
            )
        )
        
        (ok true)
    )
)

;; Withdraw fulfilled funds (only requester can call)
(define-public (withdraw-help (need-id uint))
    (let (
        (need-data (unwrap! (get-need-request need-id) ERR-NEED-NOT-FOUND))
        (community-id (get community-id need-data))
    )
        (asserts! (is-eq tx-sender (get requester need-data)) ERR-NOT-AUTHORIZED)
        (asserts! (get fulfilled need-data) ERR-INSUFFICIENT-FUNDS)
        (asserts! (not (get withdrawn need-data)) ERR-ALREADY-WITHDRAWN)
        
        ;; Mark as withdrawn first to prevent re-entrancy
        (map-set NeedRequests
            { need-id: need-id }
            (merge need-data { withdrawn: true })
        )
        
        ;; Transfer funds to requester
        (try! (as-contract (stx-transfer? (get total-contributed need-data) tx-sender (get requester need-data))))
        
        ;; Update community total helped
        (let (
            (community-data (unwrap! (get-community community-id) ERR-COMMUNITY-NOT-FOUND))
        )
            (map-set Communities
                { community-id: community-id }
                (merge community-data { 
                    total-helped: (+ (get total-helped community-data) u1)
                })
            )
        )
        
        (ok true)
    )
)

;; Emergency function to deactivate a community (only creator can call)
(define-public (deactivate-community (community-id uint))
    (let (
        (community-data (unwrap! (get-community community-id) ERR-COMMUNITY-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get creator community-data)) ERR-NOT-AUTHORIZED)
        
        (map-set Communities
            { community-id: community-id }
            (merge community-data { is-active: false })
        )
        
        (ok true)
    )
)