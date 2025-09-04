;; CommunityVault - Decentralized Mutual Aid and Resource Sharing Platform

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-INVALID-NEED (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))
(define-constant ERR-NEED-NOT-FOUND (err u104))
(define-constant ERR-ALREADY-FULFILLED (err u105))
(define-constant ERR-INVALID-VERIFICATION (err u106))
(define-constant ERR-ESCROW-LOCKED (err u107))
(define-constant ERR-INVALID-CONTRIBUTION (err u108))
(define-constant ERR-CAPABILITY-CHAIN-BROKEN (err u109))
(define-constant ERR-COMMUNITY-NOT-FOUND (err u110))
(define-constant ERR-ORACLE-VERIFICATION-FAILED (err u111))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u112))
(define-constant ERR-TIME-LOCK-ACTIVE (err u113))
(define-constant ERR-INVALID-PARAMETERS (err u114))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-NEED-AMOUNT u1000000)
(define-constant MIN-VERIFICATION-THRESHOLD u3)
(define-constant REPUTATION-MULTIPLIER u100)
(define-constant TIME-LOCK-BLOCKS u144) ;; ~24 hours

;; Data Variables
(define-data-var next-need-id uint u1)
(define-data-var next-community-id uint u1)
(define-data-var total-communities uint u0)
(define-data-var platform-fee-rate uint u250) ;; 2.5%
(define-data-var emergency-mode bool false)
(define-data-var governance-token-supply uint u0)

;; Data Maps
(define-map Communities
    { community-id: uint }
    {
        creator: principal,
        name: (string-ascii 64),
        total-members: uint,
        total-assistance: uint,
        reputation-threshold: uint,
        is-active: bool,
        governance-tokens: uint
    }
)

(define-map CommunityMembers
    { community-id: uint, member: principal }
    {
        joined-block: uint,
        reputation-score: uint,
        contributions-made: uint,
        assistance-received: uint,
        capability-chain-score: uint,
        sliding-scale-capacity: uint
    }
)

(define-map NeedRequests
    { need-id: uint }
    {
        community-id: uint,
        requester: principal,
        need-type: (string-ascii 32),
        amount-needed: uint,
        privacy-hash: (buff 32),
        verification-count: uint,
        is-fulfilled: bool,
        created-block: uint,
        fulfillment-deadline: uint,
        impact-score: uint
    }
)

(define-map NeedVerifications
    { need-id: uint, verifier: principal }
    {
        verification-type: (string-ascii 16),
        verification-hash: (buff 32),
        timestamp: uint,
        oracle-score: uint
    }
)

(define-map ResourceContributions
    { need-id: uint, contributor: principal }
    {
        amount: uint,
        contribution-type: (string-ascii 32),
        escrow-block: uint,
        is-released: bool,
        fractional-share: uint
    }
)

(define-map EscrowFunds
    { need-id: uint }
    {
        total-amount: uint,
        contributors-count: uint,
        release-conditions-met: bool,
        time-lock-end: uint
    }
)

(define-map ImpactTokens
    { holder: principal }
    {
        total-tokens: uint,
        community-tokens: uint,
        governance-power: uint,
        earned-from-assistance: uint
    }
)

(define-map HealingCircles
    { circle-id: uint }
    {
        community-id: uint,
        circle-type: (string-ascii 32),
        participants: uint,
        privacy-level: uint,
        coordinator: principal,
        is-active: bool
    }
)

(define-map CrisisProtocols
    { community-id: uint, crisis-type: (string-ascii 32) }
    {
        activation-threshold: uint,
        resource-multiplier: uint,
        auto-verify: bool,
        emergency-contacts: uint,
        is-activated: bool
    }
)

(define-map CrossCommunityNetworks
    { network-id: uint }
    {
        primary-community: uint,
        connected-communities: uint,
        shared-resources: uint,
        governance-model: (string-ascii 16)
    }
)

;; Private Functions
(define-private (validate-community-member (community-id uint) (member principal))
    (match (map-get? CommunityMembers { community-id: community-id, member: member })
        member-data (ok true)
        ERR-NOT-AUTHORIZED
    )
)

(define-private (calculate-sliding-scale-contribution (member principal) (amount uint))
    (let (
        (impact-tokens (default-to { total-tokens: u0, community-tokens: u0, governance-power: u0, earned-from-assistance: u0 }
            (map-get? ImpactTokens { holder: member })))
    )
    (ok (/ (* amount (get total-tokens impact-tokens)) u1000))
    )
)

(define-private (update-reputation-score (member principal) (community-id uint) (points uint))
    (match (map-get? CommunityMembers { community-id: community-id, member: member })
        member-data
        (map-set CommunityMembers 
            { community-id: community-id, member: member }
            (merge member-data { 
                reputation-score: (+ (get reputation-score member-data) points),
                capability-chain-score: (+ (get capability-chain-score member-data) (/ points u2))
            })
        )
        false
    )
)

(define-private (validate-need-verification (need-id uint))
    (match (map-get? NeedRequests { need-id: need-id })
        need-data
        (ok (>= (get verification-count need-data) MIN-VERIFICATION-THRESHOLD))
        ERR-NEED-NOT-FOUND
    )
)

(define-private (check-crisis-activation (community-id uint) (crisis-type (string-ascii 32)))
    (match (map-get? CrisisProtocols { community-id: community-id, crisis-type: crisis-type })
        protocol-data
        (not (get is-activated protocol-data))
        true
    )
)

;; Public Functions
(define-public (create-community (name (string-ascii 64)) (reputation-threshold uint))
    (let (
        (community-id (var-get next-community-id))
    )
    (asserts! (> (len name) u0) ERR-INVALID-PARAMETERS)
    (asserts! (<= reputation-threshold u10000) ERR-INVALID-PARAMETERS)
    
    (map-set Communities
        { community-id: community-id }
        {
            creator: tx-sender,
            name: name,
            total-members: u1,
            total-assistance: u0,
            reputation-threshold: reputation-threshold,
            is-active: true,
            governance-tokens: u1000
        }
    )
    
    (map-set CommunityMembers
        { community-id: community-id, member: tx-sender }
        {
            joined-block: block-height,
            reputation-score: u1000,
            contributions-made: u0,
            assistance-received: u0,
            capability-chain-score: u100,
            sliding-scale-capacity: u100
        }
    )
    
    (var-set next-community-id (+ community-id u1))
    (var-set total-communities (+ (var-get total-communities) u1))
    (ok community-id)
    )
)

(define-public (join-community (community-id uint))
    (match (map-get? Communities { community-id: community-id })
        community-data
        (begin
            (asserts! (get is-active community-data) ERR-COMMUNITY-NOT-FOUND)
            (asserts! (is-none (map-get? CommunityMembers { community-id: community-id, member: tx-sender })) ERR-INVALID-PARAMETERS)
            
            (map-set CommunityMembers
                { community-id: community-id, member: tx-sender }
                {
                    joined-block: block-height,
                    reputation-score: u100,
                    contributions-made: u0,
                    assistance-received: u0,
                    capability-chain-score: u10,
                    sliding-scale-capacity: u50
                }
            )
            
            (map-set Communities
                { community-id: community-id }
                (merge community-data { total-members: (+ (get total-members community-data) u1) })
            )
            (ok true)
        )
        ERR-COMMUNITY-NOT-FOUND
    )
)

(define-public (submit-need-request 
    (community-id uint) 
    (need-type (string-ascii 32)) 
    (amount-needed uint) 
    (privacy-hash (buff 32))
    (fulfillment-deadline uint))
    (let (
        (need-id (var-get next-need-id))
    )
    (asserts! (<= amount-needed MAX-NEED-AMOUNT) ERR-INVALID-AMOUNT)
    (asserts! (> fulfillment-deadline block-height) ERR-INVALID-PARAMETERS)
    (try! (validate-community-member community-id tx-sender))
    
    (map-set NeedRequests
        { need-id: need-id }
        {
            community-id: community-id,
            requester: tx-sender,
            need-type: need-type,
            amount-needed: amount-needed,
            privacy-hash: privacy-hash,
            verification-count: u0,
            is-fulfilled: false,
            created-block: block-height,
            fulfillment-deadline: fulfillment-deadline,
            impact-score: u0
        }
    )
    
    (var-set next-need-id (+ need-id u1))
    (ok need-id)
    )
)

(define-public (verify-need-request 
    (need-id uint) 
    (verification-type (string-ascii 16)) 
    (verification-hash (buff 32))
    (oracle-score uint))
    (match (map-get? NeedRequests { need-id: need-id })
        need-data
        (begin
            (asserts! (not (get is-fulfilled need-data)) ERR-ALREADY-FULFILLED)
            (asserts! (<= oracle-score u100) ERR-INVALID-VERIFICATION)
            (try! (validate-community-member (get community-id need-data) tx-sender))
            
            (map-set NeedVerifications
                { need-id: need-id, verifier: tx-sender }
                {
                    verification-type: verification-type,
                    verification-hash: verification-hash,
                    timestamp: block-height,
                    oracle-score: oracle-score
                }
            )
            
            (map-set NeedRequests
                { need-id: need-id }
                (merge