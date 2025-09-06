(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-PERCENTAGE (err u101))
(define-constant ERR-NO-PODCAST (err u102))
(define-constant ERR-HOST-EXISTS (err u103))
(define-constant ERR-HOST-NOT-FOUND (err u104))
(define-constant ERR-INSUFFICIENT-FUNDS (err u105))

(define-data-var contract-owner principal tx-sender)

(define-map podcasts
    { podcast-id: uint }
    {
        name: (string-ascii 64),
        total-revenue: uint,
        host-count: uint,
        created-at: uint,
    }
)

(define-map podcast-hosts
    {
        podcast-id: uint,
        host: principal,
    }
    {
        share-percentage: uint,
        earnings: uint,
        joined-at: uint,
    }
)

(define-map revenue-distributions
    { distribution-id: uint }
    {
        podcast-id: uint,
        amount: uint,
        distributed-at: uint,
    }
)

(define-data-var next-podcast-id uint u1)
(define-data-var next-distribution-id uint u1)

(define-read-only (get-podcast (podcast-id uint))
    (map-get? podcasts { podcast-id: podcast-id })
)

(define-read-only (get-host-info
        (podcast-id uint)
        (host principal)
    )
    (map-get? podcast-hosts {
        podcast-id: podcast-id,
        host: host,
    })
)

(define-read-only (get-distribution (distribution-id uint))
    (map-get? revenue-distributions { distribution-id: distribution-id })
)

(define-public (create-podcast (name (string-ascii 64)))
    (let ((podcast-id (var-get next-podcast-id)))
        (map-set podcasts { podcast-id: podcast-id } {
            name: name,
            total-revenue: u0,
            host-count: u1,
            created-at: burn-block-height,
        })
        (map-set podcast-hosts {
            podcast-id: podcast-id,
            host: tx-sender,
        } {
            share-percentage: u100,
            earnings: u0,
            joined-at: burn-block-height,
        })
        (map-set host-analytics {
            podcast-id: podcast-id,
            host: tx-sender,
        } {
            activity-score: u0,
            episodes-contributed: u0,
            total-distributions-received: u0,
            last-activity-height: burn-block-height,
            performance-rating: u50,
        })
        (var-set next-podcast-id (+ podcast-id u1))
        (ok podcast-id)
    )
)

(define-public (add-host
        (podcast-id uint)
        (new-host principal)
        (share-percentage uint)
    )
    (let (
            (podcast (unwrap! (get-podcast podcast-id) ERR-NO-PODCAST))
            (host-info (get-host-info podcast-id tx-sender))
        )
        (asserts! (is-some host-info) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (get-host-info podcast-id new-host)) ERR-HOST-EXISTS)
        (asserts! (<= share-percentage u100) ERR-INVALID-PERCENTAGE)
        (map-set podcast-hosts {
            podcast-id: podcast-id,
            host: new-host,
        } {
            share-percentage: share-percentage,
            earnings: u0,
            joined-at: burn-block-height,
        })
        (map-set host-analytics {
            podcast-id: podcast-id,
            host: new-host,
        } {
            activity-score: u0,
            episodes-contributed: u0,
            total-distributions-received: u0,
            last-activity-height: burn-block-height,
            performance-rating: u50,
        })
        (map-set podcasts { podcast-id: podcast-id } {
            name: (get name podcast),
            total-revenue: (get total-revenue podcast),
            host-count: (+ (get host-count podcast) u1),
            created-at: (get created-at podcast),
        })
        (ok true)
    )
)
(define-public (update-share-percentage
        (podcast-id uint)
        (host principal)
        (new-percentage uint)
    )
    (let (
            (podcast (unwrap! (get-podcast podcast-id) ERR-NO-PODCAST))
            (host-info (unwrap! (get-host-info podcast-id host) ERR-HOST-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-percentage u100) ERR-INVALID-PERCENTAGE)
        (map-set podcast-hosts {
            podcast-id: podcast-id,
            host: host,
        } {
            share-percentage: new-percentage,
            earnings: (get earnings host-info),
            joined-at: (get joined-at host-info),
        })
        (ok true)
    )
)

(define-public (distribute-revenue
        (podcast-id uint)
        (amount uint)
    )
    (let (
            (podcast (unwrap! (get-podcast podcast-id) ERR-NO-PODCAST))
            (distribution-id (var-get next-distribution-id))
        )
        (asserts! (>= (stx-get-balance tx-sender) amount) ERR-INSUFFICIENT-FUNDS)
        (map-set revenue-distributions { distribution-id: distribution-id } {
            podcast-id: podcast-id,
            amount: amount,
            distributed-at: burn-block-height,
        })
        (map-set podcasts { podcast-id: podcast-id } {
            name: (get name podcast),
            total-revenue: (+ (get total-revenue podcast) amount),
            host-count: (get host-count podcast),
            created-at: (get created-at podcast),
        })
        (var-set next-distribution-id (+ distribution-id u1))
        (ok true)
    )
)
(define-public (withdraw-earnings (podcast-id uint))
    (let (
            (host-info (unwrap! (get-host-info podcast-id tx-sender) ERR-HOST-NOT-FOUND))
            (earnings (get earnings host-info))
        )
        (asserts! (> earnings u0) ERR-INSUFFICIENT-FUNDS)
        (map-set podcast-hosts {
            podcast-id: podcast-id,
            host: tx-sender,
        } {
            share-percentage: (get share-percentage host-info),
            earnings: u0,
            joined-at: (get joined-at host-info),
        })
        (stx-transfer? earnings tx-sender (var-get contract-owner))
    )
)

(define-private (calculate-host-share
        (amount uint)
        (percentage uint)
    )
    (/ (* amount percentage) u100)
)

(define-private (distribute-to-host
        (podcast-id uint)
        (host principal)
        (amount uint)
    )
    (let (
            (host-info (unwrap! (get-host-info podcast-id host) ERR-HOST-NOT-FOUND))
            (host-share (calculate-host-share amount (get share-percentage host-info)))
        )
        (map-set podcast-hosts {
            podcast-id: podcast-id,
            host: host,
        } {
            share-percentage: (get share-percentage host-info),
            earnings: (+ (get earnings host-info) host-share),
            joined-at: (get joined-at host-info),
        })
        (ok host-share)
    )
)

(define-public (distribute-revenue-to-hosts
        (podcast-id uint)
        (amount uint)
        (hosts (list 10 principal))
    )
    (let (
            (podcast (unwrap! (get-podcast podcast-id) ERR-NO-PODCAST))
            (distribution-id (var-get next-distribution-id))
        )
        (asserts! (>= (stx-get-balance tx-sender) amount) ERR-INSUFFICIENT-FUNDS)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map distribute-to-host
            (list
                podcast-id                 podcast-id                 podcast-id
                podcast-id                 podcast-id
                podcast-id                 podcast-id                 podcast-id
                podcast-id                 podcast-id
            )
            hosts
            (list
                amount                 amount                 amount
                amount                 amount                 amount
                amount                 amount                 amount
                amount
            ))
        (map-set revenue-distributions { distribution-id: distribution-id } {
            podcast-id: podcast-id,
            amount: amount,
            distributed-at: burn-block-height,
        })
        (map-set podcasts { podcast-id: podcast-id } {
            name: (get name podcast),
            total-revenue: (+ (get total-revenue podcast) amount),
            host-count: (get host-count podcast),
            created-at: (get created-at podcast),
        })
        (var-set next-distribution-id (+ distribution-id u1))
        (ok true)
    )
)

(define-public (withdraw-earnings-fixed (podcast-id uint))
    (let (
            (host-info (unwrap! (get-host-info podcast-id tx-sender) ERR-HOST-NOT-FOUND))
            (earnings (get earnings host-info))
        )
        (asserts! (> earnings u0) ERR-INSUFFICIENT-FUNDS)
        (map-set podcast-hosts {
            podcast-id: podcast-id,
            host: tx-sender,
        } {
            share-percentage: (get share-percentage host-info),
            earnings: u0,
            joined-at: (get joined-at host-info),
        })
        (as-contract (stx-transfer? earnings tx-sender tx-sender))
    )
)

(define-constant ERR-PROPOSAL-NOT-FOUND (err u106))
(define-constant ERR-ALREADY-VOTED (err u107))
(define-constant ERR-VOTING-ENDED (err u108))
(define-constant ERR-PROPOSAL-NOT-PASSED (err u109))
(define-constant ERR-INVALID-ACTIVITY-SCORE (err u110))
(define-constant ERR-MILESTONE-ALREADY-CLAIMED (err u111))
(define-constant ERR-MILESTONE-NOT-REACHED (err u112))

(define-map proposals
    { proposal-id: uint }
    {
        podcast-id: uint,
        proposer: principal,
        proposal-type: (string-ascii 32),
        target-host: (optional principal),
        new-percentage: (optional uint),
        description: (string-ascii 256),
        yes-votes: uint,
        no-votes: uint,
        total-eligible-voters: uint,
        created-at: uint,
        voting-end-height: uint,
        executed: bool,
    }
)

(define-map votes
    {
        proposal-id: uint,
        voter: principal,
    }
    {
        vote: bool,
        voted-at: uint,
    }
)

(define-data-var next-proposal-id uint u1)

(define-map revenue-milestones
    {
        podcast-id: uint,
        milestone-amount: uint,
    }
    {
        reward-percentage: uint,
        claimed: bool,
        achieved-at: (optional uint),
    }
)

(define-data-var milestone-thresholds (list 5 uint) (list u1000000 u5000000 u10000000 u25000000 u50000000))

(define-map host-analytics
    {
        podcast-id: uint,
        host: principal,
    }
    {
        activity-score: uint,
        episodes-contributed: uint,
        total-distributions-received: uint,
        last-activity-height: uint,
        performance-rating: uint,
    }
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote
        (proposal-id uint)
        (voter principal)
    )
    (map-get? votes {
        proposal-id: proposal-id,
        voter: voter,
    })
)

(define-read-only (get-host-analytics
        (podcast-id uint)
        (host principal)
    )
    (map-get? host-analytics {
        podcast-id: podcast-id,
        host: host,
    })
)

(define-read-only (get-milestone
        (podcast-id uint)
        (milestone-amount uint)
    )
    (map-get? revenue-milestones {
        podcast-id: podcast-id,
        milestone-amount: milestone-amount,
    })
)

(define-read-only (get-milestone-thresholds)
    (var-get milestone-thresholds)
)

(define-public (create-proposal
        (podcast-id uint)
        (proposal-type (string-ascii 32))
        (target-host (optional principal))
        (new-percentage (optional uint))
        (description (string-ascii 256))
    )
    (let (
            (podcast (unwrap! (get-podcast podcast-id) ERR-NO-PODCAST))
            (host-info (unwrap! (get-host-info podcast-id tx-sender) ERR-NOT-AUTHORIZED))
            (proposal-id (var-get next-proposal-id))
        )
        (map-set proposals { proposal-id: proposal-id } {
            podcast-id: podcast-id,
            proposer: tx-sender,
            proposal-type: proposal-type,
            target-host: target-host,
            new-percentage: new-percentage,
            description: description,
            yes-votes: u0,
            no-votes: u0,
            total-eligible-voters: (get host-count podcast),
            created-at: burn-block-height,
            voting-end-height: (+ burn-block-height u144),
            executed: false,
        })
        (var-set next-proposal-id (+ proposal-id u1))
        (ok proposal-id)
    )
)

(define-public (vote-on-proposal
        (proposal-id uint)
        (vote-yes bool)
    )
    (let (
            (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
            (host-info (unwrap! (get-host-info (get podcast-id proposal) tx-sender)
                ERR-NOT-AUTHORIZED
            ))
        )
        (asserts! (is-none (get-vote proposal-id tx-sender)) ERR-ALREADY-VOTED)
        (asserts! (<= burn-block-height (get voting-end-height proposal))
            ERR-VOTING-ENDED
        )
        (map-set votes {
            proposal-id: proposal-id,
            voter: tx-sender,
        } {
            vote: vote-yes,
            voted-at: burn-block-height,
        })
        (map-set proposals { proposal-id: proposal-id } {
            podcast-id: (get podcast-id proposal),
            proposer: (get proposer proposal),
            proposal-type: (get proposal-type proposal),
            target-host: (get target-host proposal),
            new-percentage: (get new-percentage proposal),
            description: (get description proposal),
            yes-votes: (if vote-yes
                (+ (get yes-votes proposal) u1)
                (get yes-votes proposal)
            ),
            no-votes: (if vote-yes
                (get no-votes proposal)
                (+ (get no-votes proposal) u1)
            ),
            total-eligible-voters: (get total-eligible-voters proposal),
            created-at: (get created-at proposal),
            voting-end-height: (get voting-end-height proposal),
            executed: (get executed proposal),
        })
        (ok true)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let (
            (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
            (majority-threshold (/ (get total-eligible-voters proposal) u2))
        )
        (asserts! (> burn-block-height (get voting-end-height proposal))
            ERR-VOTING-ENDED
        )
        (asserts! (not (get executed proposal)) ERR-PROPOSAL-NOT-PASSED)
        (asserts! (> (get yes-votes proposal) majority-threshold)
            ERR-PROPOSAL-NOT-PASSED
        )
        (map-set proposals { proposal-id: proposal-id } {
            podcast-id: (get podcast-id proposal),
            proposer: (get proposer proposal),
            proposal-type: (get proposal-type proposal),
            target-host: (get target-host proposal),
            new-percentage: (get new-percentage proposal),
            description: (get description proposal),
            yes-votes: (get yes-votes proposal),
            no-votes: (get no-votes proposal),
            total-eligible-voters: (get total-eligible-voters proposal),
            created-at: (get created-at proposal),
            voting-end-height: (get voting-end-height proposal),
            executed: true,
        })
        (ok true)
    )
)

(define-public (update-host-activity
        (podcast-id uint)
        (host principal)
        (activity-score uint)
    )
    (let (
            (podcast (unwrap! (get-podcast podcast-id) ERR-NO-PODCAST))
            (host-info (unwrap! (get-host-info podcast-id host) ERR-HOST-NOT-FOUND))
            (current-analytics (default-to {
                activity-score: u0,
                episodes-contributed: u0,
                total-distributions-received: u0,
                last-activity-height: u0,
                performance-rating: u50,
            }
                (get-host-analytics podcast-id host)
            ))
        )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (<= activity-score u100) ERR-INVALID-ACTIVITY-SCORE)
        (map-set host-analytics {
            podcast-id: podcast-id,
            host: host,
        } {
            activity-score: activity-score,
            episodes-contributed: (get episodes-contributed current-analytics),
            total-distributions-received: (get total-distributions-received current-analytics),
            last-activity-height: burn-block-height,
            performance-rating: (get performance-rating current-analytics),
        })
        (ok true)
    )
)

(define-public (record-episode-contribution
        (podcast-id uint)
        (host principal)
    )
    (let (
            (podcast (unwrap! (get-podcast podcast-id) ERR-NO-PODCAST))
            (host-info (unwrap! (get-host-info podcast-id host) ERR-HOST-NOT-FOUND))
            (current-analytics (default-to {
                activity-score: u0,
                episodes-contributed: u0,
                total-distributions-received: u0,
                last-activity-height: u0,
                performance-rating: u50,
            }
                (get-host-analytics podcast-id host)
            ))
        )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (map-set host-analytics {
            podcast-id: podcast-id,
            host: host,
        } {
            activity-score: (get activity-score current-analytics),
            episodes-contributed: (+ (get episodes-contributed current-analytics) u1),
            total-distributions-received: (get total-distributions-received current-analytics),
            last-activity-height: burn-block-height,
            performance-rating: (get performance-rating current-analytics),
        })
        (ok true)
    )
)

(define-public (update-performance-rating
        (podcast-id uint)
        (host principal)
        (rating uint)
    )
    (let (
            (podcast (unwrap! (get-podcast podcast-id) ERR-NO-PODCAST))
            (host-info (unwrap! (get-host-info podcast-id host) ERR-HOST-NOT-FOUND))
            (current-analytics (default-to {
                activity-score: u0,
                episodes-contributed: u0,
                total-distributions-received: u0,
                last-activity-height: u0,
                performance-rating: u50,
            }
                (get-host-analytics podcast-id host)
            ))
        )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (<= rating u100) ERR-INVALID-ACTIVITY-SCORE)
        (map-set host-analytics {
            podcast-id: podcast-id,
            host: host,
        } {
            activity-score: (get activity-score current-analytics),
            episodes-contributed: (get episodes-contributed current-analytics),
            total-distributions-received: (get total-distributions-received current-analytics),
            last-activity-height: (get last-activity-height current-analytics),
            performance-rating: rating,
        })
        (ok true)
    )
)

(define-public (setup-milestones (podcast-id uint))
    (let (
            (podcast (unwrap! (get-podcast podcast-id) ERR-NO-PODCAST))
            (host-info (unwrap! (get-host-info podcast-id tx-sender) ERR-NOT-AUTHORIZED))
            (thresholds (var-get milestone-thresholds))
        )
        (fold setup-milestone-folder thresholds podcast-id)
        (ok true)
    )
)

(define-private (setup-milestone-folder
        (threshold uint)
        (podcast-id uint)
    )
    (let ((reward-percentage (if (is-eq threshold u1000000)
            u5
            (if (is-eq threshold u5000000)
                u10
                (if (is-eq threshold u10000000)
                    u15
                    (if (is-eq threshold u25000000)
                        u20
                        u25
                    )
                )
            )
        )))
        (map-set revenue-milestones {
            podcast-id: podcast-id,
            milestone-amount: threshold,
        } {
            reward-percentage: reward-percentage,
            claimed: false,
            achieved-at: none,
        })
        podcast-id
    )
)

(define-public (check-milestone-achievement (podcast-id uint))
    (let (
            (podcast (unwrap! (get-podcast podcast-id) ERR-NO-PODCAST))
            (total-revenue (get total-revenue podcast))
            (thresholds (var-get milestone-thresholds))
        )
        (fold check-milestone-folder thresholds podcast-id)
        (ok true)
    )
)

(define-private (check-milestone-folder
        (threshold uint)
        (podcast-id uint)
    )
    (let (
            (podcast-result (get-podcast podcast-id))
            (milestone (map-get? revenue-milestones {
                podcast-id: podcast-id,
                milestone-amount: threshold,
            }))
        )
        (match podcast-result
            podcast (let ((total-revenue (get total-revenue podcast)))
                (match milestone
                    milestone-data (if (and (>= total-revenue threshold) (is-none (get achieved-at milestone-data)))
                        (begin
                            (map-set revenue-milestones {
                                podcast-id: podcast-id,
                                milestone-amount: threshold,
                            } {
                                reward-percentage: (get reward-percentage milestone-data),
                                claimed: (get claimed milestone-data),
                                achieved-at: (some burn-block-height),
                            })
                            podcast-id
                        )
                        podcast-id
                    )
                    podcast-id
                )
            )
            podcast-id
        )
    )
)

(define-public (claim-milestone-reward
        (podcast-id uint)
        (milestone-amount uint)
    )
    (let (
            (podcast (unwrap! (get-podcast podcast-id) ERR-NO-PODCAST))
            (host-info (unwrap! (get-host-info podcast-id tx-sender) ERR-NOT-AUTHORIZED))
            (milestone (unwrap! (get-milestone podcast-id milestone-amount)
                ERR-MILESTONE-NOT-REACHED
            ))
            (reward-amount (/ (* milestone-amount (get reward-percentage milestone)) u100))
        )
        (asserts! (not (get claimed milestone)) ERR-MILESTONE-ALREADY-CLAIMED)
        (asserts! (is-some (get achieved-at milestone)) ERR-MILESTONE-NOT-REACHED)
        (map-set revenue-milestones {
            podcast-id: podcast-id,
            milestone-amount: milestone-amount,
        } {
            reward-percentage: (get reward-percentage milestone),
            claimed: true,
            achieved-at: (get achieved-at milestone),
        })
        (as-contract (stx-transfer? reward-amount tx-sender tx-sender))
    )
)
