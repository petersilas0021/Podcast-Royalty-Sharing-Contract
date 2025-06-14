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
