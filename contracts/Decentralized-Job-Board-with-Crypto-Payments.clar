(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-JOB-NOT-FOUND (err u101))
(define-constant ERR-INVALID-STATUS (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-ALREADY-APPLIED (err u104))

(define-data-var job-count uint u0)

(define-map Jobs 
    uint 
    {
        employer: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        payment: uint,
        status: (string-ascii 20),
        worker: (optional principal),
        created-at: uint
    }
)

(define-map Applications
    {job-id: uint, applicant: principal}
    {status: (string-ascii 20)}
)

(define-map UserReputations
    principal
    {rating: uint, jobs-completed: uint}
)

(define-public (create-job (title (string-ascii 100)) (description (string-ascii 500)) (payment uint))
    (let ((job-id (var-get job-count)))
        (try! (stx-transfer? payment tx-sender (as-contract tx-sender)))
        (map-set Jobs job-id
            {
                employer: tx-sender,
                title: title,
                description: description,
                payment: payment,
                status: "open",
                worker: none,
                created-at: burn-block-height
            }
        )
        (var-set job-count (+ job-id u1))
        (ok job-id)
    )
)

(define-public (apply-for-job (job-id uint))
    (let ((job (unwrap! (map-get? Jobs job-id) ERR-JOB-NOT-FOUND)))
        (asserts! (is-eq (get status job) "open") ERR-INVALID-STATUS)
        (map-set Applications {job-id: job-id, applicant: tx-sender} {status: "pending"})
        (ok true)
    )
)

(define-public (accept-application (job-id uint) (worker principal))
    (let ((job (unwrap! (map-get? Jobs job-id) ERR-JOB-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get employer job)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status job) "open") ERR-INVALID-STATUS)
        (map-set Jobs job-id (merge job {
            status: "in-progress",
            worker: (some worker)
        }))
        (ok true)
    )
)

(define-public (complete-job (job-id uint))
    (let ((job (unwrap! (map-get? Jobs job-id) ERR-JOB-NOT-FOUND)))
        (asserts! (is-eq (some tx-sender) (get worker job)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status job) "in-progress") ERR-INVALID-STATUS)
        (map-set Jobs job-id (merge job {status: "completed"}))
        (try! (as-contract (stx-transfer? (get payment job) tx-sender (get employer job))))
        (update-reputation tx-sender)
        (ok true)
    )
)

(define-read-only (get-job (job-id uint))
    (ok (unwrap! (map-get? Jobs job-id) ERR-JOB-NOT-FOUND))
)

(define-read-only (get-user-reputation (user principal))
    (default-to 
        {rating: u0, jobs-completed: u0}
        (map-get? UserReputations user)
    )
)

(define-private (update-reputation (user principal))
    (let ((current-rep (get-user-reputation user)))
        (map-set UserReputations 
            user 
            {
                rating: (+ (get rating current-rep) u1),
                jobs-completed: (+ (get jobs-completed current-rep) u1)
            }
        )
    )
)
