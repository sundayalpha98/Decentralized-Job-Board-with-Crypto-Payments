(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-JOB-NOT-FOUND (err u101))
(define-constant ERR-INVALID-STATUS (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-ALREADY-APPLIED (err u104))
(define-constant ERR-DISPUTE-NOT-FOUND (err u105))
(define-constant ERR-ALREADY-VOTED (err u106))
(define-constant ERR-DISPUTE-EXPIRED (err u107))
(define-constant ERR-CATEGORY-NOT-FOUND (err u108))
(define-constant ERR-SKILL-NOT-FOUND (err u109))
(define-constant ERR-MILESTONE-NOT-FOUND (err u110))
(define-constant ERR-MILESTONE-ALREADY-COMPLETED (err u111))
(define-constant ERR-INVALID-MILESTONE-STATUS (err u112))

(define-data-var job-count uint u0)
(define-data-var dispute-count uint u0)
(define-data-var category-count uint u0)
(define-data-var skill-count uint u0)
(define-data-var milestone-count uint u0)

(define-map Jobs 
    uint 
    {
        employer: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        payment: uint,
        status: (string-ascii 20),
        worker: (optional principal),
        created-at: uint,
        category-id: uint,
        required-skills: (list 5 uint)
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

(define-map Disputes
    uint
    {
        job-id: uint,
        raised-by: principal,
        reason: (string-ascii 200),
        status: (string-ascii 20),
        votes-for-employer: uint,
        votes-for-worker: uint,
        created-at: uint,
        expires-at: uint
    }
)

(define-map DisputeVotes
    {dispute-id: uint, voter: principal}
    {vote: (string-ascii 10)}
)

(define-map Categories
    uint
    {name: (string-ascii 50), description: (string-ascii 200)}
)

(define-map Skills
    uint
    {name: (string-ascii 50), category-id: uint}
)

(define-map UserSkills
    {user: principal, skill-id: uint}
    {proficiency: uint}
)

(define-map JobsByCategory
    uint
    (list 100 uint)
)

(define-map Milestones
    uint
    {
        job-id: uint,
        title: (string-ascii 100),
        description: (string-ascii 300),
        amount: uint,
        status: (string-ascii 20),
        created-at: uint,
        completed-at: (optional uint)
    }
)

(define-map JobMilestones
    uint
    (list 10 uint)
)

(define-public (create-category (name (string-ascii 50)) (description (string-ascii 200)))
    (let ((category-id (var-get category-count)))
        (map-set Categories category-id {name: name, description: description})
        (map-set JobsByCategory category-id (list))
        (var-set category-count (+ category-id u1))
        (ok category-id)
    )
)

(define-public (create-skill (name (string-ascii 50)) (category-id uint))
    (let ((skill-id (var-get skill-count)))
        (asserts! (is-some (map-get? Categories category-id)) ERR-CATEGORY-NOT-FOUND)
        (map-set Skills skill-id {name: name, category-id: category-id})
        (var-set skill-count (+ skill-id u1))
        (ok skill-id)
    )
)

(define-public (add-user-skill (skill-id uint) (proficiency uint))
    (begin
        (asserts! (is-some (map-get? Skills skill-id)) ERR-SKILL-NOT-FOUND)
        (asserts! (<= proficiency u5) ERR-INVALID-STATUS)
        (map-set UserSkills {user: tx-sender, skill-id: skill-id} {proficiency: proficiency})
        (ok true)
    )
)

(define-public (create-job (title (string-ascii 100)) (description (string-ascii 500)) (payment uint) (category-id uint) (required-skills (list 5 uint)))
    (let ((job-id (var-get job-count)))
        (asserts! (is-some (map-get? Categories category-id)) ERR-CATEGORY-NOT-FOUND)
        (try! (stx-transfer? payment tx-sender (as-contract tx-sender)))
        (map-set Jobs job-id
            {
                employer: tx-sender,
                title: title,
                description: description,
                payment: payment,
                status: "open",
                worker: none,
                created-at: burn-block-height,
                category-id: category-id,
                required-skills: required-skills
            }
        )
        (let ((current-jobs (default-to (list) (map-get? JobsByCategory category-id))))
            (map-set JobsByCategory category-id (unwrap-panic (as-max-len? (append current-jobs job-id) u100)))
        )
        (map-set JobMilestones job-id (list))
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
        (try! (as-contract (stx-transfer? (get payment job) tx-sender (unwrap-panic (get worker job)))))
        (update-reputation tx-sender)
        (ok true)
    )
)

(define-public (raise-dispute (job-id uint) (reason (string-ascii 200)))
    (let 
        (
            (job (unwrap! (map-get? Jobs job-id) ERR-JOB-NOT-FOUND))
            (dispute-id (var-get dispute-count))
        )
        (asserts! (or 
            (is-eq tx-sender (get employer job))
            (is-eq (some tx-sender) (get worker job))
        ) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status job) "in-progress") ERR-INVALID-STATUS)
        (map-set Jobs job-id (merge job {status: "disputed"}))
        (map-set Disputes dispute-id
            {
                job-id: job-id,
                raised-by: tx-sender,
                reason: reason,
                status: "active",
                votes-for-employer: u0,
                votes-for-worker: u0,
                created-at: burn-block-height,
                expires-at: (+ burn-block-height u144)
            }
        )
        (var-set dispute-count (+ dispute-id u1))
        (ok dispute-id)
    )
)

(define-public (vote-on-dispute (dispute-id uint) (vote-for (string-ascii 10)))
    (let ((dispute (unwrap! (map-get? Disputes dispute-id) ERR-DISPUTE-NOT-FOUND)))
        (asserts! (is-eq (get status dispute) "active") ERR-INVALID-STATUS)
        (asserts! (< burn-block-height (get expires-at dispute)) ERR-DISPUTE-EXPIRED)
        (asserts! (is-none (map-get? DisputeVotes {dispute-id: dispute-id, voter: tx-sender})) ERR-ALREADY-VOTED)
        (map-set DisputeVotes {dispute-id: dispute-id, voter: tx-sender} {vote: vote-for})
        (if (is-eq vote-for "employer")
            (map-set Disputes dispute-id (merge dispute {
                votes-for-employer: (+ (get votes-for-employer dispute) u1)
            }))
            (map-set Disputes dispute-id (merge dispute {
                votes-for-worker: (+ (get votes-for-worker dispute) u1)
            }))
        )
        (ok true)
    )
)

(define-public (resolve-dispute (dispute-id uint))
    (let 
        (
            (dispute (unwrap! (map-get? Disputes dispute-id) ERR-DISPUTE-NOT-FOUND))
            (job (unwrap! (map-get? Jobs (get job-id dispute)) ERR-JOB-NOT-FOUND))
        )
        (asserts! (>= burn-block-height (get expires-at dispute)) ERR-INVALID-STATUS)
        (asserts! (is-eq (get status dispute) "active") ERR-INVALID-STATUS)
        (map-set Disputes dispute-id (merge dispute {status: "resolved"}))
        (if (> (get votes-for-worker dispute) (get votes-for-employer dispute))
            (begin
                (try! (as-contract (stx-transfer? (get payment job) tx-sender (unwrap-panic (get worker job)))))
                (map-set Jobs (get job-id dispute) (merge job {status: "completed"}))
                (update-reputation (unwrap-panic (get worker job)))
            )
            (begin
                (try! (as-contract (stx-transfer? (get payment job) tx-sender (get employer job))))
                (map-set Jobs (get job-id dispute) (merge job {status: "cancelled"}))
            )
        )
        (ok true)
    )
)

(define-public (create-milestone (job-id uint) (title (string-ascii 100)) (description (string-ascii 300)) (amount uint))
    (let 
        (
            (job (unwrap! (map-get? Jobs job-id) ERR-JOB-NOT-FOUND))
            (milestone-id (var-get milestone-count))
        )
        (asserts! (is-eq tx-sender (get employer job)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status job) "open") ERR-INVALID-STATUS)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set Milestones milestone-id
            {
                job-id: job-id,
                title: title,
                description: description,
                amount: amount,
                status: "pending",
                created-at: burn-block-height,
                completed-at: none
            }
        )
        (let ((current-milestones (default-to (list) (map-get? JobMilestones job-id))))
            (map-set JobMilestones job-id (unwrap-panic (as-max-len? (append current-milestones milestone-id) u10)))
        )
        (var-set milestone-count (+ milestone-id u1))
        (ok milestone-id)
    )
)

(define-public (complete-milestone (milestone-id uint))
    (let ((milestone (unwrap! (map-get? Milestones milestone-id) ERR-MILESTONE-NOT-FOUND)))
        (let ((job (unwrap! (map-get? Jobs (get job-id milestone)) ERR-JOB-NOT-FOUND)))
            (asserts! (is-eq (some tx-sender) (get worker job)) ERR-NOT-AUTHORIZED)
            (asserts! (is-eq (get status milestone) "pending") ERR-INVALID-MILESTONE-STATUS)
            (map-set Milestones milestone-id (merge milestone {
                status: "completed",
                completed-at: (some burn-block-height)
            }))
            (try! (as-contract (stx-transfer? (get amount milestone) tx-sender (unwrap-panic (get worker job)))))
            (ok true)
        )
    )
)

(define-public (approve-milestone (milestone-id uint))
    (let ((milestone (unwrap! (map-get? Milestones milestone-id) ERR-MILESTONE-NOT-FOUND)))
        (let ((job (unwrap! (map-get? Jobs (get job-id milestone)) ERR-JOB-NOT-FOUND)))
            (asserts! (is-eq tx-sender (get employer job)) ERR-NOT-AUTHORIZED)
            (asserts! (is-eq (get status milestone) "completed") ERR-INVALID-MILESTONE-STATUS)
            (map-set Milestones milestone-id (merge milestone {status: "approved"}))
            (ok true)
        )
    )
)

(define-read-only (get-job (job-id uint))
    (ok (unwrap! (map-get? Jobs job-id) ERR-JOB-NOT-FOUND))
)

(define-read-only (get-dispute (dispute-id uint))
    (ok (unwrap! (map-get? Disputes dispute-id) ERR-DISPUTE-NOT-FOUND))
)

(define-read-only (get-jobs-by-category (category-id uint))
    (ok (default-to (list) (map-get? JobsByCategory category-id)))
)

(define-read-only (get-category (category-id uint))
    (ok (unwrap! (map-get? Categories category-id) ERR-CATEGORY-NOT-FOUND))
)

(define-read-only (get-skill (skill-id uint))
    (ok (unwrap! (map-get? Skills skill-id) ERR-SKILL-NOT-FOUND))
)

(define-read-only (get-user-skill (user principal) (skill-id uint))
    (ok (map-get? UserSkills {user: user, skill-id: skill-id}))
)

(define-read-only (get-user-reputation (user principal))
    (default-to 
        {rating: u0, jobs-completed: u0}
        (map-get? UserReputations user)
    )
)

(define-read-only (get-milestone (milestone-id uint))
    (ok (unwrap! (map-get? Milestones milestone-id) ERR-MILESTONE-NOT-FOUND))
)

(define-read-only (get-job-milestones (job-id uint))
    (ok (default-to (list) (map-get? JobMilestones job-id)))
)

(define-read-only (check-skill-match (applicant principal) (required-skills (list 5 uint)))
    (ok (fold check-single-skill required-skills {user: applicant, matches: u0}))
)

(define-private (check-single-skill (skill-id uint) (acc {user: principal, matches: uint}))
    (let ((user-skill (map-get? UserSkills {user: (get user acc), skill-id: skill-id})))
        (if (and (is-some user-skill) (>= (get proficiency (unwrap-panic user-skill)) u3))
            {user: (get user acc), matches: (+ (get matches acc) u1)}
            acc
        )
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
