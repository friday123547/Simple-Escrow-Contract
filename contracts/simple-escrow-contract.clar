(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ESCROW_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_APPROVED (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_ESCROW_ALREADY_RELEASED (err u104))
(define-constant ERR_ESCROW_ALREADY_REFUNDED (err u105))
(define-constant ERR_CANNOT_APPROVE_OWN_ESCROW (err u106))
(define-constant ERR_INVALID_AMOUNT (err u107))
(define-constant ERR_INVALID_THRESHOLD (err u108))
(define-constant ERR_APPROVER_NOT_FOUND (err u109))
(define-constant ERR_THRESHOLD_NOT_MET (err u110))
(define-constant ERR_DUPLICATE_APPROVER (err u111))
(define-constant ERR_MILESTONE_NOT_FOUND (err u112))
(define-constant ERR_MILESTONE_COMPLETED (err u113))
(define-constant ERR_INVALID_MILESTONE_COUNT (err u114))
(define-constant ERR_ALL_MILESTONES_COMPLETED (err u115))
(define-constant ERR_MILESTONE_NOT_COMPLETED (err u116))
(define-constant ERR_RECURRING_PAUSED (err u117))
(define-constant ERR_RECURRING_COMPLETED (err u118))
(define-constant ERR_PAYMENT_NOT_DUE (err u119))
(define-constant ERR_INVALID_INTERVAL (err u120))
(define-constant ERR_ALREADY_PAUSED (err u121))
(define-constant ERR_NOT_PAUSED (err u122))

(define-data-var escrow-counter uint u0)
(define-data-var multisig-counter uint u0)
(define-data-var milestone-counter uint u0)
(define-data-var recurring-counter uint u0)

(define-map escrows
  uint
  {
    buyer: principal,
    seller: principal,
    amount: uint,
    buyer-approved: bool,
    seller-approved: bool,
    released: bool,
    refunded: bool,
    created-at: uint
  }
)

(define-map user-escrows
  principal
  (list 100 uint)
)

(define-map multisig-escrows
  uint
  {
    creator: principal,
    recipient: principal,
    amount: uint,
    required-approvals: uint,
    current-approvals: uint,
    approvers: (list 10 principal),
    approved-by: (list 10 principal),
    released: bool,
    refunded: bool,
    created-at: uint
  }
)

(define-map multisig-user-escrows
  principal
  (list 50 uint)
)

(define-map milestone-escrows
  uint
  {
    payer: principal,
    recipient: principal,
    total-amount: uint,
    milestone-count: uint,
    completed-milestones: uint,
    milestone-amounts: (list 10 uint),
    milestone-descriptions: (list 10 (string-ascii 100)),
    milestone-completed: (list 10 bool),
    milestone-approved: (list 10 bool),
    released: bool,
    refunded: bool,
    created-at: uint
  }
)

(define-map milestone-user-escrows
  principal
  (list 50 uint)
)

(define-map recurring-escrows
  uint
  {
    payer: principal,
    recipient: principal,
    amount-per-period: uint,
    interval-blocks: uint,
    total-periods: uint,
    periods-paid: uint,
    last-payment-block: uint,
    next-payment-block: uint,
    paused: bool,
    cancelled: bool,
    created-at: uint
  }
)

(define-map recurring-user-escrows
  principal
  (list 50 uint)
)

(define-public (create-escrow (seller principal) (amount uint))
  (let
    (
      (escrow-id (+ (var-get escrow-counter) u1))
      (buyer tx-sender)
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (is-eq buyer seller)) ERR_NOT_AUTHORIZED)
    (try! (stx-transfer? amount buyer (as-contract tx-sender)))
    (map-set escrows escrow-id
      {
        buyer: buyer,
        seller: seller,
        amount: amount,
        buyer-approved: false,
        seller-approved: false,
        released: false,
        refunded: false,
        created-at: stacks-block-height
      }
    )
    (update-user-escrows buyer escrow-id)
    (update-user-escrows seller escrow-id)
    (var-set escrow-counter escrow-id)
    (ok escrow-id)
  )
)

(define-public (create-multisig-escrow (recipient principal) (amount uint) (approvers (list 10 principal)) (threshold uint))
  (let
    (
      (multisig-id (+ (var-get multisig-counter) u1))
      (approver-count (len approvers))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (is-eq tx-sender recipient)) ERR_NOT_AUTHORIZED)
    (asserts! (and (> threshold u0) (<= threshold approver-count)) ERR_INVALID_THRESHOLD)
    (asserts! (> approver-count u0) ERR_INVALID_THRESHOLD)
    (asserts! (is-unique-list approvers) ERR_DUPLICATE_APPROVER)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set multisig-escrows multisig-id
      {
        creator: tx-sender,
        recipient: recipient,
        amount: amount,
        required-approvals: threshold,
        current-approvals: u0,
        approvers: approvers,
        approved-by: (list),
        released: false,
        refunded: false,
        created-at: stacks-block-height
      }
    )
    (update-multisig-user-escrows tx-sender multisig-id)
    (update-multisig-user-escrows recipient multisig-id)
    (fold update-approver-entry approvers multisig-id)
    (var-set multisig-counter multisig-id)
    (ok multisig-id)
  )
)

(define-public (create-milestone-escrow 
  (recipient principal) 
  (amounts (list 10 uint)) 
  (descriptions (list 10 (string-ascii 100))))
  (let
    (
      (milestone-id (+ (var-get milestone-counter) u1))
      (total-amount (fold + amounts u0))
      (milestone-count (len amounts))
    )
    (asserts! (> total-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (is-eq tx-sender recipient)) ERR_NOT_AUTHORIZED)
    (asserts! (and (> milestone-count u0) (<= milestone-count u10)) ERR_INVALID_MILESTONE_COUNT)
    (asserts! (is-eq milestone-count (len descriptions)) ERR_INVALID_MILESTONE_COUNT)
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    (map-set milestone-escrows milestone-id
      {
        payer: tx-sender,
        recipient: recipient,
        total-amount: total-amount,
        milestone-count: milestone-count,
        completed-milestones: u0,
        milestone-amounts: amounts,
        milestone-descriptions: descriptions,
        milestone-completed: (create-false-list milestone-count),
        milestone-approved: (create-false-list milestone-count),
        released: false,
        refunded: false,
        created-at: stacks-block-height
      }
    )
    (update-milestone-user-escrows tx-sender milestone-id)
    (update-milestone-user-escrows recipient milestone-id)
    (var-set milestone-counter milestone-id)
    (ok milestone-id)
  )
)

(define-public (complete-milestone (milestone-id uint) (milestone-index uint))
  (let
    (
      (escrow (unwrap! (map-get? milestone-escrows milestone-id) ERR_ESCROW_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get recipient escrow)) ERR_NOT_AUTHORIZED)
    (asserts! (< milestone-index (get milestone-count escrow)) ERR_MILESTONE_NOT_FOUND)
    (asserts! (not (get released escrow)) ERR_ESCROW_ALREADY_RELEASED)
    (asserts! (not (get refunded escrow)) ERR_ESCROW_ALREADY_REFUNDED)
    (asserts! (not (unwrap-panic (element-at (get milestone-completed escrow) milestone-index))) ERR_MILESTONE_COMPLETED)
    (let
      (
        (new-completed-count (+ (get completed-milestones escrow) u1))
      )
      (map-set milestone-escrows milestone-id
        (merge escrow 
          {
            completed-milestones: new-completed-count
          }
        )
      )
      (ok true)
    )
  )
)

(define-public (approve-milestone (milestone-id uint) (milestone-index uint))
  (let
    (
      (escrow (unwrap! (map-get? milestone-escrows milestone-id) ERR_ESCROW_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get payer escrow)) ERR_NOT_AUTHORIZED)
    (asserts! (< milestone-index (get milestone-count escrow)) ERR_MILESTONE_NOT_FOUND)
    (asserts! (not (get released escrow)) ERR_ESCROW_ALREADY_RELEASED)
    (asserts! (not (get refunded escrow)) ERR_ESCROW_ALREADY_REFUNDED)
    (asserts! (unwrap-panic (element-at (get milestone-completed escrow) milestone-index)) ERR_MILESTONE_NOT_COMPLETED)
    (asserts! (not (unwrap-panic (element-at (get milestone-approved escrow) milestone-index))) ERR_ALREADY_APPROVED)
    (let
      (
        (milestone-amount (unwrap-panic (element-at (get milestone-amounts escrow) milestone-index)))
      )
      (try! (as-contract (stx-transfer? milestone-amount tx-sender (get recipient escrow))))
      (ok milestone-amount)
    )
  )
)

(define-public (release-all-milestones (milestone-id uint))
  (let
    (
      (escrow (unwrap! (map-get? milestone-escrows milestone-id) ERR_ESCROW_NOT_FOUND))
      (remaining-amount (calculate-remaining-amount escrow))
    )
    (asserts! (is-eq tx-sender (get payer escrow)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get released escrow)) ERR_ESCROW_ALREADY_RELEASED)
    (asserts! (not (get refunded escrow)) ERR_ESCROW_ALREADY_REFUNDED)
    (asserts! (> remaining-amount u0) ERR_INVALID_AMOUNT)
    (try! (as-contract (stx-transfer? remaining-amount tx-sender (get recipient escrow))))
    (map-set milestone-escrows milestone-id (merge escrow { released: true }))
    (ok remaining-amount)
  )
)

(define-public (refund-milestone-escrow (milestone-id uint))
  (let
    (
      (escrow (unwrap! (map-get? milestone-escrows milestone-id) ERR_ESCROW_NOT_FOUND))
      (remaining-amount (calculate-remaining-amount escrow))
    )
    (asserts! (is-eq tx-sender (get payer escrow)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get released escrow)) ERR_ESCROW_ALREADY_RELEASED)
    (asserts! (not (get refunded escrow)) ERR_ESCROW_ALREADY_REFUNDED)
    (asserts! (> remaining-amount u0) ERR_INVALID_AMOUNT)
    (try! (as-contract (stx-transfer? remaining-amount tx-sender (get payer escrow))))
    (map-set milestone-escrows milestone-id (merge escrow { refunded: true }))
    (ok remaining-amount)
  )
)

(define-public (create-recurring-escrow 
  (recipient principal) 
  (amount-per-period uint) 
  (interval-blocks uint) 
  (total-periods uint))
  (let
    (
      (recurring-id (+ (var-get recurring-counter) u1))
      (total-amount (* amount-per-period total-periods))
      (first-payment-block (+ stacks-block-height interval-blocks))
    )
    (asserts! (> amount-per-period u0) ERR_INVALID_AMOUNT)
    (asserts! (> interval-blocks u0) ERR_INVALID_INTERVAL)
    (asserts! (> total-periods u0) ERR_INVALID_MILESTONE_COUNT)
    (asserts! (not (is-eq tx-sender recipient)) ERR_NOT_AUTHORIZED)
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    (map-set recurring-escrows recurring-id
      {
        payer: tx-sender,
        recipient: recipient,
        amount-per-period: amount-per-period,
        interval-blocks: interval-blocks,
        total-periods: total-periods,
        periods-paid: u0,
        last-payment-block: u0,
        next-payment-block: first-payment-block,
        paused: false,
        cancelled: false,
        created-at: stacks-block-height
      }
    )
    (update-recurring-user-escrows tx-sender recurring-id)
    (update-recurring-user-escrows recipient recurring-id)
    (var-set recurring-counter recurring-id)
    (ok recurring-id)
  )
)

(define-public (claim-recurring-payment (recurring-id uint))
  (let
    (
      (escrow (unwrap! (map-get? recurring-escrows recurring-id) ERR_ESCROW_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get recipient escrow)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get paused escrow)) ERR_RECURRING_PAUSED)
    (asserts! (not (get cancelled escrow)) ERR_ESCROW_ALREADY_REFUNDED)
    (asserts! (< (get periods-paid escrow) (get total-periods escrow)) ERR_RECURRING_COMPLETED)
    (asserts! (>= stacks-block-height (get next-payment-block escrow)) ERR_PAYMENT_NOT_DUE)
    (let
      (
        (new-periods-paid (+ (get periods-paid escrow) u1))
        (new-next-payment (+ (get next-payment-block escrow) (get interval-blocks escrow)))
      )
      (try! (as-contract (stx-transfer? (get amount-per-period escrow) tx-sender (get recipient escrow))))
      (map-set recurring-escrows recurring-id
        (merge escrow
          {
            periods-paid: new-periods-paid,
            last-payment-block: stacks-block-height,
            next-payment-block: new-next-payment
          }
        )
      )
      (ok (get amount-per-period escrow))
    )
  )
)

(define-public (pause-recurring-escrow (recurring-id uint))
  (let
    (
      (escrow (unwrap! (map-get? recurring-escrows recurring-id) ERR_ESCROW_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get payer escrow)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get paused escrow)) ERR_ALREADY_PAUSED)
    (asserts! (not (get cancelled escrow)) ERR_ESCROW_ALREADY_REFUNDED)
    (map-set recurring-escrows recurring-id (merge escrow { paused: true }))
    (ok true)
  )
)

(define-public (resume-recurring-escrow (recurring-id uint))
  (let
    (
      (escrow (unwrap! (map-get? recurring-escrows recurring-id) ERR_ESCROW_NOT_FOUND))
      (blocks-paused (- stacks-block-height (get last-payment-block escrow)))
    )
    (asserts! (is-eq tx-sender (get payer escrow)) ERR_NOT_AUTHORIZED)
    (asserts! (get paused escrow) ERR_NOT_PAUSED)
    (asserts! (not (get cancelled escrow)) ERR_ESCROW_ALREADY_REFUNDED)
    (let
      (
        (adjusted-next-payment (+ (get next-payment-block escrow) blocks-paused))
      )
      (map-set recurring-escrows recurring-id
        (merge escrow
          {
            paused: false,
            next-payment-block: adjusted-next-payment
          }
        )
      )
      (ok true)
    )
  )
)

(define-public (cancel-recurring-escrow (recurring-id uint))
  (let
    (
      (escrow (unwrap! (map-get? recurring-escrows recurring-id) ERR_ESCROW_NOT_FOUND))
      (remaining-periods (- (get total-periods escrow) (get periods-paid escrow)))
      (refund-amount (* remaining-periods (get amount-per-period escrow)))
    )
    (asserts! (or (is-eq tx-sender (get payer escrow)) (is-eq tx-sender (get recipient escrow))) ERR_NOT_AUTHORIZED)
    (asserts! (not (get cancelled escrow)) ERR_ESCROW_ALREADY_REFUNDED)
    (asserts! (> refund-amount u0) ERR_INVALID_AMOUNT)
    (try! (as-contract (stx-transfer? refund-amount tx-sender (get payer escrow))))
    (map-set recurring-escrows recurring-id (merge escrow { cancelled: true }))
    (ok refund-amount)
  )
)

(define-public (approve-multisig-escrow (multisig-id uint))
  (let
    (
      (escrow (unwrap! (map-get? multisig-escrows multisig-id) ERR_ESCROW_NOT_FOUND))
      (approver tx-sender)
    )
    (asserts! (not (get released escrow)) ERR_ESCROW_ALREADY_RELEASED)
    (asserts! (not (get refunded escrow)) ERR_ESCROW_ALREADY_REFUNDED)
    (asserts! (is-approver approver (get approvers escrow)) ERR_NOT_AUTHORIZED)
    (asserts! (not (is-already-approved approver (get approved-by escrow))) ERR_ALREADY_APPROVED)
    (let
      (
        (new-approved-list (unwrap-panic (as-max-len? (append (get approved-by escrow) approver) u10)))
        (new-approval-count (+ (get current-approvals escrow) u1))
      )
      (map-set multisig-escrows multisig-id
        (merge escrow 
          { 
            approved-by: new-approved-list,
            current-approvals: new-approval-count
          }
        )
      )
      (ok true)
    )
  )
)

(define-public (release-multisig-funds (multisig-id uint))
  (let
    (
      (escrow (unwrap! (map-get? multisig-escrows multisig-id) ERR_ESCROW_NOT_FOUND))
    )
    (asserts! (not (get released escrow)) ERR_ESCROW_ALREADY_RELEASED)
    (asserts! (not (get refunded escrow)) ERR_ESCROW_ALREADY_REFUNDED)
    (asserts! (>= (get current-approvals escrow) (get required-approvals escrow)) ERR_THRESHOLD_NOT_MET)
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get recipient escrow))))
    (map-set multisig-escrows multisig-id (merge escrow { released: true }))
    (ok true)
  )
)

(define-public (refund-multisig-escrow (multisig-id uint))
  (let
    (
      (escrow (unwrap! (map-get? multisig-escrows multisig-id) ERR_ESCROW_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator escrow)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get released escrow)) ERR_ESCROW_ALREADY_RELEASED)
    (asserts! (not (get refunded escrow)) ERR_ESCROW_ALREADY_REFUNDED)
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get creator escrow))))
    (map-set multisig-escrows multisig-id (merge escrow { refunded: true }))
    (ok true)
  )
)

(define-public (approve-escrow (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows escrow-id) ERR_ESCROW_NOT_FOUND))
      (caller tx-sender)
    )
    (asserts! (not (get released escrow)) ERR_ESCROW_ALREADY_RELEASED)
    (asserts! (not (get refunded escrow)) ERR_ESCROW_ALREADY_REFUNDED)
    (asserts! 
      (or 
        (is-eq caller (get buyer escrow))
        (is-eq caller (get seller escrow))
      ) 
      ERR_NOT_AUTHORIZED
    )
    (if (is-eq caller (get buyer escrow))
      (begin
        (asserts! (not (get buyer-approved escrow)) ERR_ALREADY_APPROVED)
        (map-set escrows escrow-id (merge escrow { buyer-approved: true }))
        (ok true)
      )
      (begin
        (asserts! (not (get seller-approved escrow)) ERR_ALREADY_APPROVED)
        (map-set escrows escrow-id (merge escrow { seller-approved: true }))
        (ok true)
      )
    )
  )
)

(define-public (release-funds (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows escrow-id) ERR_ESCROW_NOT_FOUND))
    )
    (asserts! (get buyer-approved escrow) ERR_NOT_AUTHORIZED)
    (asserts! (get seller-approved escrow) ERR_NOT_AUTHORIZED)
    (asserts! (not (get released escrow)) ERR_ESCROW_ALREADY_RELEASED)
    (asserts! (not (get refunded escrow)) ERR_ESCROW_ALREADY_REFUNDED)
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get seller escrow))))
    (map-set escrows escrow-id (merge escrow { released: true }))
    (ok true)
  )
)

(define-public (refund-escrow (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows escrow-id) ERR_ESCROW_NOT_FOUND))
      (caller tx-sender)
    )
    (asserts! (not (get released escrow)) ERR_ESCROW_ALREADY_RELEASED)
    (asserts! (not (get refunded escrow)) ERR_ESCROW_ALREADY_REFUNDED)
    (asserts! 
      (or 
        (is-eq caller (get buyer escrow))
        (is-eq caller (get seller escrow))
      ) 
      ERR_NOT_AUTHORIZED
    )
    (asserts! (not (get buyer-approved escrow)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get seller-approved escrow)) ERR_NOT_AUTHORIZED)
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get buyer escrow))))
    (map-set escrows escrow-id (merge escrow { refunded: true }))
    (ok true)
  )
)

(define-public (top-up-escrow (escrow-id uint) (additional-amount uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows escrow-id) ERR_ESCROW_NOT_FOUND))
    )
    (asserts! (> additional-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (is-eq tx-sender (get buyer escrow)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get released escrow)) ERR_ESCROW_ALREADY_RELEASED)
    (asserts! (not (get refunded escrow)) ERR_ESCROW_ALREADY_REFUNDED)
    (try! (stx-transfer? additional-amount tx-sender (as-contract tx-sender)))
    (map-set escrows escrow-id (merge escrow { amount: (+ (get amount escrow) additional-amount) }))
    (ok (+ (get amount escrow) additional-amount))
  )
)

(define-public (emergency-refund (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows escrow-id) ERR_ESCROW_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (not (get released escrow)) ERR_ESCROW_ALREADY_RELEASED)
    (asserts! (not (get refunded escrow)) ERR_ESCROW_ALREADY_REFUNDED)
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get buyer escrow))))
    (map-set escrows escrow-id (merge escrow { refunded: true }))
    (ok true)
  )
)

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows escrow-id)
)

(define-read-only (get-multisig-escrow (multisig-id uint))
  (map-get? multisig-escrows multisig-id)
)

(define-read-only (get-milestone-escrow (milestone-id uint))
  (map-get? milestone-escrows milestone-id)
)

(define-read-only (get-recurring-escrow (recurring-id uint))
  (map-get? recurring-escrows recurring-id)
)

(define-read-only (get-multisig-status (multisig-id uint))
  (let
    (
      (escrow (unwrap! (map-get? multisig-escrows multisig-id) ERR_ESCROW_NOT_FOUND))
    )
    (ok {
      current-approvals: (get current-approvals escrow),
      required-approvals: (get required-approvals escrow),
      approved-by: (get approved-by escrow),
      ready-for-release: (>= (get current-approvals escrow) (get required-approvals escrow)),
      released: (get released escrow),
      refunded: (get refunded escrow)
    })
  )
)

(define-read-only (get-escrow-status (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows escrow-id) ERR_ESCROW_NOT_FOUND))
    )
    (ok {
      buyer-approved: (get buyer-approved escrow),
      seller-approved: (get seller-approved escrow),
      released: (get released escrow),
      refunded: (get refunded escrow),
      ready-for-release: (and (get buyer-approved escrow) (get seller-approved escrow))
    })
  )
)

(define-read-only (get-milestone-status (milestone-id uint))
  (let
    (
      (escrow (unwrap! (map-get? milestone-escrows milestone-id) ERR_ESCROW_NOT_FOUND))
    )
    (ok {
      milestone-count: (get milestone-count escrow),
      completed-milestones: (get completed-milestones escrow),

      total-amount: (get total-amount escrow),
      remaining-amount: (calculate-remaining-amount escrow),
      released: (get released escrow),
      refunded: (get refunded escrow),
      progress-percentage: (/ (* (get completed-milestones escrow) u100) (get milestone-count escrow))
    })
  )
)

(define-read-only (get-recurring-status (recurring-id uint))
  (let
    (
      (escrow (unwrap! (map-get? recurring-escrows recurring-id) ERR_ESCROW_NOT_FOUND))
    )
    (ok {
      periods-paid: (get periods-paid escrow),
      total-periods: (get total-periods escrow),
      next-payment-block: (get next-payment-block escrow),
      blocks-until-next: (if (> (get next-payment-block escrow) stacks-block-height)
                           (- (get next-payment-block escrow) stacks-block-height)
                           u0),
      payment-ready: (>= stacks-block-height (get next-payment-block escrow)),
      paused: (get paused escrow),
      cancelled: (get cancelled escrow),
      completed: (>= (get periods-paid escrow) (get total-periods escrow)),
      remaining-amount: (* (- (get total-periods escrow) (get periods-paid escrow)) (get amount-per-period escrow)),
      progress-percentage: (/ (* (get periods-paid escrow) u100) (get total-periods escrow))
    })
  )
)

(define-read-only (get-user-escrows (user principal))
  (default-to (list) (map-get? user-escrows user))
)

(define-read-only (get-user-multisig-escrows (user principal))
  (default-to (list) (map-get? multisig-user-escrows user))
)

(define-read-only (get-user-milestone-escrows (user principal))
  (default-to (list) (map-get? milestone-user-escrows user))
)

(define-read-only (get-user-recurring-escrows (user principal))
  (default-to (list) (map-get? recurring-user-escrows user))
)

(define-read-only (get-escrow-count)
  (var-get escrow-counter)
)

(define-read-only (get-multisig-count)
  (var-get multisig-counter)
)

(define-read-only (get-milestone-count)
  (var-get milestone-counter)
)

(define-read-only (get-recurring-count)
  (var-get recurring-counter)
)

(define-read-only (is-party-to-escrow (escrow-id uint) (user principal))
  (match (map-get? escrows escrow-id)
    escrow (or 
             (is-eq user (get buyer escrow))
             (is-eq user (get seller escrow))
           )
    false
  )
)

(define-read-only (is-party-to-multisig (multisig-id uint) (user principal))
  (match (map-get? multisig-escrows multisig-id)
    escrow (or 
             (is-eq user (get creator escrow))
             (is-eq user (get recipient escrow))
             (is-approver user (get approvers escrow))
           )
    false
  )
)

(define-read-only (is-party-to-milestone (milestone-id uint) (user principal))
  (match (map-get? milestone-escrows milestone-id)
    escrow (or 
             (is-eq user (get payer escrow))
             (is-eq user (get recipient escrow))
           )
    false
  )
)

(define-read-only (is-party-to-recurring (recurring-id uint) (user principal))
  (match (map-get? recurring-escrows recurring-id)
    escrow (or 
             (is-eq user (get payer escrow))
             (is-eq user (get recipient escrow))
           )
    false
  )
)

(define-read-only (can-release-funds (escrow-id uint))
  (match (map-get? escrows escrow-id)
    escrow (and 
             (get buyer-approved escrow)
             (get seller-approved escrow)
             (not (get released escrow))
             (not (get refunded escrow))
           )
    false
  )
)

(define-read-only (can-release-multisig (multisig-id uint))
  (match (map-get? multisig-escrows multisig-id)
    escrow (and 
             (>= (get current-approvals escrow) (get required-approvals escrow))
             (not (get released escrow))
             (not (get refunded escrow))
           )
    false
  )
)

(define-read-only (can-refund (escrow-id uint))
  (match (map-get? escrows escrow-id)
    escrow (and 
             (not (get buyer-approved escrow))
             (not (get seller-approved escrow))
             (not (get released escrow))
             (not (get refunded escrow))
           )
    false
  )
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

(define-private (update-user-escrows (user principal) (escrow-id uint))
  (let
    (
      (current-escrows (default-to (list) (map-get? user-escrows user)))
    )
    (map-set user-escrows user (unwrap-panic (as-max-len? (append current-escrows escrow-id) u100)))
  )
)

(define-private (update-multisig-user-escrows (user principal) (multisig-id uint))
  (let
    (
      (current-escrows (default-to (list) (map-get? multisig-user-escrows user)))
    )
    (map-set multisig-user-escrows user (unwrap-panic (as-max-len? (append current-escrows multisig-id) u50)))
  )
)

(define-private (update-approver-entry (approver principal) (multisig-id uint))
  (begin
    (update-multisig-user-escrows approver multisig-id)
    multisig-id)
)

(define-private (update-milestone-user-escrows (user principal) (milestone-id uint))
  (let
    (
      (current-escrows (default-to (list) (map-get? milestone-user-escrows user)))
    )
    (map-set milestone-user-escrows user (unwrap-panic (as-max-len? (append current-escrows milestone-id) u50)))
  )
)

(define-private (update-recurring-user-escrows (user principal) (recurring-id uint))
  (let
    (
      (current-escrows (default-to (list) (map-get? recurring-user-escrows user)))
    )
    (map-set recurring-user-escrows user (unwrap-panic (as-max-len? (append current-escrows recurring-id) u50)))
  )
)

(define-private (create-false-list (count uint))
  (if (is-eq count u0)
    (list)
    (if (is-eq count u1)
      (list false)
      (if (is-eq count u2)
        (list false false)
        (if (is-eq count u3)
          (list false false false)
          (if (is-eq count u4)
            (list false false false false)
            (if (is-eq count u5)
              (list false false false false false)
              (if (is-eq count u6)
                (list false false false false false false)
                (if (is-eq count u7)
                  (list false false false false false false false)
                  (if (is-eq count u8)
                    (list false false false false false false false false)
                    (if (is-eq count u9)
                      (list false false false false false false false false false)
                      (list false false false false false false false false false false)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)



(define-private (calculate-remaining-amount (escrow {payer: principal, recipient: principal, total-amount: uint, milestone-count: uint, completed-milestones: uint, milestone-amounts: (list 10 uint), milestone-descriptions: (list 10 (string-ascii 100)), milestone-completed: (list 10 bool), milestone-approved: (list 10 bool), released: bool, refunded: bool, created-at: uint}))
  (- (get total-amount escrow) (calculate-approved-amount-simple escrow))
)

(define-private (calculate-approved-amount-simple (escrow {payer: principal, recipient: principal, total-amount: uint, milestone-count: uint, completed-milestones: uint, milestone-amounts: (list 10 uint), milestone-descriptions: (list 10 (string-ascii 100)), milestone-completed: (list 10 bool), milestone-approved: (list 10 bool), released: bool, refunded: bool, created-at: uint}))
  (let
    (
      (amounts (get milestone-amounts escrow))
      (approved (get milestone-approved escrow))
    )
    (+ 
      (if (and (> (len approved) u0) (unwrap-panic (element-at approved u0))) (unwrap-panic (element-at amounts u0)) u0)
      (+ 
        (if (and (> (len approved) u1) (unwrap-panic (element-at approved u1))) (unwrap-panic (element-at amounts u1)) u0)
        (+ 
          (if (and (> (len approved) u2) (unwrap-panic (element-at approved u2))) (unwrap-panic (element-at amounts u2)) u0)
          (+ 
            (if (and (> (len approved) u3) (unwrap-panic (element-at approved u3))) (unwrap-panic (element-at amounts u3)) u0)
            (+ 
              (if (and (> (len approved) u4) (unwrap-panic (element-at approved u4))) (unwrap-panic (element-at amounts u4)) u0)
              (+ 
                (if (and (> (len approved) u5) (unwrap-panic (element-at approved u5))) (unwrap-panic (element-at amounts u5)) u0)
                (+ 
                  (if (and (> (len approved) u6) (unwrap-panic (element-at approved u6))) (unwrap-panic (element-at amounts u6)) u0)
                  (+ 
                    (if (and (> (len approved) u7) (unwrap-panic (element-at approved u7))) (unwrap-panic (element-at amounts u7)) u0)
                    (+ 
                      (if (and (> (len approved) u8) (unwrap-panic (element-at approved u8))) (unwrap-panic (element-at amounts u8)) u0)
                      (if (and (> (len approved) u9) (unwrap-panic (element-at approved u9))) (unwrap-panic (element-at amounts u9)) u0)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

(define-private (is-approver (user principal) (approvers (list 10 principal)))
  (is-some (index-of approvers user))
)

(define-private (is-already-approved (user principal) (approved-list (list 10 principal)))
  (is-some (index-of approved-list user))
)

(define-private (is-unique-list (items (list 10 principal)))
  (is-eq (len items) (len (remove-duplicates items)))
)

(define-private (remove-duplicates (items (list 10 principal)))
  (fold check-and-add items (list))
)

(define-private (check-and-add (item principal) (acc (list 10 principal)))
  (if (is-some (index-of acc item))
    acc
    (unwrap-panic (as-max-len? (append acc item) u10))
  )
)
