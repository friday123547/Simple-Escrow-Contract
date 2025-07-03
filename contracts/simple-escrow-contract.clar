(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ESCROW_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_APPROVED (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_ESCROW_ALREADY_RELEASED (err u104))
(define-constant ERR_ESCROW_ALREADY_REFUNDED (err u105))
(define-constant ERR_CANNOT_APPROVE_OWN_ESCROW (err u106))
(define-constant ERR_INVALID_AMOUNT (err u107))

(define-data-var escrow-counter uint u0)

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

(define-read-only (get-user-escrows (user principal))
  (default-to (list) (map-get? user-escrows user))
)

(define-read-only (get-escrow-count)
  (var-get escrow-counter)
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

(define-private (update-user-escrows (user principal) (escrow-id uint))
  (let
    (
      (current-escrows (default-to (list) (map-get? user-escrows user)))
    )
    (map-set user-escrows user (unwrap-panic (as-max-len? (append current-escrows escrow-id) u100)))
  )
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
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


(define-constant ERR_NOT_AUTHORIZED (err u300))
(define-constant ERR_ESCROW_NOT_FOUND (err u301))
(define-constant ERR_ESCROW_EXPIRED (err u302))
(define-constant ERR_ESCROW_NOT_EXPIRED (err u303))
(define-constant ERR_INVALID_DEADLINE (err u304))
(define-constant ERR_ALREADY_RELEASED (err u305))
(define-constant ERR_INVALID_AMOUNT (err u306))

(define-data-var escrow-counter uint u0)
(define-data-var min-lock-duration uint u144)

(define-map timelock-escrows
  uint
  {
    buyer: principal,
    seller: principal,
    amount: uint,
    deadline: uint,
    auto-release: bool,
    buyer-approved: bool,
    seller-approved: bool,
    released: bool,
    expired: bool,
    created-at: uint
  }
)

(define-map escrow-milestones
  uint
  {
    total-milestones: uint,
    completed-milestones: uint,
    milestone-amount: uint,
    next-deadline: uint
  }
)

(define-public (create-timelock-escrow (seller principal) (amount uint) (lock-duration uint) (auto-release bool))
  (let
    ((escrow-id (+ (var-get escrow-counter) u1))
     (deadline (+ stacks-block-height lock-duration)))
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (is-eq tx-sender seller)) ERR_NOT_AUTHORIZED)
    (asserts! (>= lock-duration (var-get min-lock-duration)) ERR_INVALID_DEADLINE)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set timelock-escrows escrow-id
      {
        buyer: tx-sender,
        seller: seller,
        amount: amount,
        deadline: deadline,
        auto-release: auto-release,
        buyer-approved: false,
        seller-approved: false,
        released: false,
        expired: false,
        created-at: stacks-block-height
      })
    (var-set escrow-counter escrow-id)
    (ok escrow-id)
  )
)

(define-public (create-milestone-escrow (seller principal) (total-amount uint) (milestone-count uint) (milestone-duration uint))
  (let
    ((escrow-id (+ (var-get escrow-counter) u1))
     (milestone-amount (/ total-amount milestone-count))
     (first-deadline (+ stacks-block-height milestone-duration)))
    (asserts! (> total-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> milestone-count u0) ERR_INVALID_DEADLINE)
    (asserts! (not (is-eq tx-sender seller)) ERR_NOT_AUTHORIZED)
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    (map-set timelock-escrows escrow-id
      {
        buyer: tx-sender,
        seller: seller,
        amount: total-amount,
        deadline: first-deadline,
        auto-release: false,
        buyer-approved: false,
        seller-approved: false,
        released: false,
        expired: false,
        created-at: stacks-block-height
      })
    (map-set escrow-milestones escrow-id
      {
        total-milestones: milestone-count,
        completed-milestones: u0,
        milestone-amount: milestone-amount,
        next-deadline: first-deadline
      })
    (var-set escrow-counter escrow-id)
    (ok escrow-id)
  )
)

(define-public (approve-timelock-escrow (escrow-id uint))
  (let
    ((escrow (unwrap! (map-get? timelock-escrows escrow-id) ERR_ESCROW_NOT_FOUND)))
    (asserts! (< stacks-block-height (get deadline escrow)) ERR_ESCROW_EXPIRED)
    (asserts! (not (get released escrow)) ERR_ALREADY_RELEASED)
    (asserts! (or (is-eq tx-sender (get buyer escrow)) (is-eq tx-sender (get seller escrow))) ERR_NOT_AUTHORIZED)
    (if (is-eq tx-sender (get buyer escrow))
      (map-set timelock-escrows escrow-id (merge escrow { buyer-approved: true }))
      (map-set timelock-escrows escrow-id (merge escrow { seller-approved: true })))
    (ok true)
  )
)

(define-public (release-timelock-funds (escrow-id uint))
  (let
    ((escrow (unwrap! (map-get? timelock-escrows escrow-id) ERR_ESCROW_NOT_FOUND)))
    (asserts! (not (get released escrow)) ERR_ALREADY_RELEASED)
    (asserts! 
      (or
        (and (get buyer-approved escrow) (get seller-approved escrow))
        (and (get auto-release escrow) (>= stacks-block-height (get deadline escrow))))
      ERR_NOT_AUTHORIZED)
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get seller escrow))))
    (map-set timelock-escrows escrow-id (merge escrow { released: true }))
    (ok true)
  )
)

(define-public (complete-milestone (escrow-id uint))
  (let
    ((escrow (unwrap! (map-get? timelock-escrows escrow-id) ERR_ESCROW_NOT_FOUND))
     (milestone (unwrap! (map-get? escrow-milestones escrow-id) ERR_ESCROW_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get buyer escrow)) ERR_NOT_AUTHORIZED)
    (asserts! (<= stacks-block-height (get next-deadline milestone)) ERR_ESCROW_EXPIRED)
    (asserts! (< (get completed-milestones milestone) (get total-milestones milestone)) ERR_NOT_AUTHORIZED)
    (let
      ((new-completed (+ (get completed-milestones milestone) u1))
       (is-final (is-eq new-completed (get total-milestones milestone))))
      (try! (as-contract (stx-transfer? (get milestone-amount milestone) tx-sender (get seller escrow))))
      (if is-final
        (map-set timelock-escrows escrow-id (merge escrow { released: true }))
        (map-set escrow-milestones escrow-id
          (merge milestone
            {
              completed-milestones: new-completed,
              next-deadline: (+ stacks-block-height u144)
            })))
      (ok true)
    )
  )
)

(define-public (expire-escrow (escrow-id uint))
  (let
    ((escrow (unwrap! (map-get? timelock-escrows escrow-id) ERR_ESCROW_NOT_FOUND)))
    (asserts! (>= stacks-block-height (get deadline escrow)) ERR_ESCROW_NOT_EXPIRED)
    (asserts! (not (get released escrow)) ERR_ALREADY_RELEASED)
    (asserts! (not (get auto-release escrow)) ERR_NOT_AUTHORIZED)
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get buyer escrow))))
    (map-set timelock-escrows escrow-id (merge escrow { expired: true }))
    (ok true)
  )
)

(define-read-only (get-timelock-escrow (escrow-id uint))
  (map-get? timelock-escrows escrow-id)
)

(define-read-only (get-milestone-info (escrow-id uint))
  (map-get? escrow-milestones escrow-id)
)

(define-read-only (is-escrow-expired (escrow-id uint))
  (match (map-get? timelock-escrows escrow-id)
    escrow (>= stacks-block-height (get deadline escrow))
    false)
)

(define-read-only (blocks-until-expiry (escrow-id uint))
  (match (map-get? timelock-escrows escrow-id)
    escrow (if (>= stacks-block-height (get deadline escrow))
             u0
             (- (get deadline escrow) stacks-block-height))
    u0)
)
