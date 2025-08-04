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

(define-data-var escrow-counter uint u0)
(define-data-var multisig-counter uint u0)

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

(define-read-only (get-user-escrows (user principal))
  (default-to (list) (map-get? user-escrows user))
)

(define-read-only (get-user-multisig-escrows (user principal))
  (default-to (list) (map-get? multisig-user-escrows user))
)

(define-read-only (get-escrow-count)
  (var-get escrow-counter)
)

(define-read-only (get-multisig-count)
  (var-get multisig-counter)
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
