;; An on-chain counter that stores a count for each individual

(define-constant MAX-COUNT u100)

(define-map counters principal uint)

(define-data-var total-ops uint u0)

(define-map operation-history principal (list 10 (tuple (op-type (string-ascii 10)) (value uint) (timestamp uint))))

(define-data-var reset-locks (list 10 principal) (tuple))

(define-read-only (get-count (who principal))
  (default-to u0 (map-get? counters who))
)

(define-read-only (get-total-operations)
  (var-get total-ops)
)

(define-private (update-total-ops)
  (var-set total-ops (+ (var-get total-ops) u1))
)

(define-private (record-operation (user principal) (op-type (string-ascii 10)) (value uint))
  (let ((history (default-to (list) (map-get? operation-history user))))
    (map-set operation-history user (cons (tuple (op-type op-type) (value value) (timestamp (block-height))) history))
  )
)

(define-public (count-up-step (step uint))
  (asserts! (and (>= step u1) (<= step u10)) (err u4))
  (let ((current-count (get-count tx-sender)))
    (asserts! (< (+ current-count step) MAX-COUNT) (err u1))
    (update-total-ops)
    (map-set counters tx-sender (+ current-count step))
    (record-operation tx-sender "increment" step)
    (ok (+ current-count step))
  )
)

(define-public (count-down-step (step uint))
  (asserts! (and (>= step u1) (<= step u10)) (err u4))
  (let ((current-count (get-count tx-sender)))
    (asserts! (>= current-count step) (err u2))
    (update-total-ops)
    (map-set counters tx-sender (- current-count step))
    (record-operation tx-sender "decrement" step)
    (ok (- current-count step))
  )
)

(define-public (reset-count-locked)
  (let ((locks (var-get reset-locks)))
    (asserts! (not (some (lambda (p) (is-eq p tx-sender)) locks)) (err u3))
    (var-set reset-locks (cons tx-sender locks))
    (update-total-ops)
    (map-set counters tx-sender u0)
    (record-operation tx-sender "reset" u0)
    (ok u0)
  )
)
