;; Farm Equipment Maintenance Smart Contract
;; Manages agricultural machinery maintenance, parts inventory, and operator training

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_INSUFFICIENT_PARTS (err u103))
(define-constant ERR_EQUIPMENT_IN_USE (err u104))

;; Data structures
(define-map equipment 
  { equipment-id: uint }
  {
    name: (string-ascii 50),
    type: (string-ascii 30),
    status: (string-ascii 20), ;; "operational", "maintenance", "down"
    last-service: uint,
    next-service: uint,
    operator-id: (optional uint),
    location: (string-ascii 50)
  }
)

(define-map maintenance-records
  { record-id: uint }
  {
    equipment-id: uint,
    service-type: (string-ascii 30),
    date: uint,
    technician: principal,
    parts-used: (list 10 uint),
    downtime-hours: uint,
    cost: uint,
    notes: (string-ascii 200)
  }
)

(define-map parts-inventory
  { part-id: uint }
  {
    name: (string-ascii 50),
    category: (string-ascii 30),
    quantity: uint,
    min-threshold: uint,
    unit-cost: uint,
    supplier: (string-ascii 50)
  }
)

(define-map operators
  { operator-id: uint }
  {
    name: (string-ascii 50),
    certification-level: uint,
    training-expiry: uint,
    equipment-authorized: (list 10 uint),
    contact: (string-ascii 100)
  }
)

(define-map service-schedules
  { schedule-id: uint }
  {
    equipment-id: uint,
    service-type: (string-ascii 30),
    scheduled-date: uint,
    assigned-technician: (optional principal),
    priority: (string-ascii 10), ;; "low", "medium", "high", "critical"
    estimated-hours: uint,
    status: (string-ascii 20) ;; "scheduled", "in-progress", "completed", "cancelled"
  }
)

;; Counters
(define-data-var equipment-counter uint u0)
(define-data-var record-counter uint u0)
(define-data-var part-counter uint u0)
(define-data-var operator-counter uint u0)
(define-data-var schedule-counter uint u0)

;; Equipment Management Functions
(define-public (register-equipment (name (string-ascii 50)) (equipment-type (string-ascii 30)) (location (string-ascii 50)))
  (let ((equipment-id (+ (var-get equipment-counter) u1)))
    (map-set equipment
      { equipment-id: equipment-id }
      {
        name: name,
        type: equipment-type,
        status: "operational",
        last-service: u0,
        next-service: (+ stacks-block-height u8760), ;; Approximately 1 year from now
        operator-id: none,
        location: location
      }
    )
    (var-set equipment-counter equipment-id)
    (ok equipment-id)
  )
)

(define-public (update-equipment-status (equipment-id uint) (new-status (string-ascii 20)))
  (let ((equipment-data (unwrap! (map-get? equipment { equipment-id: equipment-id }) ERR_NOT_FOUND)))
    (if (or (is-eq tx-sender CONTRACT_OWNER) 
            (is-eq new-status "operational")
            (is-eq new-status "maintenance")
            (is-eq new-status "down"))
      (begin
        (map-set equipment
          { equipment-id: equipment-id }
          (merge equipment-data { status: new-status })
        )
        (ok true)
      )
      ERR_INVALID_STATUS
    )
  )
)

(define-public (assign-operator (equipment-id uint) (operator-id uint))
  (let ((equipment-data (unwrap! (map-get? equipment { equipment-id: equipment-id }) ERR_NOT_FOUND))
        (operator-data (unwrap! (map-get? operators { operator-id: operator-id }) ERR_NOT_FOUND)))
    (if (is-eq (get status equipment-data) "operational")
      (begin
        (map-set equipment
          { equipment-id: equipment-id }
          (merge equipment-data { operator-id: (some operator-id) })
        )
        (ok true)
      )
      ERR_EQUIPMENT_IN_USE
    )
  )
)

;; Parts Inventory Functions
(define-public (add-part (name (string-ascii 50)) (category (string-ascii 30)) (quantity uint) (min-threshold uint) (unit-cost uint) (supplier (string-ascii 50)))
  (let ((part-id (+ (var-get part-counter) u1)))
    (map-set parts-inventory
      { part-id: part-id }
      {
        name: name,
        category: category,
        quantity: quantity,
        min-threshold: min-threshold,
        unit-cost: unit-cost,
        supplier: supplier
      }
    )
    (var-set part-counter part-id)
    (ok part-id)
  )
)

(define-public (update-part-quantity (part-id uint) (new-quantity uint))
  (let ((part-data (unwrap! (map-get? parts-inventory { part-id: part-id }) ERR_NOT_FOUND)))
    (map-set parts-inventory
      { part-id: part-id }
      (merge part-data { quantity: new-quantity })
    )
    (ok true)
  )
)

(define-public (use-parts (part-ids (list 10 uint)) (quantities (list 10 uint)))
  (begin
    (try! (fold check-and-use-part (zip part-ids quantities) (ok true)))
    (ok true)
  )
)

(define-private (check-and-use-part (part-qty-pair { part-id: uint, quantity: uint }) (previous-result (response bool uint)))
  (match previous-result
    success
      (let ((part-data (unwrap! (map-get? parts-inventory { part-id: (get part-id part-qty-pair) }) ERR_NOT_FOUND))
            (current-qty (get quantity part-data))
            (use-qty (get quantity part-qty-pair)))
        (if (>= current-qty use-qty)
          (begin
            (map-set parts-inventory
              { part-id: (get part-id part-qty-pair) }
              (merge part-data { quantity: (- current-qty use-qty) })
            )
            (ok true)
          )
          ERR_INSUFFICIENT_PARTS
        )
      )
    error (err error)
  )
)

;; Helper function to zip two lists
(define-private (zip (list1 (list 10 uint)) (list2 (list 10 uint)))
  (map pair-elements list1 list2)
)

(define-private (pair-elements (item1 uint) (item2 uint))
  { part-id: item1, quantity: item2 }
)

;; Operator Management Functions
(define-public (register-operator (name (string-ascii 50)) (certification-level uint) (training-expiry uint) (contact (string-ascii 100)))
  (let ((operator-id (+ (var-get operator-counter) u1)))
    (map-set operators
      { operator-id: operator-id }
      {
        name: name,
        certification-level: certification-level,
        training-expiry: training-expiry,
        equipment-authorized: (list),
        contact: contact
      }
    )
    (var-set operator-counter operator-id)
    (ok operator-id)
  )
)

(define-public (update-operator-certification (operator-id uint) (new-level uint) (new-expiry uint))
  (let ((operator-data (unwrap! (map-get? operators { operator-id: operator-id }) ERR_NOT_FOUND)))
    (map-set operators
      { operator-id: operator-id }
      (merge operator-data { 
        certification-level: new-level,
        training-expiry: new-expiry
      })
    )
    (ok true)
  )
)

;; Service Scheduling Functions
(define-public (schedule-service (equipment-id uint) (service-type (string-ascii 30)) (scheduled-date uint) (priority (string-ascii 10)) (estimated-hours uint))
  (let ((schedule-id (+ (var-get schedule-counter) u1))
        (equipment-data (unwrap! (map-get? equipment { equipment-id: equipment-id }) ERR_NOT_FOUND)))
    (map-set service-schedules
      { schedule-id: schedule-id }
      {
        equipment-id: equipment-id,
        service-type: service-type,
        scheduled-date: scheduled-date,
        assigned-technician: none,
        priority: priority,
        estimated-hours: estimated-hours,
        status: "scheduled"
      }
    )
    (var-set schedule-counter schedule-id)
    (ok schedule-id)
  )
)

(define-public (assign-technician (schedule-id uint) (technician principal))
  (let ((schedule-data (unwrap! (map-get? service-schedules { schedule-id: schedule-id }) ERR_NOT_FOUND)))
    (map-set service-schedules
      { schedule-id: schedule-id }
      (merge schedule-data { assigned-technician: (some technician) })
    )
    (ok true)
  )
)

(define-public (complete-service (schedule-id uint) (parts-used (list 10 uint)) (actual-hours uint) (cost uint) (notes (string-ascii 200)))
  (let ((schedule-data (unwrap! (map-get? service-schedules { schedule-id: schedule-id }) ERR_NOT_FOUND))
        (record-id (+ (var-get record-counter) u1))
        (equipment-id (get equipment-id schedule-data)))
    
    ;; Create maintenance record
    (map-set maintenance-records
      { record-id: record-id }
      {
        equipment-id: equipment-id,
        service-type: (get service-type schedule-data),
        date: stacks-block-height,
        technician: (unwrap-panic (get assigned-technician schedule-data)),
        parts-used: parts-used,
        downtime-hours: actual-hours,
        cost: cost,
        notes: notes
      }
    )
    
    ;; Update schedule status
    (map-set service-schedules
      { schedule-id: schedule-id }
      (merge schedule-data { status: "completed" })
    )
    
    ;; Update equipment last service date and calculate next service
    (let ((equipment-data (unwrap! (map-get? equipment { equipment-id: equipment-id }) ERR_NOT_FOUND)))
      (map-set equipment
        { equipment-id: equipment-id }
        (merge equipment-data {
          last-service: stacks-block-height,
          next-service: (+ stacks-block-height u4380), ;; Approximately 6 months
          status: "operational"
        })
      )
    )
    
    (var-set record-counter record-id)
    (ok record-id)
  )
)

;; Read-only functions
(define-read-only (get-equipment (equipment-id uint))
  (map-get? equipment { equipment-id: equipment-id })
)

(define-read-only (get-part-inventory (part-id uint))
  (map-get? parts-inventory { part-id: part-id })
)

(define-read-only (get-operator (operator-id uint))
  (map-get? operators { operator-id: operator-id })
)

(define-read-only (get-service-schedule (schedule-id uint))
  (map-get? service-schedules { schedule-id: schedule-id })
)

(define-read-only (get-maintenance-record (record-id uint))
  (map-get? maintenance-records { record-id: record-id })
)

(define-read-only (get-equipment-status (equipment-id uint))
  (match (map-get? equipment { equipment-id: equipment-id })
    equipment-data (ok (get status equipment-data))
    ERR_NOT_FOUND
  )
)

(define-read-only (check-parts-below-threshold)
  ;; This is a simplified version - in practice you'd iterate through all parts
  (ok "Check inventory levels manually")
)

(define-read-only (get-equipment-by-status (status (string-ascii 20)))
  ;; Simplified - would need to iterate through all equipment in practice
  (ok "Use filtering mechanism")
)

(define-read-only (get-overdue-services)
  ;; Simplified - would check all equipment with next-service < block-height
  (ok "Check service schedules manually")
)


;; title: equipment-maintenance
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

