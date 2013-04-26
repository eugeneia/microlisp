;;;; units:  A simple language to convert units.
;;;; It reads a query in the form of
;;;;   ((value unit-identifier) target-unit-identifiers...)
;;;; and returns conversion results for value from unit to target-units.
;;;; Depends on includes/standard.ml.

;; units:  Table of units.
(define units
    (quote
     (((millimeter mm)    1)
      ((centimeter cm)   10)
      ((meter m)       1000)
      ((inch in zoll)    25.4)
      ((feet ft fuß)    304.8))))

;; unit-identifiers:  Accessor for identifiers for UNIT.
(define unit-identifiers (lambda (unit)(first unit)))

;; unit-scale:  Accessor for scale factor for UNIT.
(define unit-scale (lambda (unit) (first (rest unit))))

;; find-identifier:  Find IDENTIFIER in LIST.
(define find-identifier
    (lambda (identifier list)
      (find (lambda (member) (symbolic= member identifier))
            list)))

;; find-unit:  Find unit by UNIT-IDENTIFIER in UNITS.
(define find-unit
    (lambda (unit-identifier units)
      (find (lambda (unit)
              (find-identifier unit-identifier (unit-identifiers unit)))
            units)))

;; length-value:  Accessor for value of LENGTH.
(define length-value (lambda (length) (first length)))

;; length-unit-identifier:  Accessor for unit-identifier of LENGTH.
(define length-unit-identifier (lambda (length) (first (rest length))))

;; convert:  Convert VALUE from UNIT to TARGET-UNIT.
(define convert
    (lambda (value unit target-unit)
      (/ (* (unit-scale unit) value)
         (unit-scale target-unit))))

;; query-length:  Accessor for length of QUERY.
(define query-length (lambda (query) (first query)))

;; query-target-unit-identifiers: Accessor for target-unit-identifiers
;; of QUERY.
(define query-target-unit-identifiers (lambda (query) (rest query)))

;; query-valid?:  Returns nil if QUERY is syntactically invalid.
(define query-valid?
    (lambda (query)
      (and (cell? query)
           (let ((length (query-length query))
                 (target-unit-identifiers
                  (query-target-unit-identifiers query)))
             (and (number? (length-value length))
                  (symbol? (length-unit-identifier length))
                  (cell? target-unit-identifiers)
                  (not (find (lambda (identifier)
                               (not (symbol? identifier)))
                             target-unit-identifiers)))))))

;; interpret-query:  Interpret QUERY and return result.
(define interpret-query
    (lambda (query)
      (let ((value (length-value (query-length query)))
            (unit (find-unit (length-unit-identifier
                              (query-length query))
                             units))
            (target-unit-identifiers
             (query-target-unit-identifiers query)))
        (map (lambda (target-unit-identifier)
               (let ((target-unit
                      (find-unit target-unit-identifier units)))
                 (if (and unit target-unit)
                     (list (convert value unit target-unit)
                           target-unit-identifier)
                     (quote unknown-unit-identifier))))
             target-unit-identifiers))))

(let ((query (read nil)))
  (write (if (query-valid? query)
             (interpret-query query)
             (quote invalid-query))
         nil))
