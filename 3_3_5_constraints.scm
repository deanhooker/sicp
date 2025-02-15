;; Constraint system
;; Celsius - Fahrenheit Converter

;; Basic operations on connectors should be:
;; (has-value <connector>)
;; (get-value <connector>)
;; (set-value! <connector> <new-value> <informant>)
;; (forget-value! <connector> <retractor>)
;; (connect <connector> <new-constraint>)

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;; Connectors:
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (make-connector)
  (let ((value false)
        (informant false)
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint
                      constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            ((eq? request 'informant) informant)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))


;; Connectors communicate with constraints by procedures:
;; - inform-about-value
;; - inform-about-no-value

;; Constraints:
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? m1) (has-value? product))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? m2) (has-value? product))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

;; Syntax interfaces:
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

;; Probe
(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

;; Start with:
(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)
(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)
(set-value! C 25 'user)
(set-value! F 212 'user)
(forget-value! C 'user)
(set-value! F 212 'user)

;; Exercise 3.33
(define (averager a b c)
  (define (process-new-value)
    (cond ((and (has-value? a)
                (has-value? b))
           (set-value! c
                       (/ (+ (get-value a)
                             (get-value b))
                          2)
                       me))
          ((and (has-value? b)
                (has-value? c))
           (set-value! a
                       (- (* (get-value c) 2)
                          b)
                       me))
          ((and (has-value? a)
                (has-value? c))
           (set-value! b
                       (- (* (get-value c) 2)
                          a)
                       me))))
  (define (process-forget-value)
    (forget-value! c me)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- AVERAGER"
                  request))))
  (connect a me)
  (connect b me)
  (connect c me)
  'ok)

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))
(constant 3 B)
(averager A B C)
(probe "A" A)
(probe "C" C)
(set-value! A 5 'user)
(forget-value! A 'user)

;; Exercise 3.34
;; Defining squarer to be:
;; (define (squarer a b)
;;   (multiplier a a b))
;; is problematic. If b is updated the procedure
;; will try to pull a value from one of the a's
;; and never find anything.

;; Exercise 3.35
(define (squarer a b)
  (define (process-new-value)
    (cond ((has-value? b)
           (if (< (get-value b) 0)
               (error "square less than 0 -- SQUARER"
                      (get-value b))
               (set-value! a
                           (sqrt (get-value b))
                           me)))
          ((has-value? a)
           (set-value! b
                      (square (get-value a))
                      me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- SQUARER"
                  request))))
  (connect a me)
  (connect b me)
  'ok)

(define A (make-connector))
(define B (make-connector))
(squarer A B)
(probe "A" A)
(probe "B" B)
(set-value! A 5 'user)
(forget-value! A 'user)
(set-value! B 5 'user)

;; Exercise 3.36
;; Draw environment diagram

;; Exercise 3.37
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (cv v)
  (let ((z (make-connector)))
    (constant v z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
(probe "Celsius: " C)
(probe "Fahrenheit " F)
(set-value! F 0 'user)
(forget-value! F 'user)
