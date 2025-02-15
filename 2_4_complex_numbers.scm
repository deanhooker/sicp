;; Implement complex number representation and arithmetic

;; We can represent complex numbers in rectangular
;; form [real, imaginary]
;; or polar (magnitude & angle) [magnitude, angle]

;; Assume we have selectors.
;; Arithmetic
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1)
                          (real-part z2))
                       (+ (imag-part z1)
                          (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1)
                          (real-part z2))
                       (- (imag-part z1)
                          (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1)
                        (magnitude z2))
                     (+ (angle z1)
                        (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1)
                        (magnitude z2))
                     (- (angle z1)
                        (angle z2))))

;; Rectangle constructors and selectors
(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))

(define (angle-rectangular z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular
              (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

;; Polar form constructors and selectors
(define (real-part-polar z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part-polar z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude-polar z)
  (car z))

(define (angle-polar z)
  (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar
              (cons r a)))

;; Tagged Data
;; ===========

;; We can use both representations if we are able
;; to tag our data with how it is represented,
;; then we'll know which selectors to use.

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
    (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;; Create tag procedures for our data
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;; Now define generic selectors
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))

;; Data-Directed Programming and Additivity
;; ========================================

;; Checking a type and calling an appropriate
;; procedure is called "dispatching on type".

;; The paradigm we've used is not additive, or
;; modular. Every time a new respresentation is
;; added we need to ensure there are no name
;; conflicts, and we need to edit each selector
;; to dispatch on the right type.

;;                 Polar     \  Rectangular
;;           ======================================
;; real-part \real-part-polar\real-part-rectangular
;; imag-part \imag-part-polar\imag-part-rectangular
;; magnitude \magnitude-polar\magnitude-rectangular
;; angle     \angle-polar    \angle-rectangular

;; As representations and procedures increase the
;; table expands, increasing the maintanence.

;; DATA-DIRECTED PROGRAMMING is the technique to
;; work with such a table directly.

;; Assume we have a put and get procedure to add
;; to this table, and retrieve the relevant
;; procedure. We can install a package by putting
;; a new procedure in this table.

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag
                      (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag
                      (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag
                      (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag
                      (make-from-mag-ang r a))))
  'done)

;; TODO: define install-polar-package

;; The complex arithmetic procedures access the
;; table through a general method called
;; apply-generic:

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

;; Now we can define our generic selectors
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; Exercise 2.73
;; a) Deriv can be converted to dispatch on type,
;; where the type is the operator. We can't dispatch
;; on number? or same-variable? because there are
;; no operators.

;; b)
(define (install-deriv-package)
  ;; internal procedures
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0)
               (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          ((product? m2)
           (append (list '* m1) (cdr m2)))
          ((product? m1)
           (append (list '* m2) (cdr m1)))
          (else (list '* m1 m2))))
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (deriv-product operands var)
    (make-sum
     (make-product (multiplier operands)
                   (deriv (multiplicand operands)
                          var))
     (make-product (deriv (multiplier operands)
                          var)
                   (multiplicand operands))))
  (define (make-difference a1 a2)
    (cond ((and (number? a1) (number? a2)) (- a1 a2))
          ((=number? a1 0) (list '- a2))
          ((=number? a2 0) a1)
          (else (list '- a1 a2))))
  (define (base exp) (car exp))
  (define (exponent exp) (cadr exp))
  (define (make-exponentiation b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          ((and (number? b) (number? e)) (expt b e))
          (else (list '** b e))))
  ;; c)
  (define (deriv-expt operands var)
    (make-product (exponent operands)
                  (make-exponentiation
                   (base operands)
                   (make-difference (exponent operands 1)))
                  (deriv (base operands) var)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'deriv x))
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-expt)
  'done)

;; d)
;; If we want to index the procedures to be accessed the
;; opposite way e.g.
;; ((get (operator exp) 'deriv) (operands exp))
;; we could alter get, put or change the order of the arguments
;; provided to put in the install-deriv-package procedure

;; Exercise 2.74
;; Set of employee records keyed by employee. Record is a set with
;; information such as address and salary.
;; a) get-record retrieves a specified employee's record from a
;; specified personnel file.
;; (define (get-record employee-id records)
;;   (apply-generic 'get-record employee-id records))
;; a division must provide a type for their employee-id and
;; a get-record procedure in their package.

;; b) get-salary
;; (define (get-salary record)
;;   (apply-generic 'get-salary record))
;; record needs to be typed

;; c)

;; Message passing
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle)
           (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg)
  (arg op))

(define (real-part z)
  (apply-generic 'real-part z))

;; Exercise 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;; Exercise 2.76
;; Pros and cons of
;; EXPLICIT DISPATCH
;; Add new types:
;; - Define new constructors and selectors.
;; - Update the cond's for selectors.
;; - One new constructor
;; - Ensure no name clashes
;; Add new operations:
;; - Nothing new

;; DATA-DIRECTED
;; Add new types:
;; - Create an installer with selectors and constructors
;; Add new operations:
;; - Nothing new

;; MESSAGE-PASSING
;; Add new types:
;; - Implement a new constuctor with all selectors
;; Add new operations:
;; - Nothing new
