;; Generic Arithmetic
;; Data-driven operation table
(define **table** (make-equal-hash-table 100))
(define (put key1 key2 value)
  (hash-table/put! **table**
                   (list key1 key2)
                   value))
(define (get key1 key2)
  (hash-table/get **table** (list key1 key2) #f))

(define (count) (hash-table/count **table**))
(define (key-list) (hash-table/key-list **table**))

;; Helper fns
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
    (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

;; Generic Arithmetic Procedures
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;; Install scheme-number package
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (/ x y)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= 0 x)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; Install rational package
(define (install-rational-package)
  ;; Internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))

  ;; interface to rest of system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (add-rat x y)))
  (put 'sub '(rational rational)
       (lambda (x y) (sub-rat x y)))
  (put 'mul '(rational rational)
       (lambda (x y) (mul-rat x y)))
  (put 'div '(rational rational)
       (lambda (x y) (div-rat x y)))
  (put 'equ? '(rational rational) equal-rat?)
  (put '=zero? '(rational)
       (lambda (x) (= 0 (numer x))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'numer '(rational)
       (lambda (r) (numer r)))
  (put 'denom '(rational)
       (lambda (r) (denom r)))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (numer r)
  ((get 'numer 'rational) r))

(define (denom r)
  ((get 'denom 'rational) r))


;; Install complex package
(define (install-complex-package)
  ;; imported procedures from rectangular and
  ;; polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1)
                            (real-part z2))
                         (+ (imag-part z1)
                            (imag-part z2))))
  (define (add-complex3 z1 z2 z3)
    (make-from-real-imag (+ (real-part z1)
                            (real-part z2)
                            (real-part z3))
                         (+ (imag-part z1)
                            (imag-part z2)
                            (imag-part z3))))
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
  (define (equal-complex? z1 z2)
    (and (= (real-part z1)
            (real-part z2))
         (= (imag-part z1)
            (imag-part z2))))
  (define (zero? z)
    (and (= 0 (real-part z))
         (= 0 (imag-part z))))

  ;; interface to rest of system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (add-complex z1 z2)))
  (put 'add '(complex complex complex)
       (lambda (z1 z2 z3) (add-complex3 z1 z2 z3)))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (sub-complex z1 z2)))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (mul-complex z1 z2)))
  (put 'div '(complex complex)
       (lambda (z1 z2) (div-complex z1 z2)))
  (put 'equ? '(complex complex)
       equal-complex?)
  (put '=zero? '(complex) zero?)
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))

  ;; Add selectors for complex numbers
  ;; Exercise 2.77
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; Exercise 2.77
;; (magnitude z) where z is constructed from
;; make-from-real-imag or make-from-mag-ang
;; results in No method for these types -- APPLY-GENERIC (magnitude (complex))

;; So far the complex number selectors only
;; dispatch on types polar or rectangular,
;; they don't recognize complex (complex numbers
;; in our namespace are double tagged, e.g.
;; '(complex polar 2 .3))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; Magnitude must be loaded as a procedure,
;; using apply-generic to dispatch on type.
;; Installing the polar and rectangular packages
;; adds entries to the data-driven table for
;; magnitude polar and rectangular dispatches.

;; When the complex number package is installed
;; we get an entry for magnitude dispatched on
;; complex, which in turn dispatches on the next
;; type, in this case polar.

;; Exercise 2.78
;; Modify type-tag, contents and attach-tag to
;; take advantage of scheme's internal type
;; system.

;; No change to attach-tag. type-tag returns
;; 'scheme-number if number?, and contents
;; returns the datum.
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else
         (error "Bad tagged datum -- TYPE-TAG"
                datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else
         (error "Bad tagged datum -- CONTENTS"
                datum))))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

;; Exercise 2.79
(define (equ? x y) (apply-generic 'equ? x y))

;; Exercise 2.80
(define (=zero? x) (apply-generic '=zero? x))

;; Combining Data Types
;; ====================
;; Create a coercion table with procedures for
;; coercing a type to a new type. Apply-generic
;; tries to coerce args of different types to
;; the type of the other.

(define **coercion-table** (make-equal-hash-table 100))

(define (put-coercion key1 key2 value)
  (hash-table/put! **coercion-table**
                   (list key1 key2)
                   value))
(define (get-coercion key1 key2)
  (hash-table/get **coercion-table** (list key1 key2) #f))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1
                                            type2))
                      (t2->t1 (get-coercion type2
                                            type1)))
                  (cond
                   (t1->t2
                    (apply-generic op
                                   (t1->t2 a1)
                                   a2))
                   (t2->t1
                    (apply-generic op
                                   a1
                                   (t2->t1 a2)))
                   (else
                    (error "No method for these types"
                           (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; Create an entry in the coercion table.
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

;; (add 1 (make-complex-from-real-imag 2 3))
;; => (rectangular 3 . 3)

;; Exercise 2.81
;; (define (scheme-number->scheme-number n) n)
;; (define (complex->complex z) z)
;; (put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
;; (put-coercion 'complex 'complex complex->complex)

(define (exp x y) (apply-generic 'exp x y))
(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (expt x y)))

;; a)
;; Args will attempt to be coerced if there is no
;; procedure for same type args defined.

;; If we call exp for two complex numbers:
;; - there is no exp procedure for two complex nums
;; - we'll coerce complex->complex for each type

;; (exp (make-complex-from-real-imag 2 3)
;;      (make-complex-from-real-imag 4 3))
;; => No method for these types

;; b) Nothing had to be done about coercion, it
;; works correctly. It just does extra work if
;; there is no same type procedure defined.

;; c) We just need to modify apply-generic so
;; it doesn't try coercion if args are the same
;; type.

;; Exercise 2.82
(define (coerce datum new-type)
  (let ((d-type (type-tag datum)))
    (if (eq? d-type new-type)
        datum
        (let ((proc (get-coercion d-type new-type)))
          (if proc
              (proc datum)
              false)))))

(define (coerce-args args type)
  (map (lambda (x) (coerce x type)) args))

(define (gen-arg-list-types args)
  (define (valid? args)
    (define (iter args)
      (cond ((null? args) true)
            ((not (car args)) false)
            (else (iter (cdr args)))))
    (iter args))

  (filter valid?
          (cons args
                (map (lambda (x)
                       (coerce-args args
                                    (type-tag x))) args))))

(define (apply-generic op . args)
  (define (apply1 args*)
    (let ((type-tags (map type-tag args*)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args*))
            false))))
  (let ((r (filter (lambda (x) x)
                   (map apply1 (gen-arg-list-types args)))))
    (if (not (null? r))
        (car r)
        (error "No proc for" (list op (map type-tag args))))))

;; If we have more than two args we can try
;; coercing every arg to the type of the first,
;; then the second, then the third, etc. We could
;; end up missing a valid procedure though if we
;; don't try each supertype. e.g. we only try
;; coercing to the types contained in the args
;; if we have definied a procedure for a type
;; all args can be coerced into, but is not
;; present, apply-generic will fail to find it.

;; Exercise 2.83
(define (install-raise)
  (define (integer->rational x)
    (make-rational x 1))
  (define (rational->real x)
    (attach-tag 'real
                (* 1.0 (/ (numer x) (denom x)))))
  (define (real->complex x)
    (make-complex-from-real-imag x 0))

  (put 'raise '(scheme-number) integer->rational)
  (put 'raise '(rational) rational->real)
  (put 'raise '(real) real->complex)
  'done)

(define (raise datum)
  (apply-generic 'raise datum))

;; Exercise 2.84
(define tower
  '(scheme-number rational real complex))

(define (apply-generic op . args)
  (define (apply1 args*)
    (let ((type-tags (map type-tag args*)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args*))
            false))))
  (let ((r (filter (lambda (x) x)
                   (map apply1 (gen-arg-list-types args)))))
    (if (not (null? r))
        (car r)
        (error "No proc for" (list op (map type-tag args))))))
