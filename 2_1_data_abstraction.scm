;; Rational numbers
;; Util
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

;; Constructors and selectors
(define (make-rat n d)
  (let ((g (abs (gcd n d))))
    (if (and (positive? n) (negative? d))
        (cons (- (/ n g)) (- (/ d g)))
        (cons (/ n g) (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; Arithmetic
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

;; Exercise 2.1
;; Make rat should return a positive rational number
;; if passed two negatives, or the numer should be
;; negative if either numer or denom is negative.

;; Exercise 2.2 see 2_line_segments.scm
;; Exercise 2.3 see 2_line_segments.scm

;; Exercise 2.4
;; (define (cons x y)
;;   (lambda (m) (m x y)))
;; (define (car z)
;;   (z (lambda (p q) p)))

;; (car (cons x y))
;; (car (lambda (m) (m x y)))
;; ((lambda (m) (m x y)) (lambda (p q) p))
;; ((lambda (p q) p) x y)
;; => x

;; cdr would be defined as
;; (define (cdr z)
;;   (z (lambda (p q) q)))

;; Exercise 2.5
;; We can represent pairs of non-negative integers
;; as only numbers and arithmetic operations if
;; we represent the pair a and b as the integer
;; 2^a * 3^b
(define (count-0-remainder-divisions n divisor)
  (define (iter try-exp)
    (if (= 0 (remainder n (exp divisor try-exp)))
        (iter (+ try-exp 1))  ;; Try another division.
        (- try-exp 1)))

  ;; We don't need to try 0 divisions, as that will obviously pass.
  (iter 1))

(define (cons2.5 a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car2.5 n)
  (count-0-remainder-divisions n 2))

(define (cdr2.5 n)
  (count-0-remainder-divisions n 3))

;; Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (int->church n)
   (define (iter i result)
     (if (= i n)
         result
         (iter (+ i 1) (add-1 result))))
   (iter 0 zero))

(define (church->int n)
  ((n (lambda (x) (+ 1 x))) 0))

;; (add-1 zero)
;; ((lambda (n)
;;    (lambda (f)
;;      (lambda (x) (f ((n f) x)))))
;;  (lambda (f) (lambda (x) x)))
;; (lambda (f)
;;   (lambda (x)
;;     (f (((lambda (f) (lambda (x) x)) f) x))))
;; (lambda (f)
;;   (lambda (x)
;;     (f x)))

(define one (lambda (f) (lambda (x) (f x))))

;; (add-1 one)
;; ((lambda (n)
;;    (lambda (f) (lambda (x) (f ((n f) x)))))
;;  (lambda (f) (lambda (x) (f x))))
;; (lambda (f)
;;   (lambda (x)
;;     (f (((lambda (f) (lambda (x) (f x))) f) x))))
;; (lambda (f)
;;   (lambda (x)
;;     (f ((lambda (x) (f x)) x))))
;; (lambda (f)
;;   (lambda (x)
;;     (f (f x))))
;; (lambda (f)
;;   (lambda (x)
;;     (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f) (lambda (x) ((b f) ((a f) x)))))
