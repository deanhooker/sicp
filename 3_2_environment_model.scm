;; Exercise 3.9

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 6)
;; ------------------------------------------
;; Global env
;; factorial
;; ------------------------------------------
;;     ^ ^                       ^ ^
;; E1: n:6                 | E2: n:5
;; (* n                    | (* n
;;    (factorial (- n 1))) |    (factorial (- n 1)))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (factorial n)
  (fact-iter 1 1 n))

(factorial 6)
;; ------------------------------------------
;; Global env
;; factorial
;; fact-iter
;; ------------------------------------------
;;     ^ ^                 ^         ^
;; E1: n:6           | E2: product:1
;; (fact-iter 1 1 n) |     counter:1
;;                   |     max-count:6
;;                         (if (> counter max-count)
;;                           product
;;                          (fact-iter (* counter product)
;;                                     (+ counter 1)
;;                                     max-count))

;; etc, etc.

;; Exercise 3.10
;; -----------------------------------------
;; make-withdraw:...
;; W2:-----------------------\
;; W1:-\                     \
;; -----------------------------------------
;;     ^                     ^
;; E1: initial-amount:100    E2: initial-amount:100
;; ^                         ^
;; E3: balance:100           E4: balance:100

;; Using let within the procedure definition just creates
;; an extra frame. It behaves the same as without the extra
;; frame.

;; Exercise 3.11
;; Looking back at the message passing account example in
;; 3.1.1, we can see that the balance which the object is
;; initialized with is in a new frame pointing to the global
;; environment. When we create a new object we create a new
;; frame, separate from the first.
