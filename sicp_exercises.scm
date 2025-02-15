;; Exercise 1.1
10
;; => 10

(+ 5 3 4)
;; => 12

(- 9 1)
;; => 8

(/ 6 2)
;; => 3

(+ (* 2 4) (- 4 6))
;; => 6

(define a 3)
;; => a

(define b (+ a 1))
;; => b

(+ a b (* a b))
;; => 19

(= a b)
;; => #f

(if (and (> b a) (< b (* a b)))
    b
    a)
;; => 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;; => 16

(+ 2 (if (> b a ) b a))
;; => 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;; => 16

;; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3
;; Define a function which takes 3 arguments and
;; calculates the sum of squares of the two larger
;; numbers.
(define (sum-of-squares x y)
  (+ (* x x)
     (* y y)))

(define (func a b c)
  (cond
   ((and (< a b) (< a c)) (sum-of-squares b c))
   ((and (< b a) (< b c)) (sum-of-squares a c))
   (else (sum-of-squares a b))))

;; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;; If b is positive then add `a` and `b`,
;; if b is 0 or negative then substract `b`
;; from `a`.

;; Exercise 1.5
;; Is interpreter applicative-order or normal-order?
(define (p) (p)) ;; Infinite recursive call

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

;; If applicative-order (p) will be evaluated when
;; passed to `test` and the program will spin up an
;; infinite process.
;; If normal-order (p) will not be evaluated as the
;; predicate will be met and `0` will be returned.

;; 1.1.7 Example: Square Roots by Newton's Method
(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (display guess) (newline)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)

;; Exercise 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter2 (improve guess x)
                 x)))

(define (sqrt2 x)
  (sqrt-iter2 1.0 x))

;; new-if is applicative-order (instead of
;; normal-order special form) and since it calls
;; itself it will create an infinite loop

;; Exercise 1.7
;; good-enough? will not be very accurate for small
;; numbers because the guess squared minus the
;; initial number will meet the greater than 0.001
;; criteria much earlier. For large numbers it will
;; take many more iterations for the guess to be
;; "good enough"

(define (good-enough-better? old-guess new-guess)
  (display old-guess)
  (newline)
  (< (abs (- (abs old-guess)
             (abs new-guess)))
     (/ new-guess 100)))

(define (sqrt-iter3 new-guess old-guess x)
  (if (good-enough-better? old-guess new-guess)
      new-guess
      (sqrt-iter3 (improve new-guess x)
                  new-guess
                  x)))

(define (sqrt3 x)
  (sqrt-iter3 1.0 2.0 x))

;; Exercise 1.8
(define (improve-cbrt y x)
  (/ (+ (/ x (* y y)) (* 2 y))
     3))

(define (cbrt-iter new-guess old-guess x)
  (if (good-enough-better? old-guess new-guess)
      new-guess
      (cbrt-iter (improve-cbrt new-guess x)
                 new-guess
                 x)))

(define (cbrt x)
  (cbrt-iter 1.0 2.0 x))

;; Exercise 1.9
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9 Recursive

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9 Iterative

;; Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

y gives the number of doublings
x gives the number of squarings
(define (f n) (A 0 n)) == 2n
(define (g n) (A 1 n)) == 2^n
(define (h n) (A 2 n)) == 2^2^2^2... n times for n > 1

;; (A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
;; (A 0 (A 0 (A 0 (A 0 64))))
;; (A 0 (A 0 (A 0 128)))
;; (A 0 (A 0 256))
;; (A 0 512)
;; 1024

;; (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 (A 0 (A 1 1))))
;; (A 1 (A 1 (A 0 2)))
;; (A 1 (A 1 4))
;; (A 1 (A 0 (A 1 3)))
;; (A 1 (A 0 (A 0 (A 1 2))))
;; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;; (A 1 (A 0 (A 0 (A 0 2))))
;; (A 1 (A 0 (A 0 4)))
;; (A 1 (A 0 8))
;; (A 1 16)
;; (A 0 (A 1 15))
;; ... and on and on

;; Ways of making change example:
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (count-change amount)
  (cc amount 5))

;; Exercise 1.11
(define (func1.11-recur n)
  (if (< n 3)
      n
      (+ (func1.11-recur (- n 1))
         (* (func1.11-recur (- n 2)) 2)
         (* (func1.11-recur (- n 3)) 3))))

(define (func1.11-iter n)
  (define (iter a b c count)
    (cond ((< n 3) n)
          ((<= count 0) a)
          (else (iter (+ a (* 2 b) (* 3 c))
                      a
                      b
                      (- count 1)))))

  (iter 2 1 0 (- n 2)))

;; Exercise 1.12 Pascal's Triangle
(define (pascal row column)
  (cond ((> column row) "undefined")
        ((or (= row column) (= 1 column)) 1)
        (else (+ (pascal (- row 1) (- column 1))
                 (pascal (- row 1) column)))))

;; Exercise 1.13

;; Exercise 1.14
;; Draw a tree illustrating the process generated
;; by count-change for 11 cents
;; (+
;;  (+
;;   (+
;;    (+
;;     (+ 0
;;        (+ 0
;;           (+ 0
;;              (+ 0
;;                 (+ 0
;;                    (+ 0
;;                       (+ 0
;;                          (+ 0
;;                             (+ 0
;;                                (+ 0
;;                                   (+ 0
;;                                      1)))))))))))
;;     (+ (cc 6 1) ;; etc. => 1
;;        (+
;;         (+ 0
;;            1)
;;         0)))
;;    (+
;;     (+
;;      (+ 0
;;         1)
;;      0)
;;     0))
;;   0)
;;  0)
;; Space / resources = depth of tree = O(amount)
;; Number of steps = nodes = O(amount ^ num-coins)

;; Exercise 1.15
(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; (sine 12.15)
;;  (p (sine 4.05))
;;  (p (p (sine 1.35)))
;;  (p (p (p (sine 0.45))))
;;  (p (p (p (p (sine 0.15)))))
;;  (p (p (p (p (p (sine 0.05))))))
;;  (p (p (p (p (p 0.05)))))

;; p is applied 5 times for angle 12.15
;; order of growth is O(log(n))
;; as n increases spaces and number of steps
;; grows slowly

;; Exercise 1.16
(define (even? n)
  (= (remainder n 2) 0))

(define (expt b n)
  (define (expt-iter new-n a)
    (cond ((= new-n 0) a)
          ((even? new-n) (expt-iter (/ new-n 2)
                                    (* a (square b))))
          (else (expt-iter (- new-n 1)
                           (* a b)))))

  (expt-iter n 1))
