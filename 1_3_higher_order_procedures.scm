(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (identity x) x)

(define (inc x)
  (+ x 1))

(define (cube x)
  (* x x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (sum-integers a b)
  (sum identity a inc b))

(define (sum-of-cubes a b)
  (sum cube a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; Exercise 1.29
;; Simpson's rule
(define (simpsons-integral f a b n)
  (define h (/ (- b a) n))
  (define (yk k)
    (f (+ a (* k h))))
  (define (term k)
    (cond ((= k 1) (* 1 (yk k)))
          ((= k n) (* 1 (yk k)))
          ((even? k) (* 4 (yk k)))
          (else (* 2 (yk k)))))
  (* (/ h 3) (sum term 1 inc n)))

;; Exercise 1.30
;; Replace linear recursion sum with iterative
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; Exercise 1.31
(define (product* f a next b)
  (if (> a b)
      1
      (* (f a)
         (product f (next a) next b))))

(define (product f a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (f a)
                          result))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (pi n)
  (define (f i)
    (if (even? i)
        (/ (+ i 2)
           (+ i 1))
        (/ (+ i 1)
           (+ i 2))))
  (* 4 (product f 1.0 inc n)))

;; Exercise 1.32
;; Write accumulate, the abstraction of sum
;; and product:

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner
                            null-value
                            term
                            (next a)
                            next
                            b))))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner (term a)
                        result))))
  (iter a null-value))

;; Exercise 1.33
;; You can write an even more generalized version
;; of accumulate by introducing the notion of a
;; filter:

(define (filtered-accumulate
         pred? combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((pred? a) (iter (next a)
                           (combiner (term a)
                                     result)))
          (else (iter (next a)
                      result))))
  (iter a null-value))
;; a)
;; (filtered-accumulate prime? + 0 identity 1 inc 10)
;; b)
(filtered-accumulate (lambda (x) (gcd x 100)) + 0 identity 1 inc 100)

;; Exercise 1.34
(define (f g)
  (g 2))

;; (f square)

;; (f (lambda (z) (* z (+ z 1))))

;; (f f)
;; ((g 2) 2)


;; Finding zeroes and fixed-points
;; Search for f(x) = 0 where f(a) < 0 < f(b)
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

;; Finding fixed points
;; f(x) = x

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; Define sqrt as a fixed point where
;; y^2 = x or y = x/y
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;; Exercise 1.35
;; Show that the golden ratio is a fixed point
;; of the transformation x -> 1 + 1/x

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;; Exercise 1.36
(fixed-point (lambda (x) (/ (log 1000)
                            (log x))) 2.0)
;; => 4.555532270803653  // 34 steps

(fixed-point (lambda (x) (average x
                                  (/ (log 1000)
                                     (log x)))) 2.0)
;; => 4.555537551999825  // 9 steps

;; Exercise 1.37
(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        (/ (n i)
           (d i))
        (/ (n i)
           (+ (d i) (iter (+ 1 i))))))
  (iter 1.0))

(define (cont-frac-iter n d k)
  (define (iter i r)
    (if (< i 1)
        r
        (iter (- i 1)
              (/ (n i)
                 (+ (d i) r)))))
  (iter k (/ (n k)
             (d k))))

;; Exercise 1.38
;; Use cont-frac to calculate e - 2.
;; Euler's algorithm for this has Ni = 1 and Di
;; successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8,...

(define (divides? a b)
  (= (remainder a b) 0))

(define (e-2 k)
  (define D
    (lambda (i)
      (if (divides? (+ i 1) 3)
          (let ((number-of-times (/ (+ i 1) 3)))
            (* 2 number-of-times))
          1)))
  (define (N i) 1)
  (cont-frac N D k))

;; Exercise 1.39
(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- (square x))))
             (lambda (i)
               (- (* i 2) 1))
             k))


;; Procedures as returned values
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp
                (lambda (y) (/ x (square y))))
               1.0))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

;; Exercise 1.40
;; Define cubic to create x^3 + ax^2 + bx + c
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;; Exercise 1.41
(define (double f)
  (lambda (x)
    (f (f x))))

;; (((double (double double)) inc) 5)
;; => 21

;; Exercise 1.42
;; composition f after g is x -> f(g(x))
(define (compose f g)
  (lambda (x)
    (f (g x))))

;; Exercise 1.43
(define (repeated f n)
  (if (= n 0)
      (lambda (x)
        x)
      (compose f (repeated f (- n 1)))))

;; Exercise 1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (n-fold f n)
  ((repeated smooth n) f))

;; Exercise 1.45
;; Using fixed point y -> x/y did not converge
;; without damping. Damping also works with y ->
;; x/y^2. However it does not work for 4th roots
;; y -> x / y^4 does not converge with a single
;; average-damp.
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (nth-rt x n)
  (fixed-point-of-transform
   (lambda (y) (/ x (expt y (- n 1))))
   (repeated average-damp 3)
   1.0))

;; average damp
;; 1x 2rt 3rt
;; 2x 4th - 7th rt
;; 3x 8th - 15th
;; 4x 16th <
;; TODO repeat average damp based on above
;; observations

;; Exercise 1.46
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (lambda (first-guess)
    (iter first-guess)))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess)
               x))
       0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough?
                      improve) 1.0))

(define (fixed-point f first-guess)
  (define (close-enough? guess)
    (< (abs (- (f guess) guess)) tolerance))
  ((iterative-improve close-enough?
                      f) first-guess))
