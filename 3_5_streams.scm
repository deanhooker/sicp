;; Streams
;; Streams are delayed lists
;; Exercise 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (take-stream s i)
  (if (= i 0)
      '()
      (cons (stream-car s)
            (take-stream (stream-cdr s)
                         (- i 1)))))

;; Exercise 3.51
(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;; (define x
;;   (stream-map show
;;               (stream-enumerate-interval 0 10)))
;; => 0

;; (stream-ref x 5)
;; => 1 2 3 4

;; (stream-ref x 7)
;; => 6 7

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-stream s)
  (for-each display-line (take-stream s 20)))

;; Exercise 3.52
;; (define sum 0)
;; sum => 0

;; (define (accum x)
;;   ;; (display-line x)
;;   (set! sum (+ x sum))
;;   sum)
;; sum => 0

;; (define seq
;;   (stream-map accum
;;               (stream-enumerate-interval 1 20)))
;; sum => 1

;; (define y (stream-filter even? seq))
;; sum => 6

;; (define z (stream-filter (lambda (x)
;;                            (= (remainder x 5) 0))
;;                          seq))
;; sum => 10

;; (stream-ref y 7)
;; sum => 136
;; (display-stream z)
;; sum => 210

;; If we didn't memoize then accum would have a sum
;; of 210 (z) + 21


;; ====================================
;; Infinite Streams

;; Exercise 3.53
(define (add-streams s1 s2)
  (stream-map + s1 s2))

;; (define s (cons-stream 1 (add-streams s s)))
;; =>(stream 1 2 4 8 16)

;; Exercise 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ 1 n))))

(define factorials
  (cons-stream 1
               (mul-streams factorials integers)))

;; Exercise 3.55
;; Create a procedure which takes a stream s and
;; returns a stream whose elements are:
;; S0, S0 + S1, S0 + S1 + S2, ...
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                            (partial-sums s))))

;; Exercise 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))))))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define S
  (cons-stream 1
               (merge (scale-stream S 2)
                      (merge (scale-stream S 3)
                             (scale-stream S 5)))))

;; Exercise 3.57
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams
                             (stream-cdr fibs)
                             fibs))))

;; To find the nth fibonacci number requires n-2
;; additions.

;; If stream element evals were not memoized
;; additions would grow exponentially.

;; Exercise 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den)
           den
           radix)))

;; (stream-take (expand 1 7 10) 10)
;; => '(1 4 2 8 5 7 1 4 2 8)
;; (inexact (/ 1 7)) ~> 0.14285714285714285

;; (stream-take (expand 3 8 10) 10)
;; => '(3 7 5 0 0 0 0 0 0 0)
;; (inexact (/ 3 8)) ~> 0.375

;; Exercise 3.59
;; a)
(define (integrate-series s)
  (define (iter s i)
    (cons-stream
     (/ (stream-car s) i)
     (iter (stream-cdr s)
           (+ i 1))))
  (iter s 1))

;; b)
;; x -> e^x is its own derivative
;; e^x and integral of e^x are the same except
;; for e^0 which is one
(define exp-series
  (cons-stream 1
               (integrate-series exp-series)))


;; The derivative of sine is cosine and the
;; derivative of cosine is negative sine

;; Cosine is therefore the integral of negative
;; sine
(define cosine-series
  (cons-stream
   1
   (integrate-series
    (scale-stream sine-series -1))))

;; and sine is the integral of cosine
(define sine-series
  (cons-stream
   0
   (integrate-series
    cosine-series)))

(define (accumulate f r xs)
  (define (iter r xs)
    (if (null? xs)
      r
      (iter (f (car xs) r)
            (cdr xs))))
  (iter r xs))

(define (eval-series s x n)
  (let ((terms (stream-map (lambda (c k) (* c (expt x k)))
                           s
                           (integers-starting-from 0))))
    (accumulate + 0.0 (take-stream terms n))))

;; Exercise 3.60
(define (mul-series s1 s2)
  (cons-stream
   (* (stream-car s1)
      (stream-car s2))
   (add-streams
    (scale-stream (stream-cdr s2) (stream-car s1))
    (mul-series (stream-cdr s1) s2))))

;; sin^2(x) + cos^2(x) = 1
;; (eval-series
;;  (add-streams
;;   (mul-series sine-series sine-series)
;;   (mul-series cosine-series cosine-series))
;;  (random 100)
;;  15)
;; => 1.0

;; Exercise 3.61
(define (negate-stream s)
  (scale-stream s -1))

(define (invert-unit-series s)
  (cons-stream
   1
   (negate-stream
    (mul-series (stream-cdr s)
                (invert-unit-series s)))))

(define (invert-series s)
  (let ((constant (stream-car s)))
    (cond ((= constant 0)
           (error 'invert-series
                  "division by zero" s))
          ((= constant 1)
           (invert-unit-series s))
          (else
           (scale-stream
            (invert-unit-series
             (scale-stream s (/ constant)))
            (/ constant))))))

;; Exercise 3.62
(define (div-series s1 s2)
  (let ((den-constant (stream-car s2)))
    (cond ((zero? den-constant)
           (error 'div-series
                  "division by zero" s1 s2))
          (else
           (mul-series s1 (invert-series s2))))))

(define tangent-series
  (div-series sine-series cosine-series))

;; (eval-series tangent-series (atan 0.123) 10) ~> 0.123

;; Exploiting the Stream Paradigm
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map
                  (lambda (guess)
                    (sqrt-improve guess x))
                  guesses)))
  guesses)

;; Exercise 3.63
;; Louis Reasoner suggests implementing sqrt-stream
;; in the following way without the internal
;; guesses variable.
(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map
                (lambda (guess)
                  (sqrt-improve guess x))
                (sqrt-stream x))))

(display-stream (sqrt-stream 2))

;; Because the cdr is a recursive procedure call
;; we're not getting out of the box memoization
;; which we usually do for streams.

;; If our delay function (used for creating stream
;; cdr's) was not memoized, the implementation above
;; would be just as effecient as it is now.

;; Exercise 3.64
(define (stream-limit s tolerance)
  (define (iter s i)
    (if (< (abs (- (stream-car s)
                   (stream-car (stream-cdr s))))
           tolerance)
        (begin
          (display-line "Number of iterations:")
          (display-line i)
          (stream-car (stream-cdr s)))
        (iter (stream-cdr s) (+ i 1))))
  (iter s 0))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;; Exercise 3.65
;; ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define alternating-ones
  (cons-stream 1.0
               (cons-stream -1.0
                            alternating-ones)))

(define ln2-stream
  (mul-streams
   (stream-map (lambda (i)
                 (/ 1.0 i)) integers)
   alternating-ones))

(define (ln2 tolerance)
  (stream-limit (accelerated-sequence euler-transform ln2-stream)
                tolerance))

;; Using no sequences of approximations:
;; (ln2 0.0001) => 19999 iterations

;; Using euler-transform:
;; (ln2 0.0001) => 15 iterations

;; Using tableau data structure:
;; (ln2 0.0001) => 3 iterations


;; Infinite Streams of Pairs
;; Supposed we want the pairs (i, j) with i<=j
;; such that i+j is prime:

;; (stream-filter (lambda (pair)
;;                  (prime? (+ (car pair)
;;                             (cadr pair))))
;;                int-pairs)

;; Now we need to make int-pairs. Suppose we have
;; two streams S = Si and T = Tj

;; (s0, t0) (s0, t1) (s0, t2) ...
;; (s1, t0) (s1, t1) (s1, t2) ...
;; (s2, t0) (s2, t1) (s2, t2) ...

;; We really need the pairs in the array which
;; lie above the diagonal (i<=j):

;; (s0, t0) (s0, t1) (s0, t2) ...
;;          (s1, t1) (s1, t2) ...
;;                   (s2, t2) ...

;; Call the general stream of pairs (pairs S T)
;; and consider it composed of 3 parts:

;; (s0, t0)|(s0, t1) (s0, t2) ...
;; ------------------------------
;;         |(s1, t1) (s1, t2) ...
;;         |         (s2, t2) ...

;; The pair (s0, t0), the rest of the pairs with
;; s0, and the remaining pairs.

;; Note that the 3rd piece is (stream-cdr S) and
;; (stream-cdr T).

;; Also the 2nd piece is
;; (stream-map (lambda (x) (list (stream-car s) x))
;;             (stream-cdr t))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map
     (lambda (x) (list (stream-car s) x))
     (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; We can't use append as we did in the last chapter
;; since it would go to the end of the first stream

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

;; Interleave alternately takes elements from each
;; stream.

(display-stream (pairs integers integers))
;; => (1 1)
;; (1 2)
;; (2 2)
;; (1 3)
;; (2 3)
;; (1 4)
;; (3 3)
;; (1 5)
;; (2 4)
;; (1 6)

;; Exercise 3.66
;; (pairs integers integers) returns:
;; (1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3) (1 5)
;; (2 4) (1 6)

;; Preceeding (1 100) there are the 99th odd number
;; of pairs

;; Exercise 3.67
;; Modify pairs to get all pairs of integers, not
;; just where i<=j
;; (define (pairs s t)
;;   (cons-stream
;;    (list (stream-car s) (stream-car t))
;;    (interleave
;;     (interleave
;;      (stream-map
;;       (lambda (x) (list (stream-car s) x))
;;       (stream-cdr t))
;;      (stream-map
;;       (lambda (x) (list x (stream-car t)))
;;       (stream-cdr s)))
;;     (pairs (stream-cdr s) (stream-cdr t)))))

;; Interleave the stream where S0, Tn

;; Exercise 3.68
;; Louis Reasoner suggests pairs:
;; (define (pairs s t)
;;   (interleave
;;    (stream-map
;;     (lambda (x) (list (stream-car s) x))
;;     t)
;;    (pairs (stream-cdr s) (stream-cdr t))))

;; Not separating out the pair s0t0 leads to
;; infinite recursive calls. The procedure call
;; (pairs (stream-cdr s) (stream-cdr t)) is not
;; delayed by cons-stream

;; Exercise 3.69
;; Write a procedure triples which takes 3 infinite
;; streams such that i =< j =< k.

;; Use it to find all Pythagorean triples of
;; positive integers i^2 + j^2 = k^2

(define (triples s t u)
  (cons-stream
   (list (stream-car s)
         (stream-car t)
         (stream-car u))
   (interleave
    (stream-map
     (lambda (x)
       (cons (stream-car s)
             x))
     (stream-cdr (pairs t u)))
    (triples (stream-cdr s)
             (stream-cdr t)
             (stream-cdr u)))))

;; car is list of cars of each stream
;; interleave:
;; - map pairs of t and u, ignoring first (e.g.
;; (1, 1)), cons the car of s
;; - triples of cdr s, cdr t, and cdr, u

(define pythagorean-triples
  (stream-filter
   (lambda (triplet)
     (= (+ (square (car triplet))
           (square (cadr triplet)))
        (square (caddr triplet))))
   (triples integers integers integers)))


;; Exercise 3.70
;; Create merge-weighted which takes two streams
;; and a weight procedure which takes i1 and j1,
;; (from the streams) and produces a new stream

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((<= (weight s1car) (weight s2car))
                  (cons-stream
                   s1car
                   (merge-weighted
                    (stream-cdr s1)
                    s2
                    weight)))
                 ((> (weight s1car) (weight s2car))
                  (cons-stream
                   s2car
                   (merge-weighted
                    s1
                    (stream-cdr s2)
                    weight)))
                 )))))


(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map
     (lambda (x) (list (stream-car s) x))
     (stream-cdr t))
    (weighted-pairs
     (stream-cdr s)
     (stream-cdr t)
     weight) weight)))

(define (a-weight p) (apply + p))
(define a-stream (weighted-pairs integers integers a-weight))
(define a-10 (take-stream a-stream 10))
;; => ((1 1) (1 2) (1 3) (2 2) (1 4) (2 3) (1 5) (2 4) (3 3) (1 6))

;; Integers not divisible by 2, 3 or 5
(define x (stream-filter
           (lambda (z)
             (and (not (= 0 (remainder z 2)))
                  (not (= 0 (remainder z 3)))
                  (not (= 0 (remainder z 5)))))
           integers))

(define (x-weight p)
  (let ((i (car p))
        (j (cadr p)))
    (+ (* 2 i)
       (* 3 j)
       (* 5 i j))))
(define x-stream
  (weighted-pairs x x x-weight))
(take-stream x-stream 10)
;; => ((1 1) (1 7) (1 11) (1 13) (1 17) (1 19) (1 23) (1 29) (1 31) (7 7))


;; Exercise 3.71
;; Ramanujan numbers
(define (ram-weight p)
  (let ((i (car p))
        (j (cadr p)))
    (+ (cube i)
       (cube j))))

;; Integers weighted by i^3 + j ^3
(define pairs-by-ram
  (weighted-pairs integers integers ram-weight))

(define (stream-compare s weight)
  (define (iter s r)
    (let ((s1 (stream-car s))
          (s2 (stream-car (stream-cdr s))))
      (cond ((= 5 (length r)) r)
            ((= (weight s1) (weight s2))
             (iter
              (stream-cdr s)
              (cons (weight s1) r)))
            (else
             (iter (stream-cdr s) r)))))
  (iter s '()))

;; (stream-compare pairs-by-ram ram-weight)
;; => (32832 20683 13832 4104 1729)

;; Exercise 3.72
;; generate stream of all numbers which can be
;; written as the sum of two squares in 3 different
;; ways
(define (square-weight p)
  (let ((i (car p))
        (j (cadr p)))
    (+ (square i)
       (square j))))

;; Integers weighted by i^2 + j^2
(define pairs-by-square-weight
  (weighted-pairs integers integers square-weight))

(define (stream-compare s weight)
  (define (iter s r)
    (let ((s1 (stream-car s))
          (s2 (stream-car (stream-cdr s)))
          (s3 (stream-car
               (stream-cdr
                (stream-cdr s)))))
      (cond ((= 5 (length r)) r)
            ((= (weight s1)
                (weight s2)
                (weight s3))
             (begin
               (newline)
               (display s1)(display ",")
               (display s2)(display ",")
               (display s3)(display ": ")
               (display (weight s1))
               (iter
                (stream-cdr s)
                (cons (weight s1) r))))
            (else
             (iter (stream-cdr s) r)))))
  (iter s '()))

;; (stream-compare
;;  pairs-by-square-weight square-weight)
;; =>
;; (1 18),(6 17),(10 15): 325
;; (5 20),(8 19),(13 16): 425
;; (5 25),(11 23),(17 19): 650
;; (7 26),(10 25),(14 23): 725
;; (2 29),(13 26),(19 22): 845


;; Didn't create a stream of these numbers


;; Exercise 3.73
(define (integral integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)

(define (RC r c dt)
  (lambda (i v0)
    (add-streams
     (scale-stream (integral i v0 dt) (/ 1 c))
     (scale-stream i
                   r))))

(define RC1 (RC 5 1 0.5))

(take-stream (RC1 ones 0) 10)

;; Exercise 3.74
;; (define (make-zero-crossings
;;          input-stream last-value)
;;   (cons-stream
;;    (sign-change-detector (stream-car input-stream)
;;                          last-value)
;;    (make-zero-crossings (stream-cdr input-stream)
;;                         (stream-car input-stream))))

;; (define zero-crossings
;;   (make-zero-crossings sense-data 0))

;; This is approximately equivalent to:
;; (define zero-crossings
;;   (stream-map sign-change-detector
;;               sense-data
;;               (cons-stream 0 sense-data)))

;; Exercise 3.75
(define (make-zero-crossings
         input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (cons-stream
     (sign-change-detector apvt last-value)
     (make-zero-crossings (stream-cdr input-stream)
                          avpt))))

(define (make-zero-crossings
         input-stream last-value last-avpt)
  (let ((value (stream-car input-stream)))
    (let ((avpt (/ (+ (stream-car input-stream)
                      last-value)
                   2)))
      (cons-stream
       (sign-change-detector apvt last-avpt)
       (make-zero-crossings
        (stream-cdr input-stream)
        value
        avpt)))))


;; Exercise 3.76
;; Smoothing should be abstracted out of the
;; zero-crossing extraction.

(define (smooth s)
  (stream-map average s (cons-stream 0 s)))

(define (make-zero-crossings
         input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (cons-stream
     (sign-change-detector apvt last-value)
     (make-zero-crossings (stream-cdr input-stream)
                          avpt))))

;; (make-zero-crossings (smooth sense-data) 0)

;; Exercise 3.77
(define (integral delayed-integrand
                  initial-value
                  dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand (force delayed-integrand)))
       (add-streams (scale-stream integrand dt)
                    int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (x) x) 1 0.001) 1000)

(define (integral delayed-integrand initial-val dt)
  (cons-stream
   initial-val
   (let ((integrand (force delayed-integrand)))
     (if (stream-null? integrand)
       the-empty-stream
       (integral (delay (stream-cdr integrand))
                 (+ (* dt (stream-car integrand))
                    initial-val)
                 dt)))))

;; Exercise 3.78
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy
    (add-streams (scale-stream dy a)
                 (scale-stream y b)))
  y)

;; Exercise 3.79
(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy
    (stream-map f dy y))
  y)
