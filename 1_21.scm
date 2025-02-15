;; Find prime O(sqrt(n))
(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

;; Find prime O(log(n))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; Exercise 1.21
;; (smallest-divisor 199)
;; => 199
;; (smallest-divisor 1999)
;; => 1999
;; (smallest-divisor 19999)
;; => 7

;; Exercise 1.22
(define (runtime) (real-time-clock))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (search-for-primes a b)
  (timed-prime-test a)
  (cond ((> a b) (display "stop"))
        ((even? a) (search-for-primes (+ a 1) b))
        (else (search-for-primes (+ a 2) b))))

;; 3 smallest primes greater than 1000
;; 1009 1013 1019

;; 3 smallest primes greater than 10000
;; 10007 10009 10037

;; 3 smallest primes greater than 100000
;; 100003 100019 100043

;; 3 smallest primes greater than 1000000
;; 1000003 1000033 1000037

;; runtime was not cooperating to get sufficent
;; precision

;; Exercise 1.23
;; After checking if a number is divisible by 2
;; there is no need to check any other even numbers

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

;; This halves the number of steps

;; Exercise 1.24
;; Replace prime? with fast-prime? and gather
;; runtime data. Explain.

;; Exercise 1.25
;; (define (expmod base exp m)
;;   (cond ((= exp 0) 1)
;;         ((even? exp)
;;          (remainder (square (expmod base (/ exp 2) m))
;;                     m))
;;         (else
;;          (remainder (* base (expmod base (- exp 1) m))
;;                     m))))

;; vs
;; (define (expmod base exp m)
;;   (remainder (fast-expt base exp) m))


;; (define (fast-expt b n)
;;   (cond ((= n 0) 1)
;;         ((even? n) (square (fast-expt b (/ n 2))))
;;         (else (* b (fast-expt b (- n 1))))))


;; (expmod 3 4 2)
;; (remainder
;;  (square (expmod 3 2 2)) 2)
;; (remainder
;;  (square (remainder
;;           (square (expmod 3 1 2)) 2)))
;; (remainder
;;  (square (remainder
;;           (square (remainder
;;                    (* 3 (expmod 3 0 2)) 2)))))

;; (expmod 3 4 2)
;; (remainder (fast-expt 3 4) 2)
;; (remainder
;;  (square (fast-expt 3 2))
;;  2)
;; (remainder
;;  (square (square (fast-expt 3 1)))
;;  2)
;; (remainder
;;  (square (square (* 3 (fast-expt 3 0))))
;;  2)
;; (remainder
;;  (square (square (* 3 1)))
;;  2)

;; The fast-expt implementation has less deferred operations,
;; however the numbers computed are huge (square of a square of
;; a square). The original implemenation keeps the numbers decreasing
;; in size. The second implementation ends up with a longer runtime.

;; Exercise 1.26

;; (define (expmod base exp m)
;;   (cond ((= exp 0) 1)
;;         ((even? exp)
;;          (remainder (square (expmod base (/ exp 2) m))
;;                     m))
;;         (else
;;          (remainder (* base (expmod base (- exp 1) m))
;;                     m))))

;; vs

;; (define (expmod base exp m)
;;   (cond ((= exp 0) 1)
;;         ((even? exp)
;;          (remainder (* (expmod base (/ exp 2) m)
;;                        (expmod base (/ exp 2) m))
;;                     m))
;;         (else
;;          (remainder (* base (expmod base (- exp 1) m))
;;                     m))))

;; In the case where the square procedure is no longer being used
;; expmod is recursively called twice on the even case. Therefore
;; instead of halving the number of steps they remain the same.
;; O(n) instead of O(log(n))

;; Exercise 1.27
;; Carmichael numbers 561 1105 1729 2465 2821 6601
(define (carmichael? n) ;; Or prime
  (define (test a)
    (= (expmod a n n) a))
  (define (iter a)
    (cond ((= a 0) true)
          ((test a) (iter (- a 1)))
          (else false)))
  (iter (- n 1)))
