(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count) (fib-iter a
                                 b
                                 (+ (* p p) (* q q))
                                 (+ (* 2 p q) (* q q))
                                 (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (fib n)
  (fib-iter 1 0 0 1 n))

;; T is the transformation where
;; a <- a + b
;; b <- a

;; T^n therefore gives the pair
;; Fib(n+1)
;; Fib(n)

;; T is the special case Tpq where p=0 and q=1
;; a <- bq + aq + ap
;; b <- bp + aq

;; 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987
