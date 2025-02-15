;; Given halve, double and even define a procedure
;; which spawns an iterative process with O(log(n))
;; time
(define (halve n)
  (/ n 2))

(define (double n)
  (+ n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (* a b)
  (define (iter x y r)
    (cond ((= y 0) r)
          ((even? y) (iter (double x)
                           (halve y)
                           r))
          (else (iter x
                      (- y 1)
                      (+ x r)))))
  (iter a b 0))
