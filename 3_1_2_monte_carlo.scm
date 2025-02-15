;; Example Monte Carlo estimation of pi
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (cesaro-test)
  (= (gcd (random 1000) (random 1000)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

;; Exercise 3.5
;; Monte Carlo integration
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (integration-experiment x1 x2 y1 y2)
  (define (square x) (* x x))
  (lambda ()
    (let ((radius 1))
      (<= (+ (square (random-in-range x1 x2))
             (square (random-in-range y1 y2)))
          (square radius)))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((area (* (- x2 x1) (- y2 y1))))
    (* area (monte-carlo trials (P x1 x2 y1 y2)))))
