;; Exercise 1.17

;; Imagine our language doesn't have *
(define (* a b)
  (display b)
  (display "\n")
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

;; If we have halve and double:
(define (halve n)
  (/ n 2))

(define (double n)
  (+ n n))

(define (even? n)
  (= (remainder n 2) 0))

;; A better implementation of * which would be
;; O(log(n)) time space instead of
;; O(n) could be:

(define (* a b)
  (display a)
  (display " ")
  (display b)
  (display "\n")
  (cond ((= b 0) 0)
        ((even? b) (* (double a)
                      (halve b)))
        (else (+ a (* a (- b 1))))))
