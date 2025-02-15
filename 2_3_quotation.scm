;; Exercise 2.54
(define (equal? xs ys)
  (cond ((and (null? xs)
              (null? ys)) #t)
        ((not (pair? xs)) (eq? xs ys))
        (else (and (equal? (car xs) (car ys))
                   (equal? (cdr xs) (cdr ys))))))

;; Exercise 2.55
(car ''abracadabra)
;; => (car (quote (quote abracadbra)))
;; => quote
