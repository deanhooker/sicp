(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Normal-order

;; (gcd 206 40)
;; (gcd 40 (remainder 206 40))
;; (gcd (remainder 206 40)
;;      (remainder
;;       (remainder 40
;;                  (remainder 206 40))))
;; (gcd (remainder
;;       (remainder 40
;;                  (remainder 206 40)))
;;      (remainder (remainder 206 40)
;;                 (remainder
;;                  (remainder 40
;;                             (remainder 206 40)))))
;; i.e. lots of calls to remainder

;; Applicative-order

;; (gcd 206 40)
;; (gcd 40 (remainder 206 40))
;; (gcd 40 6)
;; (gcd 6 (remainder 40 6))
;; (gcd 6 4)
;; (gcd 4 (remainder 6 4))
;; (gcd 4 2)
;; (gcd 2 (remainder 4 2))
;; (gcd 2 0)

;; 4 calls to remainder
