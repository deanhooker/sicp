;; Exercise 3.12

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))

(define y (list 'c 'd))

(define z (append x y))

;; z
;; => (a b c d)

;; (cdr x)
;; => (b)

(define w (append! x y))

;; w
;; => (a b c d)

;; (cdr x)
;; => (b c d)

;; Exercise 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;; (define z (make-cycle (list 'a 'b 'c)))

;; (list 'a 'b 'c) =>
;; [][]->[][]->[][nil]
;; |     |     |
;; a     b     c

;; z =>
;;  |<------------------|
;; [][]->[][]->[][]---->|
;; |     |     |
;; a     b     c

;; If we try to compute (last-pair z) we'll end
;; up in an infinite loop.

;; Exercise 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))

(define w (mystery v))
;;=> (d c b a)

;; mystery is a list reverse operation

;; Exercise 3.15
(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

;; z1
;; => ((a b) a b)
;; (set-to-wow! z1)
;; => ((wow b) wow b)

;; z2
;; => ((a b) a b)
;; (set-to-wow! z2)
;; => ((wow b) a b)


;; Exercise 3.16
;; Incorrect since the same pair could be in
;; the car and cdr. Must check for equality with
;; each pair that has been checked.
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; Exercise 3.17
(define (count-pairs-correct x)
  (let ((r '()))
    (define (iter x)
      (newline)
      (display x)(display r)

      (cond ((not (pair? x)) 0)
            ((not (memq x r))
             (begin
               (set! r (cons x r))
               (+ (iter (car x))
                  (iter (cdr x))
                  1)))
            (else (+ (iter (car x))
                     (iter (cdr x))))))
    (iter x)))

;; Create a local variable to hold the result,
;; if the current pair is not in the result then
;; cons it, and add one to the result of recuring
;; on each branch.

;; Exercise 3.18
(define (cycle? x)
  (let ((seen-pairs '()))
    (define (iter x)
      (cond ((not (pair? x)) #f)
          ((memq (cdr x) seen-pairs) #t)
          (else (begin
                  (set! seen-pairs
                        (cons x seen-pairs))
                  (iter (cdr x))))))
    (iter x)))

constant amount of space
go along list
for each element check if cdr is in previous

(define (cycle? x)
  (let ((this-element x))
    (define (iter)
      (cond ((null? this-element) #f)
            ((memq? this-element))
            (else (set-cdr! this-element (cdr x))
                  (iter))))))
