;; Arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; Constructors and selectors
(define (make-interval a b)
  (cons a b))

;; Exercise 2.7 - define selectors
(define (lower-bound x)
  (min (car x) (cdr x)))

(define (upper-bound x)
  (max (car x) (cdr x)))


;; Exercise 2.8 - define sub-interval
;; For subtraction the upper bound of x and lower of y will
;; yield the largest number, and the lower of x and upper of
;; y will yield the smallest
(define (sub-interval x y)
  (let ((lower (- (lower-bound x) (upper-bound y)))
        (upper (- (upper-bound x) (lower-bound y))))
    (make-interval lower upper)))

;; Exercise 2.9
;; Show that the width of a sum or difference of two intervals
;; is a function only of the widths of the intervals being
;; combined, whereas this is not true for multiplication or
;; division.

;; (2 3) + (5 6) -> (7 9)
;; 1w      1w       2w

;; (12 14)-(2 3) -> (9 12)
;; 2w      1w       3w

;; (2 3) * (5 6) -> (10 18)
;; 1w      1w       8w

;; (12 14)/(2 3) -> (4 7)
;; 2w      1w       3w

;; Exercise 2.10
;; (12 14)/(-2 2) -> (-7 7)
;; If you divide by an interval which spans 0
;; you end up with a huge interval
;; (12 14)/(-1 1) -> (-14 14)

(define (div-interval x y)
  (if (and (positive? (upper-bound y))
           (negative? (lower-bound y)))
      (error "Cannot divide by an interval which spans 0")
      (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

;; Exercise 2.11
;; (-3 -1) * (4 5) -> -12 -4 -15 -5
;; (-3 1)  * (4 5) -> -12  4 -15  5
;; (1 3)   * (4 5) -> 4   12   5 15

;; (-3 -1) * (-5 -4) ->  15  5  12  4
;; (-3 1)  * (-5 -4) ->  15 -5  12 -4
;; (1 3)   * (-5 -4) ->  -5 -15 -4 -12

;; (-3 -1) * (-5 4) ->  15 -4  -12  -4
;; (-3 1)  * (-5 4) ->  15  4  -12   4
;; ^ this is the special case where min and max need to be
;; taken
;; (1 3)   * (-5 4) ->  -5 -15  4   12

;; Lots of code to implement, so skipped

;; Engineers decide they want to work with intervals such as
;; 3.5 +/- 0.15 rather than [3.35, 3.65] so:
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; Exercise 2.12
(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100))))

(define (percent i)
  (* (/ (width i)
        (center i))
     100))

;; Exercise 2.13 - skipped proof exercise

;; Formula for parallel resistors can be written

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;; Exercise 2.14

;; For par1 percent increases but for par2 percent
;; is conserved.

;; Exercise 2.15
;; The more combinations done on uncertain numbers
;; the larger the error will be, due to
;; re-introducing the same errors. Therefore using
;; par2 will produce tighter error bounds.
