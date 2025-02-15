;; Line Segments Exercises
;; Exercise 2.2
;; Segment interface
(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

;; Point inferface
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; Segment procedures
(define (midpoint-segment seg)
  (let ((s (start-segment seg))
        (e (end-segment seg)))
    (make-point (/ (+ (x-point e) (x-point s))
                   2)
                (/ (+ (y-point e) (y-point s))
                   2))))

;; Exercise 2.3
;; A rectangle can be defined as two points or
;; two segments (which can be parallel or
;; perpendicular)

;; Create rectangle as bottom and left segments
;; (define (make-rectangle p1 p2)
;;   (if (or (= (x-point p1) (x-point p2))
;;           (= (y-point p1) (y-point p2)))
;;       (error "points cannot have same x or y coordinates")
;;       (let ((lower-y (if (> (y-point p1) (y-point p2))
;;                          (y-point p2)
;;                          (y-point p1)))
;;             (upper-y (if (> (y-point p1) (y-point p2))
;;                          (y-point p1)
;;                          (y-point p2)))
;;             (lower-x (if (> (x-point p1) (x-point p2))
;;                          (x-point p2)
;;                          (x-point p1)))
;;             (upper-x (if (> (x-point p1) (x-point p2))
;;                          (x-point p1)
;;                          (x-point p2))))
;;         (cons (make-segment (make-point lower-x
;;                                         lower-y)
;;                             (make-point upper-x
;;                                         lower-y))
;;               (make-segment (make-point lower-x
;;                                         lower-y)
;;                             (make-point lower-x
;;                                         upper-y))))))

;; (define (get-upper-left rect)
;;   (end-segment (cdr rect)))

;; (define (get-lower-left rect)
;;   (start-segment (cdr rect)))

;; (define (get-lower-right rect)
;;   (end-segment (car rect)))

;; Rectangle as two points
(define (make-rectangle p1 p2)
  (if (or (= (x-point p1) (x-point p2))
          (= (y-point p1) (y-point p2)))
      (error "points cannot have same x or y coordinates")
      (let ((lower-y (if (> (y-point p1) (y-point p2))
                         (y-point p2)
                         (y-point p1)))
            (upper-y (if (> (y-point p1) (y-point p2))
                         (y-point p1)
                         (y-point p2)))
            (lower-x (if (> (x-point p1) (x-point p2))
                         (x-point p2)
                         (x-point p1)))
            (upper-x (if (> (x-point p1) (x-point p2))
                         (x-point p1)
                         (x-point p2))))
        (cons (make-point lower-x
                          lower-y)
              (make-point upper-x
                          upper-y)))))

(define (get-upper-left rect)
  (make-point (x-point (car rect))
              (y-point (cdr rect))))

(define (get-lower-left rect)
  (cdr rect))

(define (get-lower-right rect)
  (make-point (x-point (cdr rect))
              (y-point (car rect))))

;; Rectangle procedures
(define (length p1 p2)
  (let ((x-length (abs (- (x-point p1)
                          (x-point p2))))
        (y-length (abs (- (y-point p1)
                          (y-point p2)))))
    (sqrt (+ (square x-length)
             (square y-length)))))

(define (perimeter rect)
  (* (+ (length (get-lower-left rect)
                (get-lower-right rect))
        (length (get-lower-left rect)
                (get-upper-left rect)))
     2))

(define (area rect)
  (* (length (get-lower-left rect)
             (get-lower-right rect))
     (length (get-lower-left rect)
             (get-upper-left rect))))

(area (make-rectangle (make-point 1 1)
                      (make-point 2 8)))
