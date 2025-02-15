;; Sets as lists

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


;; Exercise 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

;; Exercise 2.60
;; Allow duplicates e.g. #{1 2 3} can be '(1 2 1 3)
;; element-of-set? remains the same

;; adjoin-set doesn't need to check whether x is
;; in the set
(define adjoin-set cons)

;; intersection-set remains the same.
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; union-set is just append
(define union-set append)

;; in this representation the sets can be much
;; larger, therefore element-of-set? will in most
;; cases take longer as n is larger O(n).

;; adjoin-set and union-set no longer need to check
;; whether an element is contained in a set.
;; adjoin-set is O(1) and union is O(n)=> O(n)

;; intersection-set still must check for inclusion
;; in set2 so it is still O(n^2) although n's can
;; be much larger

;; this implementation may be preferred over the
;; previous if adjoin-set and union-set are the
;; most common operations and if sets remain small
