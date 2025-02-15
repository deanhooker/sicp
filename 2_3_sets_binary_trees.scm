;; Sets as binary trees

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

;; test sets:
(define set1
  (make-tree 7
             (make-tree 3
                        (make-tree 1
                                   '()
                                   '())
                        (make-tree 5
                                   '()
                                   '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))

(define set2
  (make-tree 3
             (make-tree 1
                        '()
                        '())
             (make-tree 7
                        (make-tree 5
                                   '()
                                   '())
                        (make-tree 9
                                   '()
                                   (make-tree 11
                                              '()
                                              '())))))

(define set3
  (make-tree 5
             (make-tree 3
                        (make-tree 1
                                   '()
                                   '())
                        '())
             (make-tree 9
                        (make-tree 7
                                   '()
                                   '())
                        (make-tree 11
                                   '()
                                   '()))))


(define set4
  (make-tree 6
             (make-tree 2
                        (make-tree 1
                                   '()
                                   '())
                        '())
             (make-tree 9
                        (make-tree 7
                                   '()
                                   '())
                        (make-tree 11
                                   '()
                                   '()))))
;; Set operations
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        (> x (entry set))
         (element-of-set? x (right-branch set))))


(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;; Exercise 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append
       (tree->list-1 (left-branch tree))
       (cons (entry tree)
             (tree->list-1 (right-branch tree))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append
       (tree->list-1 (left-branch tree))
       (cons (entry tree)
             (tree->list-1 (right-branch tree))))))

;; T(n) = 2*T(n/2) + O(n/2)

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

;; T(n) = 2*T(n/2) + O(1)

;; Exercise 2.64
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

;; 1 => 3 calls
;; 2 => 5
;; 3 => 7
;; 4 => 9

;; Exercise 2.65
Flatten O(n)
Ordered list union O(n/2)
list->tree
;; Union set from ordered list representation
(define (union-set* set1 set2)
  (cond
   ((null? set1) set2)
   ((null? set2) set1)
   (else
    (let ((x1 (car set1))
          (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (union-set* (cdr set1)
                              (cdr set2))))
            ((< x1 x2)
             (cons x1
                   (union-set* (cdr set1) set2)))
            ((< x2 x1)
             (cons x2
                   (union-set* set1 (cdr set2)))))))))

(define (union-set set1 set2)
  (list->tree
   (union-set* (tree->list-2 set1)
               (tree->list-2 set2))))

(define (intersection-set* set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set* (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set* (cdr set1) set2))
              ((< x2 x1)
               (intersection-set* set1 (cdr set2)))))))

(define (intersection-set set1 set2)
  (list->tree
   (intersection-set* (tree->list-2 set1)
                      (tree->list-2 set2))))
