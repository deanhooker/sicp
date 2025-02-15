;; Exercise 2.17
(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

;; Exercise 2.18
(define (reverse items)
  (define (iter original result)
    (if (null? original)
        result
        (iter (cdr original) (cons (car original)
                                   result))))
  (iter items (list)))

;; Exercise 2.19 - coin counting revisited

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? coins) (null? coins))
(define (first-denomination coins)
  (car coins))
(define (except-first-denomination coins)
  (cdr coins))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)
(cc 100 uk-coins)

;; Order doesn't matter for the coin list because
;; the procedure computes all possible combinations

;; Exercise 2.20
(define (same-parity x . ys)
  (define filter-fn (if (even? x) even? odd?))
  (cons x (filter filter-fn ys )))

;; Mapping
;; Exercise 2.21
(define (square-list items)
  (if (null? items)
      (list)
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;; Exercise 2.22
;; Trying to implement an iterative map fn with
;; car, cdr and cons results in a reversed list
;; because the iterator sees elements from left
;; to right but constructs the answer right to
;; left.

;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;;         answer
;;         (iter (cdr things)
;;               (cons (square (car things))
;;                     answer))))
;;   (iter items (list)))

;; Exercise 2.23
(define (for-each f items)
  (f (car items))

  (if (null? (cdr items))
      #t
      (for-each f (cdr items))))

;; Hierarchical structures

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; Exercise 2.24
;; (list 1 (list 2 (list 3 4)))
;; => (1 (2 (3 4)))

;; Exercise 2.25
'(1 3 (5 7) 9)
(car (cdr (caddr '(1 3 (5 7) 9))))

'((7))
(car (car '((7))))

'(1 (2 (3 (4 (5 (6 7))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car
                                                   (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

;; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

;; (append x y)
;; => (1 2 3 4 5 6)

;; (cons x y)
;; => ((1 2 3) 4 5 6)

;; (list x y)
;; => ((1 2 3) (4 5 6))

;; Exercise 2.27
;; if null result
;; if not pair put in result
;; if pair deep reverse and put in result

(define (deep-reverse items)
  (define (iter original result)
    (cond ((null? original) result)
          ((not (pair? (car original)))
           (iter (cdr original)
                 (cons (car original)
                       result)))
          (else (iter (cdr original)
                      (cons (deep-reverse (car original)) result)))))
  (iter items (list)))

(define x (list (list 1 2) (list 3 4)))

;; Exercise 2.28
(define (concat xs ys)
    (if (null? xs)
        ys
        (cons (car xs)
              (append (cdr xs) ys))))

(define (fringe items)
  (cond ((null? items) (list))
        ((not (pair? (car items)))
         (cons (car items) (fringe (cdr items))))
        (else
         (append (fringe (car items))
                 (fringe (cdr items))))))

;; Exercise 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (branch? thing)
  (number? (car thing)))

(define (total-weight mobile)
  (cond ((and (branch? mobile)
              (number? (branch-structure mobile)))
         (branch-structure mobile))
        ((branch? mobile)
         (total-weight (branch-structure mobile)))
        (else
         (+ (total-weight (left-branch mobile))
            (total-weight (right-branch mobile))))))

(define x
  (make-mobile
   (make-branch 2 (make-mobile
                   (make-branch 2 20)
                   (make-branch 4 10)))
   (make-branch 2 (make-mobile
                   (make-branch 1 20)
                   (make-branch 2 10)))))

(define (balanced? mobile)
  (let ((l (left-branch mobile))
        (r (right-branch mobile)))
    (if (and (number?
              (branch-structure l))
             (number?
              (branch-structure r)))
        (= (* (branch-length l)
              (branch-structure l))
           (* (branch-length r)
              (branch-structure r)))
        (and
         (= (* (branch-length l)
               (total-weight l))
            (* (branch-length r)
               (total-weight r)))
         (balanced? (branch-structure l))
         (balanced? (branch-structure r))))))

;; Mapping over trees
;; Exercise 2.30
(define (square-tree tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree))) tree))

;; Exercise 2.31
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree))) tree))

;; Exercise 2.32
;; List of all subsets
(define (subsets s)
  (if (null? s)
      (list (list))
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (x)
                       (cons (car s) x))
                     rest)))))

;; (subsets '(1 2 3))
;; (append (subsets '(2 3)
;;         (map (lambda (x) (cons (car '(1 2 3)) x)) (subsets '(2 3)))
;; (append (subsets '(2 3)
;;         (map (lambda (x) (cons 1 x)) (subsets '(2 3)))

;; (subsets '(2 3))
;; (append (subsets '(3)
;;         (map (lambda (x) (cons (car '(2 3)) x)) (subsets '(3)))
;; (append (subsets '(3)
;;         (map (lambda (x) (cons 2 x)) (subsets '(3)))

;; (subsets '(3))
;; (append (subsets '()
;;         (map (lambda (x) (cons (car '(3)) x)) (subsets '()))
;; (append (subsets '()
;;         (map (lambda (x) (cons 3 x)) (subsets '()))

;; (subsets '())
;; => '(())

;; (subsets '(3))
;; (append '(())
;;         (map (lambda (x) (cons (car '(3)) x)) '(()))
;; (append '(())
;;         '((3))
;; => '(() (3))

;; (subsets '(2 3))
;; (append '(() (3))
;;         (map (lambda (x) (cons (car '(2 3)) x)) '(() (3)))
;; (append '(() (3))
;;         (map (lambda (x) (cons 2 x)) '(() (3)))
;; (append '(() (3))
;;         '((2) (2 3)))
;; => '(() (3) (2) (2 3))

;; (subsets '(1 2 3))
;; (append '(() (3) (2) (2 3))
;;         (map (lambda (x) (cons (car '(1 2 3)) x)) '(() (3) (2) (2 3)))
;; (append '(() (3) (2) (2 3))
;;         (map (lambda (x) (cons 1 x)) '(() (3) (2) (2 3)))
;; (append '(() (3) (2) (2 3))
;;         '((1) (1 3) (1 2) (1 2 3)))
;; => '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

;; Sequences as conventional interfaces
;; Using a sequence to represent data to operate on allows us to
;; express our program in terms of map, filter, reduce (accumulate)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; Exercise 2.33
(define (map* p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y)) (list) sequence))

(define (append* seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length* sequence)
  (accumulate (lambda (x y)
                (+ 1 y)) 0 sequence))

;; Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (newline)
                (display higher-terms)
                (+ (* higher-terms x)
                   this-coeff))
              0
              coefficient-sequence))

;; Exercise 2.35
;; Redefine count-leaves
(define (count-leaves t)
  (accumulate (lambda (x y)
                (+ x y))
              0
              (map (lambda (x)
                     (if (pair? x)
                         (count-leaves x)
                         1)) t)))

;; Exercise 2.36
(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; (accumulate-n + 0 s)
;; => (22 26 30)

;; Exercise 2.37
;; Matrix:
(define m '((1 2 3 4)
            (4 5 6 6)
            (6 7 8 9)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x)
         (accumulate + 0
                     (map * x v))) m))

;; (matrix-*-vector
;;  '((1 0 3)
;;    (3 1 1)
;;    (0 2 5))
;;  '(2 5 1))
;; => (5 12 15)

(define (transpose mat)
  (accumulate-n cons (list) mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v)
           (matrix-*-vector cols v)) m)))

(define m '((1 2 3)
            (4 5 6)
            (7 8 9)))

(define n '((1 2 3)
            (4 5 6)
            (7 8 9)))

;; Exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

;; (fold-right list (list) (list 1 2 3))
;; => (1 (2 (3 ())))

;; (fold-left list (list) (list 1 2 3))
;; => (((() 1) 2) 3)


;; Exercise 2.39
(define (reverse sequence)
  (fold-right (lambda (x y)
                (newline)
                (display x)(display " ")(display y)
                (append y (list x)))
              (list)
              sequence))

(define (reverse sequence)
  (fold-left (lambda (x y)
               (newline)
               (display x)(display " ")(display y)
               (cons y x))
             (list)
             sequence))

;; Nested Mappings
(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
;; Prime procedures
;;===============================
(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))
;; ===============================
(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair)
                                  (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s)
      (list (list))
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))


;; Exercise 2.40
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j)
            (list i j)) (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

;; Exercise 2.41
(define (unique-triplets n)
  (flatmap
   (lambda (i)
     (flatmap
      (lambda (j)
        (map (lambda (k)
               (list i j k))
             (enumerate-interval 1 (- j 1))))
      (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))


(define (triplets-summing-to-s n s)
  (filter
   (lambda (v)
     (= s
        (+ (car v)
           (cadr v)
           (caddr v))))
   (unique-triplets n)))

;; Exercise 2.42
;; Queens puzzle
(define (row-clear? q1 q2)
  (not (= (car q1) (car q2))))

(define (column-clear? q1 q2)
  (not (= (cadr q1) (cadr q2))))

;; TODO: Check diagonal

(define (safe? k positions)
  (let ((this-queen (car positions))
        (rest (cdr positions)))
    (accumulate (lambda (x r)
                  (and r
                       (row-clear? this-queen x)
                       (column-clear? this-queen x))) #t rest)))

(define empty-board (list))

(define (adjoin-position new-row k rest-of-queens)
  (newline)(display new-row)
  (display " : ")(display k)
  (display " : ")(display rest-of-queens)
  (cons (list k new-row) rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (flatmap
         (lambda (rest-of-queens)
           ;; (newline)(display "roq ")(display rest-of-queens)
           (map (lambda (new-row)
                  ;; (newline)(display "new-row ")(display new-row)
                  (adjoin-position new-row k rest-of-queens))
                (enumerate-interval 1 board-size)))
         (queen-cols (- k 1)))))
  (queen-cols board-size))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (flatmap
         (lambda (new-row)
           ;; (newline)(display "new-row ")(display new-row)
           (map (lambda (rest-of-queens)
                  ;; (newline)(display "roq ")(display rest-of-queens)
                  (adjoin-position new-row k rest-of-queens))
                (queen-cols (- k 1))))
         (enumerate-interval 1 board-size))))
  (queen-cols board-size))

;; If board size = 0 => empty board
;; If board size = 1 => (((1, 1)))
;; If board size = 2
;; => (((1, 1))
;;     ((1, 2))
;;     ((2, 1))
;;     ((2, 2)))
;; If board size = 3
;; => (((1, 1) (2, 3))
;;     ((1, 2) (3, 2))
;;     ((2, 1))
;;     ((2, 2)))

;; (((2 1) (1 1))
;;  ((2 2) (1 1))
;;  ((2 1) (1 2))
;;  ((2 2) (1 2)))

;; (((3 1) (2 1) (1 1))
;;  ((3 2) (2 1) (1 1))
;;  ((3 3) (2 1) (1 1))
;;  ((3 1) (2 2) (1 1))
;;  ((3 2) (2 2) (1 1))
;;  ((3 3) (2 2) (1 1))
;;  ((3 1) (2 3) (1 1))
;;  ((3 2) (2 3) (1 1))
;;  ((3 3) (2 3) (1 1))
;;  ((3 1) (2 1) (1 2))
;;  ((3 2) (2 1) (1 2))
;;  ((3 3) (2 1) (1 2))
;;  ((3 1) (2 2) (1 2))
;;  ((3 2) (2 2) (1 2))
;;  ((3 3) (2 2) (1 2))
;;  ((3 1) (2 3) (1 2))
;;  ((3 2) (2 3) (1 2))
;;  ((3 3) (2 3) (1 2))
;;  ((3 1) (2 1) (1 3))
;;  ((3 2) (2 1) (1 3))
;;  ((3 3) (2 1) (1 3))
;;  ((3 1) (2 2) (1 3))
;;  ((3 2) (2 2) (1 3))
;;  ((3 3) (2 2) (1 3))
;;  ((3 1) (2 3) (1 3))
;;  ((3 2) (2 3) (1 3))
;;  ((3 3) (2 3) (1 3)))

;; Exercise 2.43
;; In the other implementation queen-cols (the more
;; computationally intensive opteration) is called
;; within the map-fn
