;; Representing tables

;; Single key tables:
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

;; Two key tables:
(define (lookup k1 k2 table)
  (let ((subtable (assoc k1 (cdr table))))
    (if subtable
        (let ((record (assoc k2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! k1 k2 value table)
  (let ((subtable (assoc k1 (cdr table))))
    (if subtable
        (let ((record (assoc k2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons k2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list k1
                              (cons k2 value))
                        (cdr table)))))
  'ok)

;; Exercise 3.24
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records))
             (car records))
            (else (assoc key (cdr records)))))
    (define (lookup k1 k2)
      (let ((subtable (assoc k1
                             (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc k2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! k1 k2 value)
      (let ((subtable
             (assoc k1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc k2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons k2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list k1
                                  (cons k2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else
             (error "Unknown operation -- TABLE"
                    m))))
    dispatch))

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; Testing keys can be within 5 ints
;; (define (within-5? given-key table-key)
;;   (> 5 (abs (- given-key table-key))))
;; (define operation-table2 (make-table within-5?))
;; (define get2 (operation-table2 'lookup-proc))
;; (define put2 (operation-table2 'insert-proc!))

;; Exercise 3.27
;; Takeaways
;; - For lookup each key's assoc result can
;;   either be a pair or a list. If no assoc
;;   result then false.
;; - For insert! use helper to construct new
;;   data structure then set! onto old


(define (lookup keys table)
  (define (iter t ks)
    (let ((k (car ks)))
      (let ((subtable (assoc k (cdr t))))
        (if subtable
            (if (list? subtable)
                (iter subtable (cdr ks))
                (cdr subtable))
            false))))

  (iter table keys))

(define (insert! keys value table)
  (define (helper keys value)
    (let ((k (car keys))
          (rest (cdr keys)))
      (if (null? rest)
          (cons k value)
          (list k
                (helper rest value)))))

  (define (iter t ks)
    (let ((k (car ks)))
      (let ((subtable (assoc k (cdr table))))
        (if subtable
            (if (list? subtable)
                (iter subtable (cdr ks))
                (set-cdr! subtable value))
            (set-cdr! t (cons (helper ks value)
                              (cdr t)))))))
  (iter table keys)
  'ok)

;; Exercise 3.26
;; In a table where the key-value records are
;; organized as a binary tree we can just alter assoc
;; to be 0(log(n)). Complexity is added to balance the
;; binary trees. See excercises on sets as binary trees.

;; Exercise 3.27
;; Memoization
(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

;; This fibonacci calculator always computes the nth
;; number in a number of steps proportional to n.
;; Each recursive call to memo-fib stores its value in
;; the memo table so (memo-fib (- n 1)) calculates each
;; value, and (memo-fib (- n 2)) ends up just returning
;; a lookup value.
