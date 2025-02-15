;; Huffman encoding
;; Leaf constructors and selectors
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

;; Tree constructors and selectors
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; Decoding
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH "
                     bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits)
                              current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))

  (decode-1 bits tree))

;; Weighted sets for constructing tree
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf
                     (car pair)   ; symbol
                     (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

;; Exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; (decode sample-message sample-tree)
;; => (a d a b b c a)

;; Exercise 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (let ((r (right-branch tree))
        (l (left-branch tree)))
    (cond ((and (leaf? r)
                (eq? symbol (symbol-leaf r))) '(1))
          ((and (leaf? l)
                (eq? symbol (symbol-leaf l))) '(0))
          ((memq symbol (symbols r))
           (cons 1
                 (encode-symbol symbol r)))
          ((memq symbol (symbols l))
           (cons 0
                 (encode-symbol symbol l)))
          (else
           (error "symbol not in tree -- ENCODE-SYMBOL"
                  symbol)))))

;; Exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (if (= (length set) 1)
      (car set)
      (let ((new-tree (make-code-tree
                       (car set)
                       (cadr set)))
            (rest-set (cddr set)))
        (successive-merge (adjoin-set new-tree
                                      rest-set)))))

;; Exercise 2.70
(define hufman-tree-data
  '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3)
    (yip 9) (wah 1)))

(define sample
  '(get a job sha na na na na na na na na
        get a job sha na na na na na na na na
        wah yip yip yip yip yip yip yip yip yip
        sha boom))

;; (encode sample (generate-huffman-tree hufman-tree-data))
;; => (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)

;; 84 bits
;; if fixed encoding we have an 8 length alphabet
;; requiring log_2 8 bits per character (3)
;; 3 * (length sample)
;; 3 * 36
;; => 108

;; Exercise 2.71
(generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16)))

(generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64)))

(((((leaf a 1) (leaf b 2) (a b) 3)
   (leaf c 4) (a b c) 7)
  (leaf d 8) (a b c d) 15)
 (leaf e 16) (a b c d e) 31)

;; For n symbols where frequency is 1, 2, 4,...,
;; 2^n-1 it takes 1 bit to encode the most frequent
;; symbol and n-1 to encode the least

;; Exercise 2.72

;; adjoin-set O(n/2)
;; make-leaf-set O(n)
;; successive-merge O(n * log(n)) because its
;; called for each n and adjoin-set is O(log(n))
;; Best case n = 1 worst case n = n
;; O(n * log(n))
;; O(n^2)

(let ((new-tree (make-code-tree
                 (car set)
                 (cadr set)))
      (rest-set (cddr set)))
  (successive-merge (adjoin-set new-tree
                                rest-set)))

;; for set n = 1 => 1
;; for set n = 2 =>
;;   2 successive merge
;;   1 adjoin-set O(2n/2)=O(n)
;; for set n = 3 =>
;;   3
