;; Nondeterministic computing
(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

;; Prime
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

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (newline)
    (display a)(display b)
    (require prime? (+ a b))
    (list a b)))

;; Exercise 4.35
;; Implement an-integer-between so we can use the
;; following procedure
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

;; Exercise 4.36
;; Looking at how an-integer-starting-from is written shows that the
;; following code wouldn’t work because there is no limit placed on the
;; value k can take:

;; (let* ((i (an-integer-starting-from low))
;;        (j (an-integer-starting-from i))
;;        (k (an-integer-starting-from j)))
;;   (require (= (square k) (+ (square i) (square j)))))

;; This code would never end because the initial values i = j = k =
;; low. The solution is k = √2 low which is not an integer. This will
;; cause the require to fail, an-integer-starting-from will then
;; backtrack to the next possible value of k, which of course will also
;; fail and since there is no limit on k the procedure will never
;; complete. Even if we set low to 3 and start j from i+1 the first
;; solution of (3,4,5) will be found, but on calling try-again for
;; another solution, the same situation occurs.

;; Exercise 4.36
;; (define (a-pythagorean-triple-between low high)
;;   (let ((i (an-integer-between low high))
;;         (hsq (* high high)))
;;     (let ((j (an-integer-between i high)))
;;       (let ((ksq (+ (* i i) (* j j))))
;;         (require (>= hsq ksq))
;;         (let ((k (sqrt ksq)))
;;           (require (integer? k))
;;           (list i j k))))))


;; 1) The first require expression, (require (>= hsq ksq)), causes
;; backtracking for some values of (i , j) that would be tested in the
;; original version. So this combinations of i and j are limited.
;; 2) The second require expression, (require (integer? k)), causes
;; backtracking for some values of k that would be tested, hence limiting
;; the values of k.


;; The previous implementation

;; i = 1 hsq = 100
;; j = 1 ksq = 2
;; require pass
;; k = sqrt 2
;; require fail


;; i = 1 hsq = 100
;; j = 2 ksq = 5
;; require pass
;; k = sqrt 5
;; require fail

;; i = 1 hsq = 100
;; j = 3 ksq = 10
;; require pass
;; k = sqrt 10
;; require fail

;; ...

;; i = 1 hsq = 100
;; j = 6 ksq = 37
;; require pass
;; k = sqrt 37
;; require fail

;; Example nondeterministic programs
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))
(define (multiple-dwelling)
  (define baker (amb 1 2 3 4 5))
  (define smith (amb 1 2 3 4 5))
  (define miller (amb 1 2 3 4 5))
  (define fletcher (amb 1 2 3 4 5))
  (define cooper (amb 1 2 3 4 5))
  (require
   (distinct? (list baker cooper fletcher miller smith)))
  (require (not (= baker 5)))
  (require (not (= cooper 1)))
  (require (not (= fletcher 5)))
  (require (not (= fletcher 1)))
  (require (> miller cooper))
  ;; (require (not (= (abs (- smith fletcher)) 1)))
  (require (not (= (abs (- fletcher cooper)) 1)))
  (list (list 'baker baker)
        (list 'cooper cooper)
        (list 'fletcher fletcher)
        (list 'miller miller)
        (list 'smith smith)))

;; 5 answers

;; Exercise 4.39
;; The order does not affect the answer, but could affect the time.
;; Depends on how many nodes we have to visit. If we can cut off
;; branches of possible answers earlier we can reduce the time.

;; Exercise 4.40
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5)))
    (require (not (= baker 5)))
    (let ((cooper (amb 1 2 3 4 5)))
      (require (not (= cooper 1)))
      (let ((fletcher (amb 1 2 3 4 5)))
        (require (not (= fletcher 5)))
        (require (not (= fletcher 1)))
        (require (not (= (abs (- fletcher cooper)) 1)))
        (let ((miller (amb 1 2 3 4 5)))
          (require (> miller cooper))
          (let ((smith (amb 1 2 3 4 5)))
            (require
             (distinct? (list baker cooper fletcher miller smith)))
            (require (not (= (abs (- smith fletcher)) 1)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))

;; Exercise 4.41

;; Exercise 4.42
(define (require p)
  (if (not p) (amb)))
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))
(define (liars)
  (define betty (amb 1 2 3 4 5))
  (define ethel (amb 1 2 3 4 5))
  (define joan (amb 1 2 3 4 5))
  (define kitty (amb 1 2 3 4 5))
  (define mary (amb 1 2 3 4 5))
  (require
   (distinct?
    (list betty ethel joan kitty mary)))
  (require (not (equal? (= kitty 2)
                        (= betty 3))))
  (require (not (equal? (= ethel 1)
                        (= joan 2))))
  (require (not (equal? (= joan 3)
                        (= ethel 5))))
  (require (not (equal? (= kitty 2)
                        (= mary 4))))
  (require (not (equal? (= mary 4)
                        (= betty 1))))
  (list (list 'betty betty)
        (list 'ethel ethel)
        (list 'joan joan)
        (list 'kitty kitty)
        (list 'mary mary)))

;; ((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))

;; Exercise 4.43
(define (require p)
  (if (not p) (amb)))
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))
(define (puzzle)
  (define moore
    (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa))
  (define downing
    (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa))
  (define hall
    (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa))
  (define hood
    (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa))
  (define parker
    (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa))
  (require
   (distinct?
    (list moore downing hall hood parker)))
  (require (not (equal? moore 'mary)))
  (require (equal? hood 'gabrielle))
  (require (equal? moore 'lorna))
  (require (equal? hall 'rosalind))
  (require (equal? downing 'melissa))
  (require (not (equal? hood 'melissa)))
  (list (list 'moore moore)
        (list 'downing downing)
        (list 'hall hall)
        (list 'hood hood)
        (list 'parker parker)))

;; ((moore lorna) (downing melissa) (hall rosalind) (hood gabrielle) (parker mary))

(define (puzzle)
  (define moore
    (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa))
  (require (equal? moore 'lorna))
  (require (not (equal? moore 'mary)))
  ((lambda ()
    (define downing
      (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa))
    (require (equal? downing 'melissa))
    ((lambda ()
      (define hall
        (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa))
      (require (equal? hall 'rosalind))
      ((lambda ()
        (define hood
          (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa))
        (require (equal? hood 'gabrielle))
        (require (not (equal? hood 'melissa)))
        ((lambda ()
          (define parker
            (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa))
          (require
           (distinct?
            (list moore downing hall hood parker)))
          (list (list 'moore moore)
                (list 'downing downing)
                (list 'hall hall)
                (list 'hood hood)
                (list 'parker parker)))))))))))

;; Parsing Natural Language
(define (require p)
  (if (not p) (amb)))
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))

(sentence (noun-phrase (article the) (noun cat))
          (verb eats))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-word verbs)))

(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*)
                 (cdr word-list)))
  ((lambda (found-word)
     (set! *unparsed* (cdr *unparsed*))
     (list (car word-list) found-word))
   (car *unparsed*)))

(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  ((lambda (sent)
     (require (null? *unparsed*))
     sent)
   (parse-sentence)))

(define prepositions '(prep for to in by with))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))
(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

;; Exercise 4.45
;; (sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase
;;   (verb-phrase
;;    (verb-phrase
;;     (verb lectures)
;;     (prep-phrase (prep to)
;;                  (simple-noun-phrase
;;                   (article the)
;;                   (noun student))))
;;    (prep-phrase (prep in)
;;                 (simple-noun-phrase
;;                  (article the) (noun class))))
;;   (prep-phrase
;;    (prep with)
;;    (simple-noun-phrase (article the) (noun cat)))))

;; (sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase
;;   (verb-phrase
;;    (verb lectures)
;;    (prep-phrase
;;     (prep to)
;;     (simple-noun-phrase
;;      (article the) (noun student))))
;;   (prep-phrase
;;    (prep in)
;;    (noun-phrase
;;     (simple-noun-phrase
;;      (article the) (noun class))
;;     (prep-phrase
;;      (prep with)
;;      (simple-noun-phrase
;;       (article the)
;;       (noun cat)))))))

;; (sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))))) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))

;; (sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb lectures) (prep-phrase (prep to) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))

;; (sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb lectures) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep in) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))))

;; Exercise 4.46
;; English is read left to write so the parser
;; must also evaluate arguments from left to right

;; Exercise 4.50
(define (shuffled s)
  (define (swap s p q)
    (let ((ps (list-starting-from s p))
          (qs (list-starting-from s q)))
      (let ((pv (car ps)))
        (set-car! ps (car qs))
        (set-car! qs pv))))
  (define (iter rest)
    (if (null? rest)
        s
        (let ((n (random (length rest))))
          (swap rest 0 n)
          (iter (cdr rest)))))
  (iter s))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define shuffled-cprocs (shuffled cprocs))
      (define (try-next choices)
        (if (null? choices)
          (fail)
          ((car choices) env
                         succeed
                         (lambda ()
                           (try-next (cdr choices))))))
      (try-next cprocs))))
