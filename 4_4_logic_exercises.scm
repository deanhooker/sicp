;; Exercise 4.55
;; a)
(supervisor ?person (Bitdiddle Ben))
;; b)
(job ?person (accounting . ?job))
;; c)
(address ?person (Slumerville . ?address))

;; Exercise 4.56
;; a)
(and (supervisor ?person (Bitdiddle Ben))
     (address ?person ?address))
;; b)
(and (salary (Bitdiddle Ben) ?amount1)
     (salary ?person ?amount2)
     (lisp-value > ?amount1 amount2))
;; c)
(and (supervisor ?person ?supervisor)
     (not (job ?supervisor (computer . ?title)))
     (job ?supervisor ?job))

;; Exercise 4.57
(rule (can-replace ?person1 ?person2)
      (and (job ?person1 ?job1)
           (or (job ?person2 ?job1)
               (and (job ?person2 ?job2)
                    (can-do-job ?job1 ?job2)))
           (not (same ?person1 ?person2))))
;; a
(can-replace ?x (Fect Cy D))

;; b
(and (can-replace ?a ?b)
     (salary ?a ?as)
     (salary ?b ?bs))

;; Exercise 4.58
(rule (bigshot ?person ?division)
      (and (job ?person (?division . ?rest))
           (or (not (supervisor ?person ?boss))
               (and (supervisor ?person ?boss)
                    (not (job ?boss (?division . ?r)))
                    (not (bigshot ?boss ?division))))))

;; Exercise 4.59
;; (a)
(meeting ?dept (Friday . ?t))
;; (b)
(rule (meeting-time ?person ?day-and-time)
      (and (job ?person (?dept . ?r))
           (or (meeting ?dept ?day-and-time)
               (meeting the-whole-company ?day-and-time))))
;; (c)
(and (meeting-time (Hacker Alyssa P) (Wednesday . ?time))
     (meeting ?dept (Wednesday . ?time)))

;; Exercise 4.61
(?x next-to ?y in (1 (2 3) 4))
;; =>
;; (1 next-to (2 3) in (1 (2 3) 4))
;; ((2 3) next-to 4 in (1 (2 3) 4))

(?x next-to 1 in (2 1 3 1))
;; =>
;; (2 next-to 1 in (2 1 3 1))
;; (3 next-to 1 in (2 1 3 1))

;; Exercise 4.62
(rule (last-pair? (?x) (?x)))
(rule (last-pair? (?u . ?v) (?x))
      (last-pair? (?v (?x))))

(last-pair? (3) ?x)
=> (last-pair? (3) (3))

(last-pair? (1 2 3) ?x)
=> (last-pair? (1 2 3) (3))

(last-pair? (2 ?x) (3))
=> (last-pair? (2 3) (3))

;; Exercise 4.63
(rule (grandson ?grandfather ?person)
      (and (son ?grandfather ?father)
           (son ?father ?person)))

;; Is logic programming mathematical logic?
;; Exercise 4.64
;; If a recursive expression in a body is specified not last
;; in the rule body then it will always match, and further
;; termination clauses (after the recursive expression) will
;; never be checked.

;; Exercise 4.65
;; There are multiple middle managers whose manager is
;; Oliver Warbucks so his name is repeated. We would have
;; to implement a reduction of repeated values to remove.

;; Exercise 4.66
;; The accumulation-function won't work as a single fact
;; can be returned multiple times. See the answer to
;; Exercise 4.65.

;; Exercise 4.67
;; A loop detector would have to maintain the list of
;; matches it is trying to make, and if a match is trying
;; to be made which is in the list, abort.

;; Exercise 4.68
(rule (reverse () ()))
(rule (reverse ?x ?y)
      (and (append-to-form (?first) ?rest ?x)
           (append-to-form ?rev-rest (?first) ?y)
           (reverse ?rev-rest ?rest)))

;; Exercise 4.69
(rule (end-with-gs (grandson)))
(rule (end-with-gs (?x . ?y)) (end-wth-gs ?y))
(rule ((grandson) ?x ?y) (grandson ?x ?y))
(rule ((great . ?rel) ?x ?y)
      (and (has-son ?f ?y)
           (?rel ?x ?f)
           (end-with-gs ?rel)))

;; Exercise 4.70
;; (define (add-assertion! assertion)
;;   (store-assertion-in-index assertion)
;;   (set! THE-ASSERTIONS
;;         (cons-stream assertion THE-ASSERTIONS))
;;   'ok)

;; The above definition would start repeating assertions as we
;; add more


;; Examples
(assert!
 (address (Bitdiddle Ben)
          (Slumerville (Ridge Road) 10)))
(assert!
 (job (Bitdiddle Ben)
      (computer wizard)))
(assert!
 (salary (Bitdiddle Ben) 60000))


;; Exercise 4.71
;; This will postpone some infinite loop. for example:
;; (assert! (married Minnie Mickey))
;; (assert! (rule (married ?x ?y)
;;                (married ?y ?x)))
;; (married Mickey ?who)
;; if we don't use delay, there is no answer to display. but if we use it, we can get:
;;  ;;; Query output:
;; (married Mickey Minnie)
;; (married Mickey Minnie)
;; (married Mickey Minnie)
;; ....
;; this is better than nothing. the reason of this difference is that in this example (apply-rules query-pattern frame) will lead to infinite loop, if we delay it, we still can get some meaningful answers.

;; Exercise 4.72
;;

;; Exercise 4.73
;; Flatten-stream uses delay explicitly in case
;; of infinite streams

;; Exercise 4.74
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))
(define (simple-flatten stream)
  (stream-map
   stream-car
   (stream-filter
    (lambda (s)
      (not (stream-null? s)))
    stream)))
