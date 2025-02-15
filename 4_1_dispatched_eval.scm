;; exercise 4.3
;; make eval data-directed dispatch
;; note: this code doesn't run. at this point a lot
;; of these functions are yet to be defined.
;; quote and if both work though.
(define primitive-apply apply)
(define primitive-eval eval)
(define **table** (make-equal-hash-table 100))
(define (put key1 key2 value)
  (hash-table/put! **table**
                   (list key1 key2)
                   value))
(define (get key1 key2)
  (hash-table/get **table** (list key1 key2) #f))

(define (count) (hash-table/count **table**))
(define (key-list) (hash-table/key-list **table**))

(define (install-quote-package)
  ;; internal procedures
  (define (text-of-quotation exp env)
    (cadr exp))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'quote x))
  (put 'eval 'quote text-of-quotation)
  'done)
(install-quote-package)

;; (define (install-assignment-package)
;;   ;; internal procedures
;;   (define (assignment-variable exp)
;;     (cadr exp))
;;   (define (assignment-value exp)
;;     (caddr exp))
;;   (define (eval-assignment exp env)
;;     (set-variable-value!
;;      (assignment-variable exp)
;;      (eval (assignment-value exp) env)
;;      env)
;;     'ok)
;;   ;; interface to the rest of the system
;;   (define (tag x) (attach-tag 'quote x))
;;   (put 'eval 'set! eval-assignment)
;;   'done)
;; (install-assignment-package)

(define (install-if-package)
  ;; internal procedures
  (define (false? exp) (eq? false exp))
  (define (eval-if exp env)
    (if (false? (eval (if-predicate exp) env))
        (eval (if-alternative exp) env)
        (eval (if-consequent exp) env)))
  (define (if-predicate exp)
    (cadr exp))
  (define (if-consequent exp)
    (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
      (cadddr exp)
      false))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'if x))
  (put 'eval 'if eval-if)
  'done)
(install-if-package)

;; exercise 4.4 install "and" and "or" special forms
(define (install-and-package)
  ;; internal procedures
  (define (false? exp)
    (eq? false exp))
  (define (eval-and exp env)
    (if (null? (cdr exp))
        (eval (car exp) env)
        (let ((this-exp (eval (car exp) env)))
          (cond
           ((null? this-exp)
            false)
           ((false? this-exp)
            false)
           (else (eval-and (cdr exp) env))))))
  (define (fn exp env)
    (eval-and (cdr exp) env))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'and x))
  (put 'eval 'and fn)
  'done)
(install-and-package)

(define (install-or-package)
  ;; internal procedures
  (define (false? exp)
    (eq? false exp))
  (define (eval-or exp env)
    (if (null? (cdr exp))
        (eval (car exp) env)
        (let ((this-exp (eval (car exp) env)))
          (if (false? this-exp)
              (eval-or (cdr exp) env)
              this-exp))))
  (define (fn exp env)
    (eval-or (cdr exp) env))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'and x))
  (put 'eval 'or fn)
  'done)
(install-or-package)

;; Exercise 4.5
;; Extend cond to support ([test] => [recipient])
;; syntax where recipient is a procedure of one
;; argument, the evaluated test expression.
(define (install-cond-package)
  ;; internal procedures
  (define (cond-clauses exp)
    (cdr exp))
  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
  (define (cond-arrow-clause? clause)
    (eq? (cadr clause) '=>))
  (define (cond-predicate clause) (car clause))
  (define (cond-actions clause)
    (if (eq? '=> (cadr clause))
        (caddr clause)
        (cdr clause)))
  (define (cond->if exp)
    (expand-clauses (cond-clauses exp)))
  (define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative))
  (define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))
  (define (make-begin seq)
    (cons 'begin seq))
  (define (last-exp? seq) (null? (cdr seq)))
  (define (first-exp seq) (car seq))
  (define (expand-clauses clauses)
    (if (null? clauses)
        false
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error
                   "ELSE clause isn't last -- COND->IF"
                   clauses))
              (if (cond-arrow-clause? first)
                  (let ((pred (cond-predicate first)))
                    (make-if pred
                             (list
                              (cond-actions first)
                              pred)
                             (expand-clauses rest)))
                  (make-if (cond-predicate first)
                           (sequence->exp (cond-actions first))
                           (expand-clauses rest)))))))
  (define (fn exp env)
    (eval (cond->if exp) env))
  ;; interface to the rest of the system
  (put 'eval 'cond fn)
  'done)
(install-cond-package)

;; Exercise 4.6
(define (install-let-package)
  ;; internal procedures
  (define (let->combination exp)
    (define (vars exp)
      (map car exp))
    (define (vals exp)
      (map cadr exp))
    (define (body exp)
      (caddr exp))
    (define (let-block exp)
      (cadr exp))
    (cons (list 'lambda
                (vars (let-block exp))
                (body exp))
          (vals (let-block exp))))
  (define (fn exp env)
    (eval (let->combination exp) env))
  ;; interface to the rest of the system
  (put 'eval 'let fn)
  'done)
(install-let-package)

;; Exercise 4.7
(define (install-let*-package)
  ;; internal procedures
  (define (body exp)
    (caddr exp))
  (define (let-block exp)
    (cadr exp))
  (define (let->combination exp)
    (if (null? (let-block exp))
        (body exp)
        (list 'let
              (list (car (let-block exp)))
              (let->combination
               (list 'let*
                     (cdr (let-block exp))
                     (body exp))))))
  (define (fn exp env)
    (eval (let->combination exp) env))
  ;; interface to the rest of the system
  (put 'eval 'let* fn)
  'done)
(install-let*-package)

;; Exercise 4.8 Named-let
;; Key is to extend the environment with a new named
;; procedure.
(define (install-named-let-package)
  ;; internal procedures
  (define (let->combination exp env)
    (define (vars exp)
      (map car exp))
    (define (vals exp)
      (map cadr exp))
    (define (body exp)
      (caddr exp))
    (define (let-block exp)
      (cadr exp))
    (define (named-let-body exp)
      (cadddr exp))
    (define (named-let-block exp)
      (caddr exp))
    (define (named-let-fn-name exp)
      (cadr exp))
    (if (not (pair? (let-block exp)))
        (cons
         (list (list 'compound-procedure
                     (vars (named-let-block exp))
                     (named-let-body exp))
               (vals (named-let-block exp)))
         (cons
          (list
           (named-let-fn-name exp)
           (list 'lambda
                 (vars (named-let-block exp))
                 (named-let-body exp)))
          env))
        (list (list 'compound-procedure
                    (vars (let-block exp))
                    (body exp))
              (vals (let-block exp)))))
  (define (fn exp env)
    (eval (car (let->combination exp env))
          (cdr (let->combination exp env))))
  ;; interface to the rest of the system
  (put 'eval 'named-let fn)
  'done)
(install-named-let-package)

(define (tag exp)
  (car exp))
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
;; Sequences
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (last-exp? seq) (null? (cdr seq)))
(define (eval-sequence exps env)
  (newline) (display exps)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))
(define (variable? exp) (symbol? exp))
(define (application? exp)
  (pair? exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (operator exp)
  (car exp))
(define (operands exp)
  (cdr exp))
(define (no-operands? ops)
  (null? ops))
(define (first-operand ops)
  (car ops))
(define (rest-operands ops)
  (cdr ops))
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; Assignment
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp)
  (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;; Eval
(define (eval exp env)
;;  (newline)(display exp)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp)
         (lookup-variable-value exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((and (pair? exp)
              (get 'eval (tag exp)))
         ((get 'eval (tag exp)) exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;; Apply
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))

;; Evaluator Data Structures
;; =========================
;; Predicates
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

;; Procedures
(define (make-procedure params body env)
  (list 'procedure params body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; Environment operations
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-frame vars vals) (cons vars vals))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;; Exercise 4.11
;; Instead of a frame being a list of vars and a
;; list of vals, make it a list of var-val pairs
;; (define (make-frame vars vals)
;;   (cons 'frame (map cons vars vals)))
;; (define (frame-variables frame)
;;   (map car (cdr frame)))
;; (define (frame-values frame)
;;   (map cdr (cdr frame)))
;; (define (add-binding-to-frame! var val frame)
;;   (set-cdr! frame
;;             (cons (cons var val)
;;                   (cdr frame))))

(define (extend-environment vars vals base-env)
  (newline)(display "extend")(display vars)(display vals)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied"
                 vars vals)
          (error "Too few arguments supplied"
                 vars vals))))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error "Variable unassigned --"
                        var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;; Exerise 4.12
;; To abstract define-variable!, set-variable-value!
;; and lookup-variable-value I would abstract the
;; env-loop to take extra parameters. These would
;; be:
;;  - on-find (a procedure to execute on the vals
;;    when the var is found)
;;  - on-frame-end (a procedure to execute if the
;;    var is not found in the frame, can be to move
;;    on to the next frames)
;;  - on-env-end (a procedure with what to do if
;;    the var isn't found anywhere)

;; Exercise 4.13
;; Create make-unbound! which removes a variable.
;; I would make this unbind in every frame. Making
;; it unbind in only the first frame could lead to
;; unexpected behavior where the programmer thinks
;; they've unbound a variable but it was actually
;; bound several frames back so the command is
;; ignored.

;; The trade off is that a procedure can unbind a
;; variable used elsewhere.

;; Another option is to just create an entry in the
;; first frame which shows the variable as unbound,
;; (unbinding it if its in this frame), which would
;; give this command lexical scoping, isolating the
;; effect.


;; Running the Evaluator
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'display display)
        (list 'newline newline)
        (list 'map map)
        ;; ...
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive
                            (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (primitive-apply
   (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

;; Driver Loop / REPL
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval output:")

(define (prompt-for-input string)
  (newline)(newline)(display string)(newline))
(define (announce-output string)
  (newline)(display string)(newline))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))


(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input
                        the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

;; Exercise 4.14
;; Using map from the underlying scheme doesn't
;; work unless the fn being passed to map is from
;; underlying scheme.

;; Redefining map in the metacircular evaluator's
;; implementation works for both primitive and
;; new compound procedures.

;; Data as Programs
;; Exercise 4.15
;; (define (halts? p a)
;;   (error 'halts? "the halting problem is impossible to solve"))

;; (define (run-forever) (run-forever))
;; (define (try p) (if (halts? p p) (run-forever) 'halted))

;; It is impossible for halts to determine whether
;; (p a) runs forever. Consider (try try). If it
;; halts then (halts? try try) => #t =>
;; (run-forever), it must run forever. But then
;; (halts? try try) => #f.

;; Exercise 4.16
;; a) change lookup-variable-value to signal
;; an error if the value is *unassigned*
;; b) Note this doesn't finish executing the whole
;;    body as 'begin' hasn't been implemented.
(define (scan-out-defines exp)
  (let ((body (lambda-body exp)))
    (let ((defs (filter (lambda (e)
                          (eq? (car e) 'define))
                        body))
          (non-defs (filter (lambda (e)
                              (not
                               (eq? (car e) 'define)))
                            body)))
      (list
       'lambda
       (lambda-parameters exp)
       (cons 'let
             (cons
              (map (lambda (x)
                     (list (definition-variable x)
                           ''*unassigned*)) defs)
             ;;non-defs
              (append
               (map (lambda (x)
                      (list
                       'set!
                       (definition-variable x)
                       (definition-value x))) defs)
               non-defs))
             )))))

;; Exercise 4.17
;; There is an extra frame with the above
;; implementation as there is an added let block
;; which expands into an additional lambda.

;; This will never make a difference in a correct
;; program as the new let scope completely encloses
;; the body.

;; Instead of adding an additional let block we
;; could use (define u '*unassigned*) as the first
;; expression in the body, following by the set!
;; operations.

;; Exercise 4.18
;; (define solve
;;   (lambda (f y0 dt)
;;     (define y (integral (delay dy) y0 dt))
;;     (define dy (stream-map f y))
;;     y))

;; Expanded
;; (lambda (f y0 dt)
;;   (let ((y '*unassigned*)
;;         (dy '*unassigned*))
;;     (let ((a (integral (delay dy) y0 dt))
;;           (b (stream-map f y)))
;;       (set! y a)
;;       (set! dy b))
;;     y))

;; The original solve works because delay prevents
;; dy from being evaluated until after it is
;; defined.

;; The expansion will not work in the expanded case,
;; stream-map evaluates the first term eagerly and
;; so (let (b (stream-map f y))) will fail. y is
;; still unassigned.

;; Exercise 4.19

;; (let ((a 1))
;;   (define (f x)
;;     (define b (+ a x))
;;     (define a 5)
;;     (+ a b))
;;   (f 10))

;; Ben says 16 - When evaluating sequentially a
;; will be 1 when b is evaluated.

;; Alyssa says error - a and b are meant to be
;; simultaneous so a is actually undefined when
;; b is evaluated.

;; Eva says 20 - a and b are defined simultaneously
;; therefore a is 5 and b is 15.

;; To implement Eva's way would require ordering
;; the definition of variables by whether they can
;; evaluate without referencing another. This would
;; be hard to implement efficiently.

;; Exercise 4.20a)
;; (define letrec-bindings cadr)
;; (define letrec-actions cddr)

;; (define (letrec->let exp)
;;   (let ((vars (map binding-variable
;;                    (letrec-bindings exp)))
;;         (vals (map binding-value
;;                    (letrec-bindings exp))))
;;     (make-let (map (lambda (var)
;;                      (list var ''*unassigned*))
;;                    vars)
;;               (append (map make-assignment
;;                            vars vals)
;;                       (letrec-actions exp)))))

;; (define (letrec-pkg)
;;   (put 'eval 'letrec (lambda (exp env) (eval (letrec->let exp) env))))

;; (using eval-pkg let-pkg letrec-pkg)

;; (define env (make-environment))
;; (eval '(letrec () 1) env) => 1
;; (eval '(letrec ((x 2)) x) env) => 2
;; (eval '(letrec ((x 3) (y x)) y) env) => 3
;; (eval '(letrec ((f (lambda (x) (if x (f #f) "done")))) (f #t)) env) => "done"

;; Exercise 4.21
;; a) Comparing the lambda to the fib definition:
;;    The first lambda is the call to the internal
;;    fn we're creating (e.g. fib-iter) and the
;;    second lambda is the internal fn (e.g.
;;    fib-iter)
;; ((lambda (n)
;;    ((lambda (fib)
;;       (fib fib 0 1 0))
;;     (lambda (f a b i)
;;       (if (= i n)
;;           a
;;           (f f b (+ a b) (+ i 1))))))
;;  6)

;; (define (fib n)
;;   (define (fib-iter a b i)
;;     (if (= i n)
;;         a
;;         (fib-iter b (+ a b) (+ i 1))))
;;   (fib-iter 0 1 0))

;; b)
;; (define (f x)
;;   (define (even? n)
;;     (if (= n 0)
;;         true
;;         (odd? (- n 1))))
;;   (define (odd? n)
;;     (if (= n 0)
;;         false
;;         (even? (- n 1))))
;;   (even? x))

;; (define (f x)
;;   ((lambda (even? odd?)
;;      (even? even? odd? x))
;;    (lambda (ev? od? n)
;;      (if (= n 0) true (od? ev? od? (- n 1))))
;;    (lambda (ev? od? n)
;;      (if (= n 0) false (ev? ev? od? (- n 1))))))
