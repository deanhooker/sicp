;; Exercise 4.25

(define (unless condition usual-val exceptional-val)
  (if condition exceptional-val usual-val))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

;; In our applicative-order scheme trying to
;; evaluate a factorial will enter an infinite
;; recursion. This is because when unless is called
;; all of the arguments must be evaluated before
;; the body of the procedure evaluates, and the
;; usual-val (* n (factorial (- n 1))) recurs.
;; Any recursive procedure definition must contain a
;; conditional so only one branch of the body is
;; executed.

;; In a normal-order language this definition would
;; work.

;; 4.26
;; Unless can be implemented as a special form like
;; if, however then it cannot be used as a higher-
;; order procedure, such as in map.

;; Lazy Utils
(define (actual-value exp env)
  (force-it (eval exp env)))
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))
(define (list-of-delayed-args exps env)
  (if (null? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args
             (rest-operands exps)
             env))))
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; THUNKS
(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))
(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
;; memoizing thunks
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result) ; replace exp with its value
           (set-cdr! (cdr obj) '()) ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))
;; UTIL
(define primitive-apply apply)
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied"
                 vars vals)
          (error "Too few arguments supplied"
                 vars vals))))

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

;; Self Eval
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;; Variables
(define (variable? exp) (symbol? exp))
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

;; Quotation
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp)
  (let ((val (cadr exp)))
    (if (pair? val)
        (cons 'list
              (map (lambda (x)
                     (list 'quote x)) val))
        (list 'atom val))))
(define (atom? exp)
  (tagged-list? exp 'atom))
(define (text-of-atom exp)
  (cadr exp))

;; Assignment
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (eval-assignment exp env)
  (define (assignment-variable exp)
    (cadr exp))
  (define (assignment-value exp)
    (caddr exp))
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
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

;; Definition
(define (definition? exp)
  (tagged-list? exp 'define))
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
(define (eval-definition exp env)
  (define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
  (define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)
                     (cddr exp))))
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;; Let
(define (let? exp) (tagged-list? exp 'let))
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

;; If
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

;; Lambdas
(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; Sequence
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp)
  (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq)
  (cons 'begin seq))
(define (eval-sequence exps env)
  (cond
   ((last-exp? exps) (eval (first-exp exps) env))
   (else (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

;; Begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp)
  (cdr exp))

;; Cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond->if exp)
  (define (cond-clauses exp)
    (cdr exp))
  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
  (define (cond-predicate clause) (car clause))
  (define (cond-actions clause) (cdr clause))
  (define (sequence->exp seq)
    (define (make-begin seq)
      (cons 'begin seq))
    (define (last-exp? seq) (null? (cdr seq)))
    (define (first-exp seq) (car seq))
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))
  (define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative))
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error
                   "ELSE clause isn't last -- COND->IF"
                   clauses))
              (make-if (cond-predicate first)
                       (sequence->exp (cond-actions first))
                       (expand-clauses rest))))))
  (expand-clauses (cond-clauses exp)))

;; Application
(define (application? exp)
  (pair? exp))
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

;; Eval
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((atom? exp) (text-of-atom exp))
        ((quoted? exp)
         (eval (text-of-quotation exp) env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp)
                          env))
        ;; ((and? exp) (eval-and exp env))
        ;; ((or? exp) (eval-or exp env))
        ;; Changed for lazy-evaluation
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;; Apply
(define (apply procedure arguments env)
  (cond
   ((primitive-procedure? procedure)
    (apply-primitive-procedure
     procedure
     (list-of-arg-values arguments env))) ;changed
   ((compound-procedure? procedure)
    (eval-sequence
     (procedure-body procedure)
     (extend-environment
      (procedure-parameters procedure)
      (list-of-delayed-args arguments env) ;changed
      (procedure-environment procedure))))
   (else
    (error "Unknown procedure type -- APPLY" procedure))))

;; Running the Evaluator
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

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

;; Lazy constructors and selectors
(define (lazy-list? z)
  (tagged-list? z 'lazy-list))
(define (list-body z)
  (cadr z))
(define (lazy-cons x y)
  (list 'lazy-list (lambda (m) (m x y))))
(define (lazy-car z)
  (if (lazy-list? z)
      ((list-body z) (lambda (p q) p))
      (error "lazy-car called with non-lazy-list" z)))
(define (lazy-cdr z)
  (if (lazy-list? z)
      ((list-body z) (lambda (p q) q))
      (error "lazy-car called with non-lazy-list" z)))
(define (lazy-list . args)
  (define (lazy-list-helper xs)
    (if (null? xs)
      '()
      (lazy-cons (car xs)
                 (lazy-list-helper (cdr xs)))))
  (lazy-list-helper args))

(define primitive-procedures
  (list (list 'car lazy-car)
        (list 'cdr lazy-cdr)
        (list 'cons lazy-cons)
        (list 'null? null?)
        (list 'list lazy-list)
        (list '= =)
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

;; Driver-loop / REPL
(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (prompt-for-input string)
  (newline)(newline)(display string)(newline))
(define (announce-output string)
  (newline)(display string)(newline))
(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((lazy-list? object)
         (display (list (lazy-car object)
                        '...)))
        (else (display object))))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input
                         the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))


;; Exercise 4.27
;; (define count 0)
;; (define (id x)
;;   (set! count (+ count 1))
;;   x)

;; (define w (id (id 10)))
;; => 'ok

;; count
;; => 1

;; w
;; => 10

;; count
;; => 2

;; The initial definition of w executes the outer
;; id call, but the inner is delayed until w is
;; fully evaluated.

;; Exercise 4.28
;; If the operator isn't forced we can't tell what
;; the procedure is since it is a thunk.

;; Exercise 4.29
;; If the program didn't memoize then each eval
;; of a definition would

;; Exercise 4.30
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))

;; a) Ben's example works because the procedures
;; are primitives.

;; b)
(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

;; Originals
;; (p1 1) => (1 (2))
;; (p2 1) => 1

;; New eval-sequence
;; (p1 1) => (1 (2))
;; (p2 1) => (1 (2))

;; c) The new eval-sequence forces each exp except
;; for the final, which it used normal eval for.
;; The original used normal eval for each.

;; d) The behavior of the new eval-sequence gives
;; more intuitive behavior. Using for-each with
;; compound procedures and not having their
;; behavior forced seems counter to the need for
;; a for-each function in the first place, or
;; a begin block in general. Since any expression
;; in a body except for the final isn't returned,
;; presumably these are all for side-effects.


;; Streams as lazy lists
;; (define (cons x y)
;;   (lambda (m) (m x y)))
;; (define (car z)
;;   (z (lambda (p q) p)))
;; (define (cdr z)
;;   (z (lambda (p q) q)))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(define (add-lists list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (cons (+ (car list1) (car list2))
                    (add-lists (cdr list1)
                               (cdr list2))))))

(define ones (cons 1 ones))

(define integers (cons 1 (add-lists ones integers)))

;; (list-ref integers 17)
;; => 18

(define (integral integrand initial-value dt)
  (define int
    (cons initial-value
          (add-lists (scale-list integrand dt)
                     int)))
  int)

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (map f y))
  y)

(list-ref (solve (lambda (x) x) 1 0.001) 1000)

;; Exercise 4.32
;; The integral and solve procedures using streams
;; require the author to intentionally force
;; and delay components. Using the new lazy
;; evaluator none of these details need to be
;; written by the integral and solve authors.

;; (define (integral delayed-integrand
;;                   initial-value
;;                   dt)
;;   (define int
;;     (cons-stream
;;      initial-value
;;      (let ((integrand (force delayed-integrand)))
;;        (add-streams (scale-stream integrand dt)
;;                     int))))
;;   int)

;; (define (solve f y0 dt)
;;   (define y (integral (delay dy) y0 dt))
;;   (define dy (stream-map f y))
;;   y)

;; The car from stream implementation is not lazy
;; however here the car is lazily evaluated.


;; Exercise 4.33

;; In the case where the quoted expression is a list
;; convert the expression into (list <exp>) and
;; evaluate.

;; In the case where it isn't a list added a new
;; type 'atom which behaves as the original quote
;; (returns the cadr).

;; Added lazy-cons, lazy-car, lazy-cdr, and
;; lazy-list to primitive procedures.

;; Online solutions treated the quoted list case
;; by itself, expanding the expression to solve
;; the problem. However in these examples the
;; procedure (list) wouldn't work.

;; I decided that '(a b c) should be expanded to
;; (list 'a 'b 'c) and rewrote list.

;; Exercise 4.34
;; Altered the returned value from lazy-cons to
;; be a tagged list (lazy-list <procedure>) with
;; helpers to identify whether a list is lazy and
;; to grab the body (used in car and cdr).

;; To print I altered the user-print procedure to
;; identify lazy lists and print the car followed
;; by ....

;; This actually breaks the definition of ones above
;; since cons, car and cdr are now primitives whose
;; arguments are eagerly evaluated. To fix we should
;; create a new class of primitives called
;; primitive-delayed, or have cons, car and cdr
;; installed after driver-loop starts (doing this
;; would require extending lambdas to take variable
;; number of arguments, see list).
