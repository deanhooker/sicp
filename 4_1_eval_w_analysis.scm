;; UTIL
(define primitive-apply apply)
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
(define (analyze-self-evaluating exp)
  (lambda (env) exp))

;; Variables
(define (variable? exp) (symbol? exp))
(define (analyze-variable exp)
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
  (lambda (env) (lookup-variable-value exp env)))

;; Quotation
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cdr exp))
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

;; Assignment
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (analyze-assignment exp)
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
  (let ((var (assingment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

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
(define (analyze-definition exp)
  (define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
  (define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)
                     (cddr exp))))

  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

;; Exercise 4.22
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
(define (analyze-if exp)
  (define (if-predicate exp)
    (cadr exp))
  (define (if-consequent exp)
    (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false))
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

;; Lambdas
(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (analyze-lambda exp)
  (define (lambda-parameters exp)
    (cadr exp))
  (define (lambda-body exp)
    (cddr exp))
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

;; Sequence
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

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
(define (analyze-application exp)
  (define (operator exp)
    (car exp))
  (define (operands exp)
    (cdr exp))
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

(define (analyze exp)
  (newline)
  (display "analyze")
  (display exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((variable? exp) (analyze-variable exp))
        ((quoted? exp) (analyze-quoted exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((let? exp) (analyze
                     (let->combination exp)))
        ((if? exp) (analyze-if exp))
        ((lambda? exp)
         (analyze-lambda exp))
        ((begin? exp)
         (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp)
         (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (eval exp env)
  ((analyze exp) env))

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

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
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


;; Exercise 4.23
;; Original
;; (define (analyze-sequence exps)
;;   (define (sequentially proc1 proc2)
;;     (lambda (env) (proc1 env) (proc2 env)))
;;   (define (loop first-proc rest-procs)
;;     (if (null? rest-procs)
;;         first-proc
;;         (loop (sequentially first-proc (car rest-procs))
;;               (cdr rest-procs))))
;;   (let ((procs (map analyze exps)))
;;     (if (null? procs)
;;         (error "Empty sequence -- ANALYZE"))
;;     (loop (car procs) (cdr procs))))

;; '((proc 1) (proc 2) (proc 3))
;; (loop (lambda 1) (list (lambda 2) (lambda 3)))
;; (loop
;;  (lambda (env) (proc1 env) (proc2 env))
;;  (list (lambda 3)))
;; (loop
;;  (lambda (env)
;;    (lambda (env) (proc1 env) (proc2 env))
;;    (proc2 env))
;;  '())


;; ;; New
;; (define (analyze-sequence exps)
;;   (define (execute-sequence procs env)
;;     (cond ((null? (cdr procs)) ((car procs) env))
;;           (else ((car procs) env)
;;                 (execute-sequence (cdr procs) env))))
;;   (let ((procs (map analyze exps)))
;;     (if (null? procs)
;;         (error "Empty sequence -- ANALYZE"))
;;     (lambda (env) (execute-sequence procs env))))

;; '((proc 1) (proc 2) (proc 3))
;; (list (lambda 1) (lambda 2) (lambda 3))
;; (lambda (env)
;;   (execute-sequence
;;    (list (lambda 1) (lambda 2) (lambda 3))
;;    env))

;; The old way does all the analysis up front,
;; creating a procedure which takes an environment
;; and executes each body.

;; The new way retains the interwoven analysis and
;; execution
