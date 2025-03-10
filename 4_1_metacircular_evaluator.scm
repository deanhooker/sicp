;; Eval
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable exp env))
        ((quoted? exp) (text-of-quotation exp))
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
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or or env))
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

;; Procedure Arguments
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; Conditionals
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; Sequences
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; Assignment and definitions
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

;; Exercise 4.1
;; list-of-values inherits the order (right-to-left
;; or left-to-right) from the underlying LISP.
(define (list-of-values-left-to-right exps env)
  (if (no-operands? exps)
      '()
      (let ((x (eval (first-operand exps) env)))
        (cons x
              (list-of-values-left-to-right
               (rest-operands exps) env)))))

(define (list-of-values-right-to-left exps env)
  (if (no-operands? exps)
      '()
      (let ((xs
             (list-of-values-right-to-left
              (rest-operands exps)
              env)))
        (cons (eval (first-operand exps) env)
              xs))))

;; Representing Expressions
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cdr exp))

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

(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

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

;; Procedure Application
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

;; Derived Expressions
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp)
  (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

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

;; Exercise 4.2
;; If we re-order the conditionals in eval so
;; procedure application is first then procedure
;; application will happen in places we don't want
;; it to, e.g. (define x 10) or (quote hello) look
;; like applications when in fact they need to be
;; treated differently

;; We can use a tagged-list to put application first
;; e.g. (call <procedure> <operands>)
;; (call factorial 3) or (call + 1 2)

;; (define (eval exp env)
;;   (cond
;;         ((application? exp)
;;          (apply (eval (operator exp) env)
;;                 (list-of-values (operands exp) env)))
;;         ((self-evaluating? exp) exp)
;;         ((variable? exp) (lookup-variable exp env))
;;         ((quoted? exp) (text-of-quotation exp))
;;         ((assignment? exp) (eval-assignment exp env))
;;         ((definition? exp) (eval-definition exp env))
;;         ((if? exp) (eval-if exp env))
;;         ((lambda? exp)
;;          (make-procedure (lambda-parameters exp)
;;                          (lambda-body exp)
;;                          env))
;;         ((begin? exp)
;;          (eval-sequence (begin-actions exp) env))
;;         ((cond? exp) (eval (cond->if exp) env))
;;         (else
;;          (error "Unknown expression type -- EVAL" exp))))

;; (define (application? exp)
;;   (tagged-list? exp 'call))
;; (define (operator exp)
;;   (cadr exp))
;; (define (operands exp)
;;   (cddr exp))
