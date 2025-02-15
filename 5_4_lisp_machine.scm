;; The Machine Model
;; ==========================================
(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; Registers
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request -- REGISTER"
                    message))))
    dispatch))
(define (get-contents register)
  (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

;; The Stack
(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth
                           max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! current-depth 0)
      (set! max-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list
                'total-pushes '= number-pushes
                'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request -- STACK"
                    message))))
    dispatch))
(define (print-stack-statistics)
  ((lisp-machine 'stack) 'print-statistics))
(define (pop stack)
  (stack 'pop))
(define (push stack value)
  ((stack 'push) value))

;; The machine
(define (start machine)
  (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name)
                 value)
  'done)
(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))
(define (get-instruction-list machine)
  (machine 'get-instruction-list))
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (begin
                (allocate-register name)
                (lookup-register name)))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (unique-instructions)
        (let ((instruction-list '()))
          (define (add-instruction! instr)
            (let ((instr (car instr))
                  (i-type (caar instr)))
              (let ((insts (assoc i-type
                                  instruction-list)))
                (if insts
                    (if (not (member instr
                                     (cadr insts)))
                        (let ((x (list
                                  (cons instr
                                        (cadr insts)))))
                          (set-cdr! insts
                                    x)))
                    (set! instruction-list
                          (cons (list i-type
                                      (list instr))
                                instruction-list))))))
          (for-each add-instruction!
                    the-instruction-sequence)
          instruction-list))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'get-instruction-list)
               (unique-instructions))
              ;; ((eq? message 'add-instruction!)
              ;;  add-instruction!)
              (else
               (error "Unknown request -- MACHINE" message))))
      dispatch)))

;; The assembler
(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (if (assoc next-inst labels)
                   (error "Label duplicated -- EXTRACT-LABELS" next-inst)
                   (receive insts
                       (cons (make-label-entry next-inst
                                               insts)
                             labels)))
               (receive (cons (make-instructions next-inst)

                              insts)
                   labels)))))))
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       ;; (update-instruction-list!
       ;;  inst machine)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels
         machine
         pc
         flag
         stack
         ops)))
     insts)))
(define (make-instructions text)
  (cons text '()))
(define (instruction-text inst)
  (car inst))
(define (instruction-execution-proc inst)
  (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))
;; (define (update-instruction-list! inst machine)
;;   ((machine 'add-instruction!) inst))
(define (make-label-entry label-name insts)
  (cons label-name insts))
(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE"
               label-name))))

;; Generating Execution Procedures
(define (make-execution-procedure
         inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else
         (error "Unknown instruction type -- ASSEMBLE" inst))))

;; Assign
(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register
          machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp)
                machine
                labels))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))
(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))
(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;; Test, branch, goto
(define (make-test
         inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE"
               inst))))
(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch
         inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE"
               inst))))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto
         inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label
                   labels
                   (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg
                                 dest))))
             (lambda ()
               (set-contents! pc
                              (get-contents reg)))))
          (else
           (error "Bad GOTO instruction -- ASSEMBLE"
                  inst)))))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;; Others
(define (make-save inst machine stack pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform
         inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error
         "Bad PERFORM instruction -- ASSEMBLE"
         inst))))
(define (perform-action inst)
  (cdr inst))

;; Procedures for subexpressions
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label
                 labels
                 (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register
                   machine
                   (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error
          "Unknown expression type -- ASSEMBLE"
          exp))))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (register-exp? exp)
  (tagged-list? exp 'reg))
(define (register-exp-reg exp)
  (cadr exp))
(define (constant-exp? exp)
  (tagged-list? exp 'const))
(define (constant-exp-value exp)
  (cadr exp))
(define (label-exp? exp)
  (tagged-list? exp 'label))
(define (label-exp-label exp)
  (cadr exp))

(define (make-operation-exp
         exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp)
                         operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (let ((operands (cdr operation-exp)))
    (for-each
     (lambda (op)
       (if (not (or (constant-exp? op)
                    (register-exp? op)))
           (error
            "Operation operands must be constants or registers -- ASSEMBLE" operation-exp)))
     operands)
    operands))
(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE"
               symbol))))

;; LISP Machine Primitives
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
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
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp)
  (cadr exp))
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      false))
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp)
  (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-else-predicate? clause)
  (eq? clause 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond-first-clause-predicate clauses)
  (caar clauses))
(define (cond-first-clause-actions clauses)
  (cdar clauses))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
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
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq)
  (cons 'begin seq))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
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
(define (assignment? exp)
  (tagged-list? exp 'set!))
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
(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (let? exp)
  (tagged-list? exp 'let))
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
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp)
  (cdr exp))
(define (last-exp? seq)
  (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (application? exp)
  (pair? exp))

(define (make-procedure params body env)
  (list 'procedure params body env))
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
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
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define the-empty-environment '())
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define (make-frame vars vals) (cons vars vals))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied"
                 vars vals)
          (error "Too few arguments supplied"
                 vars vals))))
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '= =)
        (list '> >)
        (list '< <)
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
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))
(define (get-global-environment) the-global-environment)

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

(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))
(define (last-operand? ops)
  (null? (cdr ops)))
(define (true? x)
  (not (eq? x false)))

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

(define lisp-machine
  (make-machine
   (list (list 'read read)
         ;; (list '+ +)
         (list 'null? null?)
         ;; (list 'pair? pair?)
         ;; (list 'car car)
         ;; (list 'cdr cdr)
         (list 'cond-first-clause-predicate
               cond-first-clause-predicate)
         (list 'cond-first-clause-actions
               cond-first-clause-actions)
         (list 'self-evaluating? self-evaluating?)
         (list 'variable? variable?)
         (list 'lookup-variable-value lookup-variable-value)
         (list 'quoted? quoted?)
         (list 'text-of-quotation text-of-quotation)
         (list 'true? true?)
         (list 'if? if?)
         (list 'if-predicate if-predicate)
         (list 'if-consequent if-consequent)
         (list 'if-alternative if-alternative)
         (list 'cond? cond?)
         (list 'cond->if cond->if)
         (list 'cond-clauses cond-clauses)
         (list 'cond-actions cond-actions)
         (list 'cond-predicate cond-predicate)
         (list 'cond-else-clause? cond-else-clause?)
         (list 'cond-else-predicate? cond-else-predicate?)
         (list 'let? let?)
         (list 'let->combination let->combination)
         (list 'definition? definition?)
         (list 'definition-value definition-value)
         (list 'definition-variable definition-variable)
         (list 'define-variable! define-variable!)
         (list 'assignment? assignment?)
         (list 'assignment-value assignment-value)
         (list 'assignment-variable assignment-variable)
         (list 'set-variable-value! set-variable-value!)
         (list 'lambda? lambda?)
         (list 'lambda-parameters lambda-parameters)
         (list 'lambda-body lambda-body)
         (list 'begin? begin?)
         (list 'begin-actions begin-actions)
         (list 'last-exp? last-exp?)
         (list 'rest-exps rest-exps)
         (list 'first-exp first-exp)
         (list 'application? application?)
         (list 'primitive-procedure? primitive-procedure?)
         (list 'apply-primitive-procedure
               apply-primitive-procedure)
         (list 'primitive-implementation
               primitive-implementation)
         (list 'procedure-parameters procedure-parameters)
         (list 'procedure-body procedure-body)
         (list 'procedure-environment procedure-environment)
         (list 'compound-procedure? compound-procedure?)
         (list 'make-procedure make-procedure)
         (list 'operands operands)
         (list 'operator operator)
         (list 'no-operands? no-operands?)
         (list 'rest-operands rest-operands)
         (list 'first-operand first-operand)
         (list 'extend-environment extend-environment)
         (list 'get-global-environment get-global-environment)
         (list 'empty-arglist empty-arglist)
         (list 'adjoin-arg adjoin-arg)
         (list 'last-operand? last-operand?)
         (list 'user-print user-print)
         (list 'announce-output announce-output)
         (list 'prompt-for-input prompt-for-input)
         (list 'print-stack-statistics print-stack-statistics)
         )
   '((goto (label read-eval-print-loop))
     eval-dispatch
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op variable?) (reg exp))
     (branch (label ev-variable))
     (test (op quoted?) (reg exp))
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp))
     (branch (label ev-assignment))
     (test (op definition?) (reg exp))
     (branch (label ev-definition))
     (test (op if?) (reg exp))
     (branch (label ev-if))
     (test (op cond?) (reg exp))
     (branch (label ev-cond))
     (test (op let?) (reg exp))
     (branch (label ev-let))
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type))

     ;; Simple expressions
     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))

     ev-variable
     (assign val
             (op lookup-variable-value)
             (reg exp) (reg env))
     (goto (reg continue))

     ev-quoted
     (assign val (op text-of-quotation) (reg exp))
     (goto (reg continue))

     ev-lambda
     (assign unev (op lambda-parameters) (reg exp))
     (assign exp (op lambda-body) (reg exp))
     (assign val (op make-procedure)
             (reg unev) (reg exp) (reg env))
     (goto (reg continue))

     ;; Evaluating procedure applications
     ev-application
     (save continue)
     (save env)
     (assign unev (op operands) (reg exp))
     (save unev)
     (assign exp (op operator) (reg exp))
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))

     ev-appl-did-operator
     (restore unev)
     (restore env)
     (assign argl (op empty-arglist))
     (assign proc (reg val))
     (test (op no-operands?) (reg unev))
     (branch (label apply-dispatch))
     (save proc)

     ev-appl-operand-loop
     (save argl)
     (assign exp (op first-operand) (reg unev))
     (test (op last-operand?) (reg unev))
     (branch (label ev-appl-last-arg))
     (save env)
     (save unev)
     (assign continue (label ev-appl-accumulate-arg))
     (goto (label eval-dispatch))

     ev-appl-accumulate-arg
     (restore unev)
     (restore env)
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (assign unev (op rest-operands) (reg unev))
     (goto (label ev-appl-operand-loop))

     ev-appl-last-arg
     (assign continue (label ev-appl-accum-last-arg))
     (goto (label eval-dispatch))

     ev-appl-accum-last-arg
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (restore proc)
     (goto (label apply-dispatch))

     ;; Procedure application
     apply-dispatch
     (test (op primitive-procedure?) (reg proc))
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))
     (branch (label compound-apply))
     (goto (label unknown-procedure-type))

     primitive-apply
     (assign val
             (op apply-primitive-procedure)
             (reg proc)
             (reg argl))
     (restore continue)
     (goto (reg continue))

     compound-apply
     (assign unev (op procedure-parameters) (reg proc))
     (assign env (op procedure-environment) (reg proc))
     (assign env (op extend-environment)
             (reg unev) (reg argl) (reg env))
     (assign unev (op procedure-body) (reg proc))
     (goto (label ev-sequence))

     ;; Sequence Evaluation and Tail Recursion
     ev-begin
     (assign unev (op begin-actions) (reg exp))
     (save continue)
     (goto (label ev-sequence))

     ev-sequence
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue (label ev-sequence-continue))
     (goto (label eval-dispatch))

     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-sequence))

     ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))

     ;; Conditionals
     ev-if
     (save exp)
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     (goto (label eval-dispatch))
     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))
     ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))
     ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))

     ;; Exercise 5.23
     ;; Extend evaluator to handle derived expressions,
     ;; using syntax transformers.
     ;; ev-cond
     ;; (assign exp (op cond->if) (reg exp))
     ;; (goto (label eval-dispatch))

     ev-let
     (assign exp (op let->combination) (reg exp))
     (goto (label eval-dispatch))

     ;; Exercise 5.24
     ;; Implement cond without syntax transform
     ev-cond
     (save continue)  ; save continue for ev-sequence
     (assign unev (op cond-clauses) (reg exp))
     ev-cond-loop
     (test (op null?) (reg unev))     ; no more clauses
     (branch (label ev-cond-unspec))
     (assign exp (op cond-first-clause-predicate) (reg unev))
     (test (op cond-else-predicate?) (reg exp))
     (branch (label ev-cond-true))
     (save unev)      ; save clauses
     (save env)       ; and env to evaluate each clause
     (assign continue (label ev-cond-decide))
     (goto (label eval-dispatch))     ; eval first-predicate
     ev-cond-decide
     (restore env)              ; get the env
     (restore unev)             ; get the cluases
     (test (op true?) (reg val)); if predicate evluates to true
     (branch (label ev-cond-true))    ; goto ev-cond-true
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-cond-loop))
     ev-cond-true   ; we found a true cluase same as before
     (assign unev (op cond-first-clause-actions) (reg unev))
     (goto (label ev-sequence)) ; go to ev-sequence.
     ;; if there was no else, and no true clause
     ;; you could also leave val to be false here.
     ;; But in the implementations I tested
     ;; it was unspecified, or void.
     ev-cond-unspec
     (assign val (const 'unspecified)) ; assign val unspecified
     (restore continue)               ; go directly to caller.
     (goto (reg continue))




     ;; Definitions & Assignment
     ev-definition
     (assign unev (op definition-variable) (reg exp))
     (save unev) ; save variable for later
     (assign exp (op definition-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     (goto (label eval-dispatch)) ; evaluate the defintion val
     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op define-variable!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))

     ev-assignment
     (assign unev (op assignment-variable) (reg exp))
     (save unev) ; save variable for later
     (assign exp (op assignment-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-assignment-1))
     (goto (label eval-dispatch)) ; evaluate the assignment val
     ev-assignment-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op set-variable-value!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))

     ;; REPL
     read-eval-print-loop
     (perform (op initialize-stack))
     (perform (op prompt-for-input)
              (const ";;; EC-Eval input:"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))
     print-result
     (perform (op print-stack-statistics))
     (perform (op announce-output)
              (const ";;; EC-Eval value:"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))

     ;; Error handling
     unknown-expression-type
     (assign val (const unknown-expression-type-error))
     (goto (label signal-error))

     unknown-procedure-type
     (restore continue); to clean up stack from apply-dispatch
     (assign val (const unknown-procedure-type-error))
     (goto (label signal-error))

     signal-error
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
     )))


(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))
(append '(a b c) '(d e f))
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))
(factorial 5)
;; => (total-pushes = 144 maximum-depth = 28)
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))
(factorial 5)
;; => (total-pushes = 204 maximum-depth = 10)

;; Exercise 5.27
;;            Max Depth   |   No. of pushes
;; =========================================
;; Recursive     5n + 3   |      32n - 16
;; -----------------------------------------
;; Iterative     10       |      35n + 29
;;==========================================

;; Exercise 5.28
;; Use non-tail recursive eval-sequence
;;            Max Depth   |   No. of pushes
;; =========================================
;; Recursive    8n + 3    |    34n - 16
;; -----------------------------------------
;; Iterative    3n + 14   |    37n + 33
;;==========================================
