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

;; Exercises
;; ===================================
(define gcd-machine
  (make-machine
   (list (list 'rem remainder) (list '= =))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

;; Exercise 5.7
(define expt-rec-machine
  (make-machine
   (list (list '= =)
         (list '- -)
         (list '* *))
   '(main
     (assign continue (label expt-done))
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     (save continue)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-expt))
     (goto (label expt-loop))
     after-expt
     (restore continue)
     (assign val (op *) (reg b) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     expt-done)))

(define expt-iter-machine
  (make-machine
   (list (list '= =)
         (list '- -)
         (list '* *))
   '(main
     (assign counter (reg n))
     (assign product (const 1))
     expt-iter
     (test (op =) (reg counter) (const 0))
     (branch (label base-case))
     (assign counter (op -) (reg counter) (const 1))
     (assign product (op *) (reg b) (reg product))
     (goto (label expt-iter))
     base-case
     (assign val (reg product))
     expt-done)))

;; Exercise 5.8
;; Prevent labels from being used twice.
;; Added a check if in extract-labels, if next label
;; is a symbol, check whether that symbol has
;; already been seen.

;; Exercise 5.9
;; Operations should only allow constant or register
;; operands.
;; Added a check when extracting operands from the
;; expression to check each is either a register
;; or constant

;; Exercise 5.10
;; Define a new syntax

;; Exercise 5.11
;; (save y)
;; (save x)
;; (restore y)

;; How does the above behave? In our implementation
;; restore takes the last value added to the stack,
;; no matter where it came from. There is no
;; labeling in the stack.

;; a) If we always just restore the last value we
;;    can remove a restore from the fib function

;; b) We can have restore throw an error if the
;;    thing we are trying to restore to a register
;;    wasn't saved from that register.

;; c) We can have a separate stack for each register

;; Exercise 5.12
(define expt-iter-machine
  (make-machine
   (list (list '= =)
         (list '- -)
         (list '* *))
   '(main
     (assign counter (reg n))
     (assign product (const 1))
     expt-iter
     (test (op =) (reg counter) (const 0))
     (branch (label base-case))
     (assign counter (op -) (reg counter) (const 1))
     (assign product (op *) (reg b) (reg product))
     (goto (label expt-iter))
     base-case
     (assign val (reg product))
     expt-done)))

;; a)
(get-instruction-list expt-iter-machine)

;; b)


;; Exercise 5.13
;; TODO: Edit the machine to use the controller
;; to create the list of registers for the machine
;; rather than requiring them as input.

;; Monitoring Machine Performance
;; ==============================
(define expt-rec-machine
  (make-machine
   (list (list '= =)
         (list '- -)
         (list '* *))
   '(main
     (assign continue (label expt-done))
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     (save continue)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-expt))
     (goto (label expt-loop))
     after-expt
     (restore continue)
     (assign val (op *) (reg b) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     expt-done)))

(set-register-contents! expt-rec-machine 'b 3)
(set-register-contents! expt-rec-machine 'n 4)
(start expt-rec-machine)
(get-register-contents expt-rec-machine 'val)
;; => 81
((expt-rec-machine 'stack) 'print-statistics)
;; => (total-pushes = 4 maximum-depth = 4)
;;    ;Unspecified return value


;; Exercise 5.21
;; a)
(define count-leaves-machine
  (make-machine
   (list (list '+ +) (list 'null? null?)
         (list 'pair? pair?) (list 'car car)
         (list 'cdr cdr))
   '((assign continue (label count-leaves-done))
     (assign val (const 0))
     tree-loop
     (test (op null?) (reg tree))
     (branch (label null-tree))
     (test (op pair?) (reg tree))
     (branch (label left-tree))
     (assign val (const 1))
     (goto (reg continue))
     left-tree
     (save tree)
     (save continue)
     (assign continue (label right-tree))
     (assign tree (op car) (reg tree))
     (goto (label tree-loop))
     right-tree
     (restore continue)
     (restore tree)
     (save continue)
     (save val)
     (assign continue (label after-tree))
     (assign tree (op cdr) (reg tree))
     (goto (label tree-loop))
     after-tree
     (assign var (reg val))
     (restore val)
     (restore continue)
     (assign val (op +) (reg var) (reg val))
     (goto (reg continue))
     null-tree
     (assign val (const 0))
     (goto (reg continue))
     count-leaves-done)))

(set-register-contents!
 count-leaves-machine 'tree '(a (b c (d)) (e f) g))
(start count-leaves-machine)
(get-register-contents count-leaves-machine 'val)

(define count-leaves-machine
  (make-machine
   (list (list '+ +) (list 'null? null?)
         (list 'pair? pair?) (list 'car car)
         (list 'cdr cdr))
   '(
     start
     (assign val (const 0))
     (assign continue (label done))
     (save continue)
     (assign continue (label cdr-loop))
     count-loop
     (test (op pair?) (reg lst))
     (branch (label pair))
     (test (op null?) (reg lst))
     (branch (label null))
     (assign val (op +) (reg val) (const 1))
     (restore continue)
     (goto (reg continue))
     cdr-loop
     (restore lst)
     (assign lst (op cdr) (reg lst))
     (goto (label count-loop))
     pair
     (save lst)
     (save continue)
     (assign lst (op car) (reg lst))
     (goto (label count-loop))
     null
     (restore continue)
     (goto (reg continue))
     done)))

(set-register-contents!
 count-leaves-machine 'tree
 ;; '(a (b c (d)) (e f) g)
 '(a (b) c)
 )
(start count-leaves-machine)
(get-register-contents count-leaves-machine 'val)
