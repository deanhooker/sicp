((env) (val)
 (;; Construct procedure and skip over code
  ;; for procedure body
  (assign val (op make-compiled-procedure)
          (label entry71) (reg env))
  (goto (label after-lambda70))

  ;; Calls to factorial will enter here
  entry71
  (assign env (op compiled-procedure-environment)
          (reg proc))
  (assign env (op extend-environment)
          (const (n)) (reg argl) (reg env))

  ;; Construct iter procedure and skip over
  ;; code for procedure body
  (assign val (op make-compiled-procedure)
          (label entry76) (reg env))
  (goto (label after-lambda75))

  ;; Calls to iter will enter here
  entry76
  (assign env (op compiled-procedure-environment)
          (reg proc))
  (assign env (op extend-environment)
          (const (product counter))
          (reg argl) (reg env))

  ;; Begin procedure body
  (save continue)
  (save env)

  ;; Compute (> counter n)
  (assign proc (op lookup-variable-value)
          (const >) (reg env))
  (assign val (op lookup-variable-value)
          (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value)
          (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  ;; proc is primitive: >
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch91))

  ;; This branch will never execute
  compiled-branch90
  (assign continue (label after-call89))
  (assign val (op compiled-procedure-entry)
          (reg proc))
  (goto (reg val))

  ;; This branch always executes
  primitive-branch91
  (assign val (op apply-primitive-procedure)
          (reg proc) (reg argl))

  after-call89
  (restore env)
  (restore continue)
  ;; Do the test (> counter n)
  (test (op false?) (reg val))
  (branch (label false-branch78))

  true-branch79
  (assign val (op lookup-variable-value)
          (const product) (reg env))
  (goto (reg continue))

  ;; If (> counter n) is false:
  false-branch78
  ;; Lookup iter procedure
  (assign proc (op lookup-variable-value)
          (const iter) (reg env))
  ;; Save registers
  (save continue)
  (save proc)
  (save env)
  ;; Do (+ counter 1)
  (assign proc (op lookup-variable-value)
          (const +) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value)
          (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch85))

  ;; This will always be skipped
  compiled-branch84
  (assign continue (label after-call83))
  (assign val (op compiled-procedure-entry)
          (reg proc))
  (goto (reg val))

  ;; This is always be true (+ counter 1)
  primitive-branch85
  (assign val (op apply-primitive-procedure)
          (reg proc) (reg argl))

  after-call83
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  ;; Do (* counter product)
  (assign proc (op lookup-variable-value)
          (const *) (reg env))
  (assign val (op lookup-variable-value)
          (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value)
          (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch82))

  ;; Skipped over
  compiled-branch81
  (assign continue (label after-call80))
  (assign val (op compiled-procedure-entry)
          (reg proc))
  (goto (reg val))

  ;; This will always execute (* counter product)
  primitive-branch82
  (assign val (op apply-primitive-procedure)
          (reg proc) (reg argl))

  after-call80
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  ;; Restore iter proc
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch88))

  ;; Goto iter procedure
  compiled-branch87
  (assign val (op compiled-procedure-entry)
          (reg proc))
  (goto (reg val))

  ;; Never executes
  primitive-branch88
  (assign val (op apply-primitive-procedure)
          (reg proc) (reg argl))
  (goto (reg continue))

  after-call86
  after-if77
  after-lambda75
  ;; Define iter
  (perform (op define-variable!)
           (const iter) (reg val) (reg env))
  (assign val (const ok))
  ;; Apply iter
  (assign proc (op lookup-variable-value)
          (const iter) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch74))

  ;; Initial call to iter
  compiled-branch73
  (assign val (op compiled-procedure-entry)
          (reg proc))
  (goto (reg val))

  ;; Skipped
  primitive-branch74
  (assign val (op apply-primitive-procedure)
          (reg proc) (reg argl))
  (goto (reg continue))

  after-call72
  after-lambda70
  (perform (op define-variable!)
           (const factorial) (reg val) (reg env))
  (assign val (const ok))))
