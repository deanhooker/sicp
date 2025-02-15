((env) (val)
 ((assign val (op make-compiled-procedure)
          (label entry37) (reg env))
  (goto (label after-lambda36))

  entry37
  (assign env (op compiled-procedure-environment)
          (reg proc))
  (assign env (op extend-environment)
          (const (n)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value)
          (const =) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value)
          (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch52))

  compiled-branch51
  (assign continue (label after-call50))
  (assign val (op compiled-procedure-entry)
          (reg proc))
  (goto (reg val))

  primitive-branch52
  (assign val (op apply-primitive-procedure)
          (reg proc) (reg argl))

  after-call50
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch39))

  true-branch40
  (assign val (const 1))
  (goto (reg continue))

  false-branch39
  (assign proc (op lookup-variable-value)
          (const *) (reg env))
  (save continue)
  (save proc)
  (assign val (op lookup-variable-value)
          (const n) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value)
          (const factorial) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value)
          (const -) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value)
          (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch43))

  compiled-branch42
  (assign continue (label after-call41))
  (assign val (op compiled-procedure-entry)
          (reg proc))
  (goto (reg val))

  primitive-branch43
  (assign val (op apply-primitive-procedure)
          (reg proc) (reg argl))

  after-call41
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch46))

  compiled-branch45
  (assign continue (label after-call44))
  (assign val (op compiled-procedure-entry)
          (reg proc))
  (goto (reg val))

  primitive-branch46
  (assign val (op apply-primitive-procedure)
          (reg proc) (reg argl))

  after-call44
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch49))

  compiled-branch48
  (assign val (op compiled-procedure-entry)
          (reg proc))
  (goto (reg val))

  primitive-branch49
  (assign val (op apply-primitive-procedure)
          (reg proc) (reg argl))
  (goto (reg continue))

  after-call47
  after-if38
  after-lambda36
  (perform (op define-variable!)
           (const factorial) (reg val) (reg env))
  (assign val (const ok))))
