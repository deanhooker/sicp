((env) (val)
 ((assign val (op make-compiled-procedure)
          (label entry54) (reg env))
  (goto (label after-lambda53))

  entry54
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
  (branch (label primitive-branch69))

  compiled-branch68
  (assign continue (label after-call67))
  (assign val (op compiled-procedure-entry)
          (reg proc))
  (goto (reg val))

  primitive-branch69
  (assign val (op apply-primitive-procedure)
          (reg proc) (reg argl))

  after-call67
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch56))

  true-branch57
  (assign val (const 1))
  (goto (reg continue))

  false-branch56
  (assign proc (op lookup-variable-value)
          (const *) (reg env))
  (save continue)
  (save proc)
  (save env)
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
  (branch (label primitive-branch60))

  compiled-branch59
  (assign continue (label after-call58))
  (assign val (op compiled-procedure-entry)
          (reg proc))
  (goto (reg val))

  primitive-branch60
  (assign val (op apply-primitive-procedure)
          (reg proc) (reg argl))

  after-call58
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch63))

  compiled-branch62
  (assign continue (label after-call61))
  (assign val (op compiled-procedure-entry)
          (reg proc))
  (goto (reg val))

  primitive-branch63
  (assign val (op apply-primitive-procedure)
          (reg proc) (reg argl))

  after-call61
  (assign argl (op list) (reg val))
  (restore env)
  (assign val (op lookup-variable-value)
          (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch66))

  compiled-branch65
  (assign val (op compiled-procedure-entry)
          (reg proc))
  (goto (reg val))

  primitive-branch66
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))

  after-call64
  after-if55
  after-lambda53
  (perform (op define-variable!)
           (const factorial) (reg val) (reg env))
  (assign val (const ok))))
