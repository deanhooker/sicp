;; Exercise 3.38 - Balance starts $100
;; Peter: (set! balance (+ balance 10))
;; Paul:  (set! balance (- balance 20))
;; Mary:  (set! balance (- balance (/ balance 2)))

;;a) List all possible values of balance if
;; run sequentially
;; Peter->Paul->Mary = $45
;; Peter->Mary->Paul = $35
;; Paul->Peter->Mary = $45
;; Paul->Mary->Peter = $50
;; Mary->Peter->Paul = $40
;; Mary->Paul->Peter = $40

;;b) If processes could be interleaved

;; Exercise 3.39
(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))

;; Answers could be:
;; 101: P1 sets x to 100 and then P2 increments x to 101.
;; 121: P2 increments x to 11 and then P1 sets x to x times x.
;; 110: P2 changes x from 10 to 11 between the two times that P1
;;      accesses the value of x during the evaluation of (* x x)
;;  11: P2 accesses x, then P1 sets x to 100, then P2 sets x
;; 100: P1 accesses x (twice) then P2 sets x to 11, then P1 sets x

(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (+ x 1)))))
;; Answers can be: 101 or 121

(define s (make-serializer))
(parallel-execute (lambda () (set! x ((s (lambda ()
                                           (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))
;; Answers can be: 101, 121 or 100

;; Exercise 3.40
(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

;; 1000000: P1 then P2 or P2 then P1
;; 100000: P2 gets one x as 10, rest as 100
;; 10000: P2 gets two x as 10, one as 100
;; 10000: P1 gets one x as 10, one as 1000
;; 100: P1 overrides P2
;; 1000: P2 overrides P1

(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))

;; 100000

;; Exercise 3.41
;; Ben Bitdiddle suggests withdraw, deposit and balance
;; procedures on an account should all be serialized

;; Balance doesn't update the balance state so it doesn't
;; need to be protected. There could be a state when trying to
;; return the value of balance while being written?

;; Exercise 3.42
;; Ben Bitdiddle suggests another change when doesn't really
;; change anything. The account still has one serializer.

;; Exercise 3.43
;; 3 accounts with $10, $20 and $30 use an exchange procedure
;; which reads the balance of two accounts, calculates the difference
;; and uses withdraw/deposit to change the amounts of the accounts
;; to the other.

;; If exchange is always sequential the accounts can only ever have
;; $10, $20 and $30, in any order.

;; If exchange allows the initial balance reads to be unserialized
;; from the deposits then we could end up with difference numbers
;; that still all add to the original, such as $20, $20 and $40.

;; Exercise 3.44
;; A transfer procedure:
(define (transfer to-account from-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))
;; does not require extra guard than withdraw and deposit to be
;; serialized.

;; The amount value is not the state of some object, so it is
;; different from exchange, where the balances are read and
;; used to calculate the amount, then set. There are multiple
;; points in time where we are hoping the state (balance) does
;; not change for exchange, but not for transfer.

;; Exercise 3.45
;; In the example the serializer will be applied twice to deposit
;; or withdraw in the serialized-exchange example. i.e. deadlock

;; Exercise 3.46
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (clear! cell)
  (set-car! cell false))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ;retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

;; Using test-and-set! as defined as just a procedure without
;; a hardware approach runs the same mutable state problem as the
;; bank account. Two processes can acquire the mutex at the same
;; time.

;; Two processes using test-and-set! could return false for the
;; car of the cell test, before one sets the value to true.

;; Exercise 3.47
;; A semaphore is a generalization of a mutex allowing up to n
;; processes to acquire it.

;; a)
(define (make-semaphore n)
  (define (iter semaphore i)
    (if (= i 0)
        semaphore
        (iter (cons (make-mutex) semaphore)
              (- i 1))))
  (let ((mutexes (iter '() n)))
    (define (try sem)
      (cond ((null? (car sem)) (try mutexes))
            ((not (caar sem)) ((car sem) 'acquire))
            (else (try (cdr sem)))))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (try mutexes))
            ((eq? m 'release) (clear! cell))))
    the-semaphore))
