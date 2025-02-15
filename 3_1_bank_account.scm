;; Example 1: Global balance state variable
(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

;; Example 2: Local balance state variable
;; balance is ENCAPSULATED
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

;; Example 3: Two separate account objects
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

;; W1 and W2 are completely independent
;; (W1 50)
;; => 50

;; (W2 70)
;; => 30

;; Example 4: Object with deposits and withdrawals
;; Note: this uses the message-passing style of
;; programming we saw in the data abstraction
;; section
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE ACCOUNT" m))))
  dispatch)

(define acc (make-account 100))

;; ((acc 'withdraw) 50)
;; => 50
;; ((acc 'deposit) 50)
;; => 100

;; Exercise 3.1
;; Create an accumulator with state
(define (make-accumulator value)
  (lambda (val-to-add)
    (set! value (+ value val-to-add))
    value))

;; Exercise 3.2
;; Make a procedure which creates a "monitored
;; procedure" with methods to return how many
;; times a procedure has been called, or reset.
(define (make-monitored f)
  (let ((counter 0))
    (define (apply x)
      (set! counter (+ counter 1))
      (f x))
    (define (dispatch m)
      (cond ((eq? 'how-many-calls? m) counter)
            ((eq? 'reset-count m)
             (set! counter 0)
             'reset!)
            (else (apply m))))
    dispatch))

;; Exercise 3.3
;; Alter make-account so it takes a password
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw m)
    (cond ((not (eq? pw password))
           (lambda (x) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE ACCOUNT" m))))
  dispatch)

;; Exercise 3.4
;; Alter make-account so if 7 consecutive wrong
;; passwords are supplied it "calls the cops"
(define (make-account balance password)
  (let ((counter 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (try-password pw m)
      (cond ((eq? pw password)
             (set! counter 0)
             (dispatch m))
            ((> counter 6)
              (call-the-cops))
            (else  (set! counter (+ counter 1))
                   (lambda (x)
                     "Incorrect password"))))
    (define (call-the-cops)
      (lambda (x) 'call-the-cops!!!))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE ACCOUNT" m))))
    try-password))

;; Exercise 3.7
;; Alter make-account to create an alias of an account
;; with a new password.

(define (make-joint account old-password new-password)
  (lambda (password m)
    (if (eq? new-password password)
        (account old-password m)
        'oops)))

;; Exercise 3.8
;; Define a procedure such that
;; (+ (f 0) (f 1)) evaluates to 0 if + is
;; evaluated left to right, or 1 if + is
;; evaluated right to left.
(define f
  (let ((r 0))
    (lambda (x)
      (set! r x))))

;; set! returns the original value
