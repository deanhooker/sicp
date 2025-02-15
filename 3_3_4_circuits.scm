;; A simulator for Digital Circuits
;; Note: must load queue fns from 3_3_2

;; Primitives
;; Wires - (make-wire)
;; (get-signal <wire>)
;; (set-signal! <wire> <new value>)
;; (add-action! <wire> <procedure of no args>)

;; Components have delays; change in output occurs
;; a period of time after the input changes.

;; Representing Wires
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE"
                         m))))
    dispatch))

;; The following procedures are syntactic sugar,
;; allowing us to think of a wire as a data object
;; which we're passing as an argument to a
;; procedure.
;; However an expression such as (wire 'get-signal)
;; reveals that wire itself is a procedure.
;; In a language where we can deal with procedures
;; as objects there is really no distinction between
;; data and procedures. We can choose our syntactic
;; sugar to program in whichever style we choose.
(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-proc)
  ((wire 'add-action!) action-proc))

;; Components

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-and a b)
  (cond ((and (= a 1) (= b 1)) 1)
        ((or (= a 0) (= b 0)) 0)
        (else (error "Invalid signal a:" a "b:" b))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1)
                                  (get-signal a1))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;; Exercise 3.28
;; Define an or-gate as a primitive function box.
;; (should be similar to and-gate)
(define (logical-or a b)
  (cond ((or (= a 1) (= b 1)) 1)
        ((and (= a 0) (= b 0)) 0)
        (else (error "Invalid signal a:" a "b:" b))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1)
                                  (get-signal a1))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;; Exercise 3.29
;; Define an or-gate as a compound digital logic
;; device, built from and-gates and inverters.
(define (or-gate2 a1 a2 output)
  (let ((b1 (make-wire))
        (b2 (make-wire))
        (c (make-wire)))
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 c)
    (inverter c output)
    'ok))

;; Invert a1, a2, combine with and-gate, invert
;; output from and-gate.

;; In this case the or-delay is the inverter-
;; delay on either a1 or a2, plus and-delay, plus
;; another inverter-delay.

(define a (make-wire))
(define b (make-wire))
(define s (make-wire))
(define c (make-wire))

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))

;; Exercise 3.30
;; Ripple-carry adder
(define (ripple-carry-adder as bs ss c-out)
  (let ((c-in (make-wire)))
    (if (and (null? (cdr as))
             (null? (cdr bs))
             (null? (cdr ss)))
        (begin
          (full-adder (car as)
                      (car bs)
                      c-in
                      (car ss)
                      c-out)
          (set-signal! c-in 0)
          'ok)
        (begin
          (full-adder (car as)
                      (car bs)
                      c-in
                      (car ss)
                      c-out)
          (ripple-carry-adder (cdr as)
                              (cdr bs)
                              (cdr ss)
                              c-in)))))


;; The agenda
;; The only thing left to complete our
;; implementation is after-delay. We need to
;; maintain a data structure called an agenda.

;; (make-agenda)
;; (empty-agenda? <agenda>)
;; (first-agenda-item <agenda>)
;; (remove-first-agenda-item! <agenda>)
;; (add-to-agenda! <time> <action> <agenda>)
;; (current-time <agenda>)

(define (after-delay delay action)
  (add-to-agenda!
   (+ delay (current-time the-agenda))
   action
   the-agenda))

;; The simulation is driven by propagate, which
;; operates on the agenda if not empty.
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item
             (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;; A sample simulation
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))


;; Exercise 3.31
;; the accept-action-procedure! function defined
;; in make-wire has to run the procedure passed
;; otherwise the wires would be incorrectly
;; initialized (always 0). Creating an inverter
;; for an input wire with 0 would create the output
;; wire also with 0, which wouldn't change until
;; the input was changed from a 0 to 1 and back.

;; Implementing the agenda
;; The agenda is made up of "time segments": a pair
;; consisting of a number (the time) and a queue
;; of procedures to execute.
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue
                        (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time
                                            action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda
                       (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)

(propagate)

(set-signal! input-2 1)

(propagate)

;; Exercise 3.32
;; A queue must be used (FIFO) because changes
;; propagate forward. Input wires are set first
;; and output wires depend on their values.
;; Even if things happen "at the same time", there
;; still needs to be an order of precendence.
