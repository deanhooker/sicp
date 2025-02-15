;; Queues
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error
          "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr!
          queue (cdr (front-ptr queue)))
         queue)))

;; Exercise 3.21
;; (define q1 (make-queue))

;; (insert-queue! q1 'a)
;; => ((a) a)
;; (insert-queue! q1 'b)
;; => ((a b) b)
;; (delete-queue! q1)
;; => ((b) b)
;; (delete-queue! q1)
;; => (() b)

;; One may be confused by the fact that the
;; cdr is still b. However this is just a pointer
;; to where b lives in memory, the queue itself
;; (represented as a list) is now empty.

;; We see this if we call delete-queue! again:

;; (delete-queue! q1)
;; => DELETE! called with an empty queue (() b)

(define (print-queue queue)
  (display (front-ptr queue)))

;; Exercise 3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))

    (define (queue)
      (cons front-ptr rear-ptr))

    (define (empty-queue?) (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               (queue))
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               (queue)))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error
              "DELETE! called with an empty queue"))
            (else
             (set-front-ptr! (cdr front-ptr))
             (queue))))

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))))
    dispatch))

(define (empty-queue? queue)
  (queue 'empty-queue?))

(define (front-queue queue)
  (queue 'front-queue))

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))

(define (delete-queue! queue)
  (queue 'delete-queue!))

;; Exercise 3.23 Deques (double-ended queues)

;; Deque is a pair of pointers
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front! deque item)
  (set-car! deque item))
(define (set-rear! deque item)
  (set-cdr! deque item))
(define (empty-deque? deque)
  (null? (front-ptr deque)))
(define (make-deque) (cons '() '()))

;; Get and insert from either end wrappers
(define (get-item deque end)
  (if (empty-deque? deque)
      (error "GET-ITEM called with an empty deque" end)
      (caar (end deque))))

(define (insert-deque! deque item end)
  ;; A new pair is '((item))
  (let ((new-pair (cons (cons item '()) '())))
    (cond
     ((empty-deque? deque)
      (begin
        (set-front! deque new-pair)
        (set-rear! deque new-pair)))
     ((eq? end 'front)
      (begin
        (set-cdr! new-pair (front-ptr deque))
        (set-cdr!
         (car (front-ptr deque)) new-pair)
        (set-front-ptr! deque new-pair)))
     ((eq? end 'rear)
      (begin
        (set-cdr! (rear-ptr deque) new-pair)
        (set-cdr! (car new-pair) (rear-ptr deque))
        (set-rear! deque new-pair))))))


 (define (front-delete-deque! deque)
   (cond ((empty-deque? deque)
          (error "Cannot delete from empty deque"
                 deque))
         (else
          (set-front! deque
                      (cdr (front-ptr deque)))
          (or (empty-deque? deque)
              (set-cdr! (car (front-ptr deque))
                        '())))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "Cannot delete from empty deque"
                 deque))
        (else
         (set-rear! deque
                    (cdar (rear-ptr deque)))
         (if (null? (rear-ptr deque))
             (set-front! deque '())
             (set-cdr! (rear-ptr deque)
                       '())))))

(define (front-insert-deque! deque item)
  (insert-deque! deque item 'front))
(define (rear-insert-deque! deque item)
  (insert-deque! deque item 'rear))
(define (front-deque deque)
  (get-item deque front-ptr))
(define (rear-deque deque)
  (get-item deque rear-ptr))

(define (print-deque d)
  (define (iter res _d)
    (if (or (null? _d) (empty-deque? _d))
        res
        (iter (append res (list (caaar _d)))
              (cons (cdar _d) (cdr d)))))
  (iter '() d))
