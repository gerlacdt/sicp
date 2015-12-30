#lang racket
(require racket/trace)
(require compatibility/mlist)
(require "ch1.rkt")

(provide (all-defined-out))

;; ex 3.3 and 3.4
(define (make-account balance password)
  (let ((password-counter 0))
    (define (reset-password-counter)
      (set! password-counter 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (incorrect-password . rst)
      (set! password-counter (+ 1 password-counter))
      (if (< password-counter 3)
          "Incorrect password"
          "call-the-cops!!"))
    (define (dispatch m given-password)
      (if (equal? password given-password)
          (begin
            (reset-password-counter)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  ((eq? m 'password-counter) password-counter)
                  (else (error "Unknown request -- MAKE-ACCOUNT" m))))
          incorrect-password))
    dispatch))

(define W1 (make-account 100 'secret))
(define W2 (make-account 1000 'secret))


;; ex 3.1
(define (make-accumulator sum)
  (lambda (n)
    (set! sum (+ n sum))
    sum))

(define acc (make-accumulator 5))

;; ex 3.2
(define (make-monitored f)
  (let ((number-of-calls 0))
    (define (call x)
      (set! number-of-calls (+ 1 number-of-calls))
      (f x))
    (define (how-many-calls)
      number-of-calls)
    (define (reset-count)
      (set! number-of-calls 0)
      number-of-calls)
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls) (how-many-calls))
            ((eq? m 'reset-count) (reset-count))
            (else (call m))))
    dispatch))

(define monitored-square (make-monitored square))


;; estimate pi


(define (rand m)
  (cond ((eq? m 'generate) (random 10000))
        ((eq? m 'reset) 0)
        (else "Wrong input. Use generate or reset..")))


(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (my-gcd (random 10000) (random 10000)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


;; ex 3.17
(define (count-pair x)
  (let ((visited '()))
    (define (iter x)
      (cond ((not (pair? x)) 0)
            ((memq visited x) 0)
            (else
             (begin (set! visited (cons visited x)))
             (+ 1
                (iter (car x))
                (iter (cdr x))))))
    (iter x)))

;; 3.3.2  representing queues

(define (front-ptr queue)
  (mcar queue))

(define (rear-ptr queue)
  (mcdr queue))

(define (set-front-ptr! queue item)
  (set-mcar! queue item))

(define (set-rear-ptr! queue item)
  (set-mcdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue)
  (mcons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with empty queue" queue)
      (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with empty queue" queue))
        (else
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         queue)))

;; ex. 3.21

(define (print-queue queue)
  (cond ((empty-queue? queue)
         '())
        (else
         (mlist->list (mcar queue)))))

;; ex. 3.22

(define (make-queue-dispatch)
  (let* ((queue (mcons '() '())))
    ;; internal procedures
    (define (front-ptr)
      (mcar queue))
    (define (rear-ptr)
      (mcdr queue))
    (define (set-front-ptr! item)
      (set-mcar! queue item))
    (define (set-rear-ptr! item)
      (set-mcdr! queue item))
    (define (empty-queue?)
      (null? (front-ptr)))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with empty queue")
          (mcar (front-ptr))))
    (define (insert-queue! item)
      (let ((new-pair (mcons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               queue)
              (else
               (set-mcdr! (rear-ptr) new-pair)
               (set-rear-ptr! new-pair)
               queue))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with empty queue"))
            (else
             (set-front-ptr! (mcdr (front-ptr)))
             queue)))
    (define (print-queue)
      (cond ((empty-queue?)
             '())
            (else
             (mlist->list (mcar queue)))))
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr!)
            ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print-queue)
            (else
             (error "Unknown request -- MAKE-QUEUE-DISPATCH"))))
    dispatch))

;; ex. 3.27

(define (memoize f)
  (let ((table (make-hash)))
    (lambda (x)
      (let ((previously-computed-result (hash-ref table x #f)))
        (or previously-computed-result
            (let ((result  (f x)))
              (hash-set! table x result)
              result))))))

(define memo-fib (memoize (lambda (n)
                            (cond ((= n 0) 0)
                                  ((= n 1) 1)
                                  (else (+ (memo-fib (- n 1))
                                           (memo-fib (- n 2))))))))

;; concurrency
(define (parallel-execute . procs)
  (map thread-wait
       (map (lambda (proc) (thread proc))
            procs)))

(parallel-execute (lambda () (displayln "hello daniel"))
                  (lambda () (displayln "thread2")))




;; streams 3.5

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define-syntax-rule (my-delay exp)
  (memo-proc (lambda () exp)))

(define (my-force delayed-object)
  (delayed-object))

(define-syntax-rule (cons-stream a b)
  (cons a (my-delay b)))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (my-force (cdr stream)))

(define the-empty-stream '())

(define (stream-null? stream)
  (null? stream))

(define (my-stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (my-stream-ref (stream-cdr s) (- n 1))))

(define (my-stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (my-stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each displayln s))

(define (my-stream->list s)
  (if (stream-null? s)
      '()
      (cons (stream-car s) (my-stream->list (stream-cdr s)))))

(define (my-stream-take s n)
  (if (or (stream-null? s) (= n 0))
      '()
      (cons (stream-car s) (my-stream-take (stream-cdr s) (- n 1)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (second-prime-range from to)
  (stream-car (stream-cdr
               (stream-filter prime?
                              (stream-enumerate-interval from to)))))

;; ex 3.50
(define (my-stream-map-multi-args proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply my-stream-map-multi-args (cons proc (map stream-cdr argstreams))))))

;; ex 3.51
(define (show x)
  (displayln x)
  x)

(define x (my-stream-map show (stream-enumerate-interval 0 10)))
(my-stream-ref x 5)
(my-stream-ref x 7)

;; ex 3.52
(define ex-sum 0)
(define (accum x)
  (set! ex-sum (+ x ex-sum))
  ex-sum)

(define seq (my-stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))

(my-stream-ref y 7)
(display-stream z)

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y)
  (= (remainder x y) 0))

(define (no-sevens)
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(define (sevens)
  (stream-filter (lambda (x) (divisible? x 7))
                 integers))


(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))


(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from)))


'ch3-done
