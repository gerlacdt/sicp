#lang racket
(require racket/trace)
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


'ch3-done
