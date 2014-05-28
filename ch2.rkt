#lang racket
(require racket/trace)
(require "ch1.rkt")

(provide (all-defined-out))

(define (make-rat n d)
  (let ((g (my-gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer x) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (displayln (format "~a/~a" (numer x) (denom x))))

(define (make-segment start end)
  (cons start end))

(define (startpoint-segment seg)
  (car seg))

(define (endpoint-segment seg)
  (cdr seg))

(define (midpoint-segment line)
  (define (avg-point start-p end-p)
    (make-point (average (x-point start-p) (x-point end-p)) (average (y-point start-p) (y-point end-p))))
  (avg-point (startpoint-segment line) (endpoint-segment line)))


(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (displayln (format "~a,~a" (x-point p) (y-point p))))

(define (test-midpoint)
  (let* ((startpoint (make-point 3 3))
         (endpoint (make-point 4 4))
         (line (make-segment startpoint endpoint)))
    (midpoint-segment line)))

(define (my-cons x y)
  (lambda (m) (m x y)))

(define (my-car z)
  (z (lambda (p q) p)))

(define (my-cdr z)
  (z (lambda (p q) q)))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (length-iter items)
  (define (iter items count)
    (if (null? items)
        count
        (iter (cdr items) (+ 1 count))))
  (iter items 0))

(define odds (list 1 3 5 7))

(define squares (list 1 4 9 16 25))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))


(define (reverse list1)
  (define (iter list1 result)
    (if (null? list1)
        result
        (iter (cdr list1) (cons (car list1) result))))
  (iter list1 null))

(define (same-parity x . z)
  (define (help list1 f)
    (cond ((null? list1) null)
          ((f (car list1)) (cons (car list1) (help (cdr list1) f)))
          (else (help (cdr list1) f))))
  (cond ((odd? x) (help (append (list x) z) odd?))
        (else (help (append (list x) z) even?))))

(define (my-map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (my-map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* factor x)) items))

(define (square-list items)
  (map square items))

(define (square-list-wrong items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons answer (square (car things))))))
  (iter items null))

(define (square-list-reverse items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things)) answer))))
  (iter items null))

(define (my-for-each proc items)
  (cond ((null? items) 'done)
        (else (proc (car items))
              (my-for-each proc (cdr items)))))

(define x (cons (list 1 2) (list 3 4)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (deep-reverse li)
  (cond ((null? x) null)
        ((not (pair? li)) li)
        (else (append (deep-reverse (cdr li))
                      (list (deep-reverse (car li)))))))

(define (deep-reverse-map list1)
  (if (pair? list1)
      (reverse (map deep-reverse-map list1))
      list1))

(define (fringe list1)
  (cond ((null? list1) null)
        ((not (pair? list1)) list1)
        (else (append (fringe (cdr list1))
                      (list (fringe (car list1)))))))

'ch2-done

