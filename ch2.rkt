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

'ch2-done


