#lang racket
(require racket/trace)
(require "ch2_complex.rkt")
(provide (all-defined-out))

;;; symbolic differentiation

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))


(define (install-deriv-package)
  ;; internal procedures
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (make-product m1 m2)
    (displayln m1)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (define (sum? x)
    (and (pair? x) (eq? (car x) '+)))

  (define (addend s)
    (cadr s))

  (define (augend s)
    (caddr s))

  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))

  (define (multiplier p)
    (cadr p))

  (define (multiplicand p)
    (caddr p))

  (define (exponantiation? exp)
    (and (pair? exp) (eq? (car exp) '**)))

  (define (make-exponantiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          (else (list '** base exponent))))

  (define (base exp)
    (cadr exp))

  (define (exponent exp)
    (caddr exp))


  ;; public interface
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (define (deriv-product exp var)
    (make-sum (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier exp) var)
                            (multiplicand exp))))
  
  (define (deriv-exponatiation exp var)
    (make-product (deriv (base exp) var)
                  (make-product (exponent exp)
                                (make-exponantiation (base exp)
                                                     (make-sum (exponent exp) -1)))))

  ;; install in public hashtable
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponatiation))

(install-deriv-package)


(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var)
                             1
                             0))
        (else ((get 'deriv (operator exp))
               exp
               var))))

