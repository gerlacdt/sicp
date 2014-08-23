#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "../ch2_sym_diff.rkt")

(define symbolic-differentiation-tests
  (test-suite
   "sicp chapter 2, exercise 2.73"
   (test-case
    "some differentiations"
    (check-equal? 1 (deriv '(+ x 3) 'x))
    (check-equal? 'y (deriv '(* x y) 'x))
    (check-equal? '(+ (* x y) (* y (+ x 3))) (deriv '(* (* x y) (+ x 3)) 'x)))))


(run-tests symbolic-differentiation-tests)
