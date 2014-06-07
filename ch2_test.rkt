#lang racket
(require rackunit) 
(require rackunit/text-ui)
(require "ch2.rkt")

(define sicp-ch2-tests
  (test-suite
   "sicp chapter 2 test suite"
   (test-case
    "test my-map"
    (check-equal? (my-map (lambda (x) (* x x)) '(1 2 3 4)) '(1 4 9 16)))

   (test-case
    "test horner formula"
    (check-equal? 79 (horner-eval 2 (list 1 3 0 5 0 1))))))

(run-tests sicp-ch2-tests)

