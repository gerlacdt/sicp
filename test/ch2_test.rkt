#lang racket
(require rackunit) 
(require rackunit/text-ui)
(require "../ch2.rkt")

(define sicp-ch2-tests
  (test-suite
   "sicp chapter 2 test suite"
   (test-case
    "test my-map"
    (check-equal? (my-map (lambda (x) (* x x)) '(1 2 3 4)) '(1 4 9 16)))

   (test-case
    "test horner formula"
    (check-equal? 79 (horner-eval 2 (list 1 3 0 5 0 1))))

   (test-case
    "test binary tree implentation")
   (let* ((tree (make-tree 5 (make-tree 3 (make-tree 1 null null) (make-tree 4 null null)) (make-tree 7 null null))))
     (check-equal? false (element-of-treeset? 2 tree))
     (check-equal? true (element-of-treeset? 5 tree))
     (check-equal? true (element-of-treeset? 7 tree))
     (check-equal? true (element-of-treeset? 1 tree))
     (check-equal? true (element-of-treeset? 4 tree)))))

(run-tests sicp-ch2-tests)

