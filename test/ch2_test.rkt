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
    "test binary tree implentation"
    (let* ((tree (make-tree 5 (make-tree 3 (make-tree 1 null null) (make-tree 4 null null)) (make-tree 7 null null)))
           (tree2 (adjoin-set 7 (adjoin-treeset 3 (make-tree 5 null null)))))
      (check-equal? false (element-of-treeset? 2 tree))
      (check-equal? true (element-of-treeset? 5 tree))
      (check-equal? true (element-of-treeset? 7 tree))
      (check-equal? true (element-of-treeset? 1 tree))
      (check-equal? true (element-of-treeset? 4 tree))
      (check-equal? true (element-of-treeset? 7 tree2))))

   (test-case
    "tree as list"
    (let* ((tree (fill-tree (make-tree 50 null null) 100 20))
           (tree-list (tree->list-1 tree)))
      (check-equal? tree-list (sort tree-list <))))


   (test-case
    "decode huffman sample message"
    (let ((sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
          (sample-tree (make-code-tree (make-leaf 'A 4)
                                       (make-code-tree
                                        (make-leaf 'B 2)
                                        (make-code-tree (make-leaf 'D 1)
                                                        (make-leaf 'C 1))))))
      (check-equal? '(A D A B B C A) (decode sample-message sample-tree))))))


(run-tests sicp-ch2-tests)

