#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "../ch3.rkt")

(define sicp-ch3-tests
  (test-suite
   "sicp chapter 3 test suite"
   (test-case
    "streams"
    (check-equal? the-empty-stream '())
    (check-equal? (my-stream->list (stream-enumerate-interval 0 4)) '(0 1 2 3 4))
    (check-equal? (my-stream-ref integers 3) 4)
    (check-equal? (my-stream->list (my-stream-map (lambda (x) (+ x x))(stream-enumerate-interval 0 3))) '(0 2 4 6))
    (check-equal? (my-stream->list (my-stream-map-multi-args + (stream-enumerate-interval 0 4) (stream-enumerate-interval 0 4))) '(0 2 4 6 8))
    (check-equal? (my-stream-take (stream-enumerate-interval 0 10) 2) '(0 1))
    (check-equal? (my-stream->list (stream-filter even? (stream-enumerate-interval 0 10))) '(0 2 4 6 8 10))
    (check-equal? (my-stream-take fibs 5) '(0 1 1 2 3)))))

(run-tests sicp-ch3-tests)
