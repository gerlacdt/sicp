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
        ((not (pair? list1)) (list list1))
        (else (append (fringe (car list1))
                      (fringe (cdr list1))))))

(define (scale-tree tree factor)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* factor tree))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree-map tree factor)
  (my-map (lambda (sub-tree)
            (cond ((pair? sub-tree) (scale-tree-map sub-tree factor))
                  (else (* factor sub-tree)))) tree))

(define (square-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (tree-map proc tree)
  (my-map (lambda (sub-tree)
            (if (pair? sub-tree)
                (tree-map proc sub-tree)
                (proc sub-tree))) tree))

(define (square-tree-map tree)
  (tree-map square tree))


(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (my-map (lambda (x)
                               (cons (car s) x)) rest)))))

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (my-accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (my-accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ 1 low) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))


(define (sum-odd-squares tree)
  (my-accumulate + 0 (my-map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (my-accumulate cons null (filter even? (my-map fib (enumerate-interval 0 n)))))

(define (all-fibs n)
  (my-accumulate cons null (my-map fib (enumerate-interval 0 n))))

(define (list-fib-squares n)
  (my-accumulate cons null (my-map square
                                   (my-map fib (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (my-accumulate * 1 (my-map square (filter odd? sequence))))

(define (map-accum proc sequence)
  (my-accumulate (lambda (x y) (cons (proc x) y)) null sequence))

(define (append-accum seq1 seq2)
  (my-accumulate cons seq2 seq1))

(define (length-accum sequence)
  (my-accumulate (lambda (x y) (+ y 1)) 0 sequence))


(define (horner-eval x coefficient-sequence)
  (my-accumulate (lambda (this-coeff higher-terms)
                   (+ this-coeff (* x higher-terms)))
                 0
                 coefficient-sequence))

(= 79 (horner-eval 2 (list 1 3 0 5 0 1))) ; should be 79

(define (count-leaves-accum tree)
  (my-accumulate + 0 (my-map (lambda (sub-tree)
                               (if (pair? sub-tree)
                                   (count-leaves-accum sub-tree)
                                   1))
                             tree)))

(count-leaves-accum '(1 2 3 (4 5 (6)))) ;should be 6

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (my-accumulate op init (my-map
                                    (lambda (x) (car x)) seqs))
            (accumulate-n op init (my-map
                                   (lambda (x) (cdr x)) seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(my-accumulate / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-left + 0 (list 1 2 3))
(my-accumulate list null (list 1 2 3))
(fold-left list null (list 1 2 3))

(define (reverse-fold-right sequence)
  (my-accumulate (lambda (x y) (append y (list x))) null sequence))

(define (reverse-fold-left sequence)
  (fold-left (lambda (x y) (append  (list y) x)) null sequence))

(reverse-fold-left '(1 2 3 4 5))
(reverse-fold-right '(1 2 3 4 5))

(define (flatmap proc seq)
  (my-accumulate append null (my-map proc seq)))

;;; see good difference between map and map with append
(flatmap (lambda (x) (list x (square x) (cube x))) '(1 2 3 4 5 6))  ; all elements in one big list
(my-map (lambda (x) (list x (square x) (cube x))) '(1 2 3 4 5 6))  ; elements are distributed in list of lists

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (my-map (lambda (j)
                       (list j i)) (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (my-map make-pair-sum
           (filter prime-sum?
                   (unique-pairs n))))

(prime-sum-pairs 6)

(define (my-remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s)
      (list null)
      (flatmap (lambda (x)
                 (my-map (lambda (p) (cons x p))
                         (permutations (remove x s))))
               s)))

(permutations '(1 2 3))

(unique-pairs 4)

(define (find-triples n)
  (filter (lambda (triple)
            (and (not (= (car triple) (cadr triple)))
                 (not (= (car triple) (caddr triple)))
                 (not (= (cadr triple) (caddr triple)))))
          (flatmap
           (lambda (i) (my-map
                        (lambda (j)
                          (cons j i)) (enumerate-interval 1 n)))
           (unique-pairs n))))

(define (find-unique-triples-sum n)
  (filter (lambda (triple)
            (= n (+ (car triple) (cadr triple) (caddr triple))))
          (find-triples n)))

(find-triples 4)
(find-unique-triples-sum 10)

(define (memq item seq)
  (cond ((null? seq) false)
        ((eq? item (car seq)) seq)
        (else (memq item (cdr seq)))))

(memq 2 '(1 2 3 4 5))
(memq 'd '(a b c))
(memq 'apple '(x (apple sauce) y apple pear))

(define (my-equal x y)
  (if (and (pair? x) (pair? y))
      (my-equal (cdr x) (cdr y))
      (eq? x y)))

(my-equal '(this is a list) '(this is a list))
(my-equal '(this (is a) list) '(this is a list))

;;; symbolic differentiation
(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

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

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var)
                             1
                             0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum (make-product (multiplier exp)
                                                (deriv (multiplicand exp) var))
                                  (make-product (deriv (multiplier exp) var)
                                                (multiplicand exp))))
        ((exponantiation? exp)
         (make-product (deriv (base exp) var)
                       (make-product (exponent exp)
                                     (make-exponantiation (base exp)
                                                          (make-sum (exponent exp) -1)))))
        (else (error "unknown expression type - DERIV" exp))))


(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

(make-exponantiation 4 2)
(make-exponantiation 4 0)
(make-exponantiation 4 1)

(deriv '(** x 4) 'x)

;;; data structures (sets)

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((my-equal x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(element-of-set? 6 '(1 2 3 4 5))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(adjoin-set 1 '(2 3 4 5))
(adjoin-set 2 '(2 3 4 5))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) null)
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(intersection-set '(1 2 3 4) '(3 4 5 6))
(intersection-set '() '(1 2 3))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

(union-set '(1 2 3 4) '(3 4 5 6))

(define (element-of-set-ordered x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set-ordered x (cdr set)))))

(element-of-set-ordered 7 '(1 2 3 4 5))

(define (intersection-set-ordered set1 set2)
  (if (or (null? set1) (null? set2))
      null
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-ordered (cdr set1)
                                                  (cdr set2))))
              ((< x1 x2)
               (intersection-set-ordered (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-ordered set1 (cdr set2)))))))

(intersection-set-ordered '(1 2 3 4 7) '(3 4 5 6 7))

(define (adjoin-set-ordered x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set-ordered x (cdr set))))))

(adjoin-set-ordered 4 '(5 6))

(define (union-set-ordered set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set-ordered (cdr set1) (cdr set2))))
                      ((< x1 x2)
                       (cons x1 (union-set-ordered (cdr set1) set2)))
                      ((< x2 x1)
                       (cons x2 (union-set-ordered set1 (cdr set2)))))))))

(union-set-ordered '(1 2 3 4 5) '(2 3 4 5 6))

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-treeset? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-treeset? x (left-branch set)))
        ((> x (entry set))
         (element-of-treeset? x (right-branch set)))))

(define (adjoin-treeset x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-treeset x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-treeset x (right-branch set))))))


(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (lookup-list given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (car set-of-records))
         (car set-of-records))
        (else (lookup-list given-key (cdr set-of-records)))))

(define (lookup given-key tree)
  (cond ((null? tree) false)
        ((= given-key (entry tree)) tree)
        ((< given-key (entry tree))
         (lookup given-key (left-branch tree)))
        (else (lookup given-key (right-branch tree)))))

(define (fill-tree tree max n)
  (if (= n 0)
      tree
      (fill-tree (adjoin-treeset (random max) tree) max (- n 1))))


;;; huffman encoding

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (huffman-left-branch tree)
  (car tree))

(define (huffman-right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (huffman-left-branch branch))
        ((= bit 1) (huffman-right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH"))))


;;; huffman prepare char merging for constructing a code-tree
(define (huffman-adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (huffman-adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (huffman-adjoin-set (make-leaf (car pair)
                                       (cadr pair))
                            (make-leaf-set (cdr pairs))))))

(define (sample-tree)
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

 'ch2-done
