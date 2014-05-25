#lang racket
(require racket/trace)
(provide (all-defined-out))

(define (fib n)
  (define (fib-iter a b n)
    (if (= n 0)
        a
        (fib-iter b (+ a b) (- n 1))))
  (fib-iter 1 1 n))


(define (square n)
  (* n n))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (sum-of-squares-of-largest x y z)
  (cond ((and (> x y) (> x z) (> y z)) (sum-of-squares x y))
        ((and (> y z) (> y x) (> z x)) (sum-of-squares y z))
        (else (sum-of-squares x z))))

(define (average x y)
    (/ (+ x y) 2))

(define (sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess (improve guess))
        guess
        (sqrt-iter (improve guess))))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess new-guess)
    (< (abs (- guess new-guess)) 0.001))
  (sqrt-iter 1.0))

(define (cube x)
  (* x x x))

(define (cube-root x)
  (define (cube-root-iter guess)
    (if (good-enough? guess (improve guess))
        guess
        (cube-root-iter (improve guess))))
  (define (improve guess)
    (/ (+ (* 2 guess) (/ x (* guess guess))) 3))
  (define (good-enough? guess new-guess)
    (< (abs (- guess new-guess)) 0.00001))
  (cube-root-iter 1.0))

(define (factorial n)
  (cond ((= n 1) 1)
        (else (* n (factorial (- n 1))))))

(define (fact-iter n)
  (define (fact-helper product counter max-count)
    (if (> counter max-count)
        product
        (fact-helper (* product counter) (+ counter 1) max-count)))
  (fact-helper 1 1 n))

(define (plus a b)
  (if (= a 0)
      b
      (inc (plus (dec a) b))))

(define (plus2 a b)
  (if (= a 0)
      b
      (plus2 (dec a) (inc b))))

(define (inc n)
  (+ n 1))

(define (dec n)
  (- n 1))

(define (A x y)
  ;; Ackerman function
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(define (pow-2 n)
  (A 1 n))

(define (double n)
  (A 0 n))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1)) 
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

(define (pascal row col)
  "computes all number of the pascal triangle 1, 1, 2, 3, 3, 4, 6, 4.
This are the binominial coeffients."
  (cond ((= col 0) 1)
        ((= col row) 1)
        (else (+ (pascal (- row 1) col) (pascal (- row 1) (- col 1))))))

(define (expt b n)
  (define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b
                   (- counter 1)
                   (* b product))))
  (expt-iter b n 1))

(define (fast-expt b n)
  (cond ((= n 1) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (fast-expt-iter b n product)
  (cond ((= n 0) product)
        (else (if (even? n)
                  (fast-expt-iter (square b) (/ n 2) product)
                  (fast-expt-iter b (- n 1) (* product b))))))

(define (my-mult a b)
  (if (= b 0)
      0
      (+ a (my-mult a (- b 1)))))

(define (my-mult-fast a b)
  (cond ((= b 0) 0)
        ((even? b) (my-mult-fast (double a) (/ b 2)))
        (else (+ a (my-mult-fast a (- b 1))))))

(define (my-mult-iter a b result)
  (cond ((= b 0) result)
        (else (my-mult-iter a (- b 1) (+ result a)))))

(define (my-gcd a b)
  (if (= b 0)
      a
      (my-gcd b (remainder a b))))

(define (tower-of-honoi n from to help)
  "Solves the tower of hanoi puzzle.
    example: tower-of-hanoi number-of-disks tower-label1 tower-label2
    tower-label3"
  (cond ((= n 0) 'done)
        (else (tower-of-honoi (- n 1) from help to)
              (displayln (format "stone ~a from ~a to ~a" n from to))
              (tower-of-honoi (- n 1) help to from))))

(define (smallest-divisor n)
  "Finds the smallest divisor for n."
  (find-divisor n 2))

(define (next-test-divisor n) 
  "Generates the next test divisor."
  (cond ((= n 2) 3)
        (else (+ n 2))))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n ) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-test-divisor test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else 
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (cond ((< n 4294967087) (try-it (+ 1 (random (- n 1)))))
        (else (try-it (+ 1 (random 4294967087))))))


(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (cond ((prime? n) 
         (report-prime n (- (current-milliseconds) start-time))
         #t)
        (else #f)))

(define (report-prime n elapsed-time)
  (displayln (format "~a is prime! elapsed time: ~a ms" n elapsed-time)))

(define (search-for-primes n)
  "Finds the next prime number after n and print it to REPL."
  (let ((next (+ 1 n)))
    (cond ((odd? next) 
           (cond ((prime? next) next)
                 (else (search-for-primes (+ n 2)))))
          (else (search-for-primes (+ n 1))))))

(define (carmichael-number? n)
  "Tests if given number is a carmichael-number. That is n is a
    prime number but passes the fermat test."
  (define (carmichael-number-helper n a)
    (cond ((= a 1) (not (prime? n)))
          ((= (expmod a n n) a) (carmichael-number-helper n (- a 1)))
          (else #f)))
  (carmichael-number-helper n (- n 1)))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x)
  x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (simpson-integral f a b n)
  (let* ((h (/ (- b a) n))
         (f-zero (f a))
         (f-n (f (+ a (* h n))))
         (sum-of-zero-n (+ (* (/ h 3) f-zero) (* (/ h 3) f-n))))
    (define (sum-of-odds)
      (sum (lambda (k) (* ( / h 3) (* 2 (f (+ a (* h k)))))) 1 (lambda (k) (+ k 2)) n))
    (define (sum-of-evens)
      (sum (lambda (k) (* (/ h 3) (* 4 (f (+ a (* h k)))))) 2 (lambda (k) (+ k 2)) n))
    (+ (sum-of-odds) (sum-of-evens) sum-of-zero-n)))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter (term a) 0))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter (term a) 1))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter (term a) null-value))

(define (filtered-accummulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (cond ((filter a)
             (combiner (term a)
                       (filtered-accummulate filter combiner null-value term (next a) next b)))
            (else (filtered-accummulate filter combiner null-value term (next a) next b)))))

'ch1-done

