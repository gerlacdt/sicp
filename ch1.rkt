(module ch1 racket
  (require racket/trace)
  
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

  (define (sqrt x)
    (define (sqrt-iter guess)
      (if (good-enough? guess (improve guess))
          guess
          (sqrt-iter (improve guess))))
    (define (improve guess)
      (average guess (/ x guess)))
    (define (average x y)
      (/ (+ x y) 2))
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

  (define (done)
    'mydone1)

  (done))

