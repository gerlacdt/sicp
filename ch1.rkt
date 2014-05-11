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

  (define (done)
    'mydone1)

  (done))
