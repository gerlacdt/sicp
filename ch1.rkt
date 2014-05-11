(module ch1 racket
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

  (define (sqrt-iter guess x)
    (if (good-enough? guess (improve guess x))
        guess
        (sqrt-iter (improve guess x) x)))

  (define (improve guess x)
    (average guess (/ x guess)))

  (define (average x y)
    (/ (+ x y) 2))

  (define (good-enough? guess new-guess)
    (< (abs (- guess new-guess)) 0.001))

  (define (sqrt x)
    (sqrt-iter 1.0 x))

  (define (cube x)
    (* x x x))

  (define (cube-root-iter guess x)
    (if (good-enough-cube? guess (improve-cube guess x))
        guess
        (cube-root-iter (improve-cube guess x) x)))

  (define (improve-cube guess x)
    (/ (+ (* 2 guess) (/ x (* guess guess))) 3))

  (define (good-enough-cube? guess new-guess)
    (< (abs (- guess new-guess)) 0.00001))

  (define (cube-root x)
    (cube-root-iter 1.0 x))

  (define (done)
    'mydone1)

  (done))
