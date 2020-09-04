#lang sicp

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)
                      (- n 2))))))

(define (fib-iter n)
  (define (fib-in a b count)
    (if (= count 0)
        b
        (fib-in (+ a b) a (- count 1))))
  (fib-in 1 0 n))

(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc amount (- kinds-of-coins 1))
                   (cc (- amount
                          (first-denomination)) kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
      (cond ((= kinds-of-coins 1) 1)
            ((= kinds-of-coins 2) 5)
            ((= kinds-of-coins 3) 10)
            ((= kinds-of-coins 4) 25)
            ((= kinds-of-coins 5) 50)))


(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (f (- n 2)) (f (- n 3)))))

(define (f-iter n)
  (define (f-inner a b c count)
    (if (= count 0)
        c
        (f-inner (+ a b c) a b (- count 1))))
  (f-inner 2 1 0 n))

(define (paskal-triangle row elem)
  (if (or (= row 0) (= row 1) (= elem 0) (= elem row))
      1
      (+ (paskal-triangle (- row 1) (- elem 1)) (paskal-triangle (- row 1) elem))))


(define (^ n count)
  (define (iter produce count)
    (if (= count 1)
        produce
        (iter (* produce n) (- count 1))))
  (iter n count))


(define (average x y) (/ (+ x y) 2))

(define (better-good-enough? guessPrev guessCurrent) (< (abs (- guessPrev guessCurrent)) 0.01))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (improve guess x) (average guess (/ x guess)))
(define (sqrt-iter guess x)
  (if (better-good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x) (sqrt-iter 1.0 x))

