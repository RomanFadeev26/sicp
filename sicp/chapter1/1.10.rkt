#lang sicp

(define (factorial x)
  (if (= x 1)
      1
      (* x (factorial (- x 1)))))

(define (factorial-tail n)
  (define (fact-iter product counter)
    (if (> counter n)
        product
        (fact-iter (* counter product) (+ counter 1))))
  (fact-iter 1 1))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))