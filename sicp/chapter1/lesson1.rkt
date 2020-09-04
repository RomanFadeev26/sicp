#lang sicp

(define size 2)
(define (square x) (* x x))
(define (sum-of-squares x y) ( + (square x) (square y)))
(define (f a) (sum-of-squares (+ a 1) (* a 2)))
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(define (abs2 x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs3 x)
  (if (< x 0) (- x) x))

