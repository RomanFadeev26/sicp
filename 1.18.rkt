#lang sicp

(define (square x) (* x x))

(define (even? n) (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (^ x y)
  (define (iter a b n)
    (cond ((= n 0) a)
          
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 x y))

(define (double x) (+ x x))

(define (half x) (/ x 2))

(define (* x y)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (double b) (/ n 2)))
          (else (iter (+ a b) b (- n 1)))))
  (iter 0 x y))
