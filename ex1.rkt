#lang sicp

(define a 3)
(define b (+ a 1))

(+ a b (* a b))

(if (and (> b a) (< b (* a b)))
    b
    a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

(/ (+ 5 4 (+ (- 2 3 6) (/ 4 5))) (* 3 (- 6 2) (- 2 7)))

(define (square x) (* x x))
(define (sum-of-squares x y) ( + (square x) (square y)))

(define (big-squares a b c) (cond ((and (> a b) (> c b)) (sum-of-squares a c))
                                  ((and (> b a) (> c a)) (sum-of-squares b c))
                                  (else (sum-of-squares a b))))

(define (average x y) (/ (+ x y) 2))

(define (sqrt x) (define (good-enough? guess) (< (abs (- (square guess) x)) 0.001))
                 (define (improve guess) (average guess (/ x guess)))
                 (define (sqrt-iter guess)
                    (if (good-enough? guess x)
                        guess
                        (sqrt-iter (improve guess x) x)))
                  (sqrt-iter 1.0 x))

(define (better-good-enough? guessPrev guessCurrent) (< (abs (- guessPrev guessCurrent)) 0.01))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (improve guess x) (average guess (/ x guess)))
(define (new-sqrt-iter guess x)
  (if (better-good-enough? guess (improve guess x))
      guess
      (new-sqrt-iter (improve guess x) x)))

(define (new-sqrt x) (new-sqrt-iter 1.0 x))

(define (improve-for-cube guess x) (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-iter guess x)
  (if (better-good-enough? guess (improve-for-cube guess x))
      guess
      (cube-iter (improve-for-cube guess x) x)))
(define (cube x) (cube-iter 1.0 x))


