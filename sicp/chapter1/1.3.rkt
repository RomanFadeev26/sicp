#lang sicp

(define (cube n) (* n n n))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))



;;(define (pi-sum a b)
;;  (if (> a b)
;;     0
;;     (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;;(define (sum term a next b)
;;  (if (> a b)
;;      0
;;      (+ (term a) (sum term (next a) next b))))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (inc a) (+ a 1))

(define (sum-cubes2 a b) (sum cube a inc b))

(define (identity x) x)

;(define (sum-integers2 a b) (sum identity a inc b))

;(define (sum-integers3 a b) (sum2 identity a inc b))

;;(define (pi-sum2 a b)
;;  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
;;  (define (pi-next x) (+ x 4))
;;  (sum pi-term a pi-next b))

;(define (integral f a b dx)
;  (define (add-dx x) (+ x dx))
;  (* (sum f (+ a (/ dx 2)) add-dx b) dx))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial a) (product identity 1 inc a))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))


(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2)))) a (lambda (x) (+ x 4))))

(define (integral f a b dx)
  (* (sum f (+ a (/ dx 2.0)) (lambda (x) (+ x dx)) b)
     dx))

(define (square x) (* x x))

(define (f x y)
  (let ((a (+ 1 (* x y)))
       (b (- 1 y)))
  (+ (* x (square a))
     (* y b)
     (* a b))))

(define (average x y) (/ (+ x y) 2))

;(define (positive? x) (> x 0))

;(define (negative? x) (< x 0))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
        (if (close-enough? neg-point pos-point)
            mid-point
            (let ((test-value (f mid-point)))
                  (cond ((positive? test-value) (search f neg-point mid-point))
                        ((negative? test-value) (search f mid-point pos-point))
                        (else mid-point))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
          ((and (positive? a-value) (negative? b-value)) (search f b a))
          (else
           (error "У аргументов не разные знаки" a b)))))



(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;(define (sqrt x)
;  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define gold-point (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(define (average-damp f)
   (lambda (x) (average x (f x))))

;(define (sqrt x)
;  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))


(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g) (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;(define (sqrt x)
;  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;(define (sqrt x)
;  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x)) newton-transform 1.0))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))


(define (double g) (lambda (x) (g (g x))))


(define (compose f g) (lambda (x) (f (g x))))

(define (repeat f n)
  (define (iter count g)
    (if (= count 1)
        (lambda (x) (g x))
        (iter (- count 1) (compose f g))))
  (iter n f))



(define (smooth f)
  (define (average a b c) (/ (+ a b c) 3))
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))


(define (smooth-n f n)
  ((repeat smooth n) f))

(define (^ x y)
  (define (iter a b n)
    (cond ((= n 0) a)
          
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 x y))

(define (root x n)
  (let ((mod (- n 1)))
  (let ((n-average-damp (repeat average-damp mod)))
    (fixed-point (n-average-damp (lambda (y) (/ x (^ y mod)))) 1.0))))


(define (iterative-improve check f)
  (define (result x)
    (let ((next (f x)))
    (if (check x next)
        x
        (result next))))
  result)




(define (better-good-enough? guessPrev guessCurrent) (< (abs (- guessPrev guessCurrent)) 0.01))
(define (improve guess x) (average guess (/ x guess)))
;(define (new-sqrt-iter guess x)
;  (if (better-good-enough? guess (improve guess x))
;      guess
;      (new-sqrt-iter (improve guess x) x)))
;
;(define (new-sqrt x) (new-sqrt-iter 1.0 x))

(define (new-sqrt x)
  ((iterative-improve better-good-enough? (lambda (z) (average z (/ x z)))) x))

(define (new-fixed-point f first-guess)
  ((iterative-improve close-enough? f) first-guess))