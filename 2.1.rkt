#lang sicp

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define numer car)
(define denom cdr)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (negative x) (- x (* 2 x)))
(define (positive x) (+ x (* 2 x)))

(define (make-rat x y)
  (let ((g (gcd x y)))
    (if(< x 0)
       (cons (/ (positive x) g) (/ (negative y) g))
       (cons (/ x g) (/ y g)))))


;(cons (/ x g) (/ y g))
(define x (cons 1 2))
(define y (cons 3 4))

(define z (cons x y))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

(define (make-segment start end)
  (cons start end))

(define (start-segment point)
  (car point))
(define (end-segment point)
  (cdr point))

(define (make-point x y)
  (cons x y))

(define (x-coord point)
  (car point))
(define (y-coord point)
  (cdr point))

(define (midpoint-segment segment)
  (let ((start-point (start-segment segment))
        (end-point (end-segment segment)))
    (let ((start-x (x-coord start-point))
          (start-y (y-coord start-point))
          (end-x (x-coord end-point))
          (end-y (y-coord end-point)))
      (let ((mid-x (/ (+ start-x end-x) 2))
            (mid-y (/ (+ start-y end-y) 2)))
        (make-point mid-x mid-y)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-coord p))
  (display ",")
  (display (y-coord p))
  (display ")"))

(define test1 (make-segment (make-point 200 200) (make-point 400 400)))


(define (make-rectangle top-left bottom-right)
  (let ((top-right (make-point (x-coord bottom-right) (y-coord top-left)))
        (bottom-left (make-point (x-coord top-left) (y-coord bottom-right))))
    (cons (make-segment top-left bottom-left) (make-segment top-left top-right))))

(define (height rect)
  (let ((a (y-coord (start-segment (car rect))))
        (b (y-coord (end-segment (car rect)))))
    (- b a)))


(define (width rect)
  (let ((a (x-coord (start-segment (cdr rect))))
        (b (x-coord (end-segment (cdr rect)))))
    (- b a)))

(define (perimiter rect)
  (* 2 (+ (height rect) (width rect))))

(define (square rect)
  (* (height rect) (width rect)))


(define (cons1 x y)
  (lambda (m) (m x y)))

(define (car1 z)
  (z (lambda (p q) p)))

(define (cdr1 z)
  (z (lambda (p q) q)))


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval-all x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((and (negative? lx) (positive? ux) (negative? ly) (positive? uy))
           (make-interval (* lx uy) (max (* ux uy) (* lx ly))))
          ((and (negative? lx) (positive? ux) (negative? ly) (negative? uy))
           (make-interval (* ux ly) (* lx ly)))
          ((and (negative? lx) (negative? ux) (negative? ly) (positive? uy))
           (mul-interval y x))
          ((and (negative? lx) (positive? ux) (positive? ly) (positive? uy))
           (make-interval (* lx uy) (* ux uy)))
          ((and (positive? lx) (positive? ux) (negative? ly) (positive? uy))
           (mul-interval y x))
          ((and (negative? lx) (negative? ux) (positive? ly) (positive? uy))
           (make-interval (* lx uy) (* ux ly)))
          ((and (positive? lx) (positive? ux) (negative? ly) (negative? uy))
           (mul-interval y x))
          ((and (negative? lx) (negative? ux) (negative? ly) (negative? uy))
           (make-interval (* ux uy) (* lx ly)))
          (else (make-interval (* lx ly) (* ux uy))))))

(define (div-interval x y)
  (if (or (< y 0) (= y 0))
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))
      (error "Делить на 0 нельзя!")))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (radius x)
  (/ (- (upper-bound x) (lower-bound x)) 2))


(define test2 (make-interval 23.0 25.0))

(define test3 (make-interval -23.0 25.0))

(define test4 (make-interval -25.0 -23.0))