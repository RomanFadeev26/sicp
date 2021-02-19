#lang sicp

(define (square n) (* n n))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle-rectangular z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag-rectangular x y) (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a)))))


;; Lisa

(define (real-part-polar z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part-polar z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar (cons (sqrt (+ (square x) (square y)))
        (atan y x))))

(define (make-from-mag-ang-polar r a) (attach-tag 'polar (cons r a)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Некорректно помеченные данные -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Некорректно помеченные данные -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;(define (real-part z)
;  (cond ((rectangular? z)
;         (real-part-rectangular (contents z)))
;        ((polar? z)
;         (real-part-polar (contents z)))
;        (else (error "Неизвестный тип -- REAL-PART" z))))

;(define (imag-part z)
 ; (cond ((rectangular? z)
  ;       (imag-part-rectangular (contents z)))
   ;     ((polar? z)
    ;     (imag-part-polar (contents z)))
     ;   (else (error "Неизвестный тип -- IMAG-PART" z))))

;(define (magnitude z)
 ; (cond ((rectangular? z)
  ;       (magnitude-rectangular (contents z)))
   ;     ((polar? z)
    ;     (magnitude-polar (contents z)))
     ;   (else (error "Неизвестный тип -- MAGNITUDE" z))))

;(define (angle z)
 ; (cond ((rectangular? z)
  ;       (angle-rectangular (contents z)))
   ;     ((polar? z)
    ;     (angle-polar (contents z)))
     ;   (else (error "Неизвестный тип -- ANGLE" z))))

;(define (make-from-real-imag x y)
 ; (make-from-real-imag-rectangular x y))

;(define (make-from-mag-ang r a)
 ; (make-from-mag-ang-polar r a))

(define (install-rectangular-package)
  ;; внутренние процедуры
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z )))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; интерфейс к остальной системе
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; внутренние процедуры
  (define (magnitude z)
    (car z))
  (define (angle z)
    (cdr z))
  (define (make-from-mag-ang r a)
    (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  
  
  
  ;; интерфейс к остальной системе
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (error
;           "Нет метода для этих типов -- APPLY-GENERIC"
;           (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;(define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
;(define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'polar) r a))

(define (derive exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'derive (operator exp)) (operands exp) var))))

(define (install-diff-package)
  (define (calc-sum exp var)
    (make-sum (derive (addend exp) var)
              (derive (augend exp) var)))
  (define (calc-prod exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (muliplier exp) var)
                   (multiplicand exp))))

  (put 'derive '(+) calc-sum)
  (put 'derive '(*) calc-prod)
  'done)

(define (get-record file)
  ((get 'get-record (type-tag file)) (contents first-name)))


(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Неизвестная операция -- MAKE-FROM-REAL0IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Неизвестная операция -- MAKE-FROM-MAG-ANG" op))))
  dispatch)