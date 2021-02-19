#lang sicp

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Некорректно помеченные данные -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) datum)
        (else (error "Некорректно помеченные данные -- CONTENTS" datum))))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "Нет метода для этих типов -- APPLY-GENERIC"
                           (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                            (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error
                              "Нет метода для этих типов -- APPLY-GENERIC"
                              (list op type-tags)))))))
              (error "Нет метода для этих типов -- APPLY-GENERIC"
                     (list op type-tags)))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (put 'raise 'scheme-number (lambda (x) (make-rat n 1)))
  (put 'change-sign '(scheme-number) (lambda (x) (- 0 x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


(define (install-rational-package)
  ;; внутренние процедуры
  (define numer car)
  (define denom cdr)
  (define (equ? x y)
    (and (= (numer x) (numer y)) (= (denom x) (denom y))))
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  
  (define (make-rat x y)
    (let ((g (gcd x y)))
      (if(< x 0)a
         (cons (/ (positive x) g) (/ (negative y) g))
         (cons (/ x g) (/ y g)))))
  
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
  ;; интерфейс к остальной системе
  (define (tag x) (attach-tag 'rational x))
  (put 'equ? '(rational rational) equ?)
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put '=zero? '(rational) (lambda (x) (= (numer x) 0)))
  (put 'raise 'rational (lambda (x) (make-real (/ (number x) (numer y)))))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  (put 'change-sign '(rational) (lambda (x) (make-rat (change-sign (numer x)) (denom x))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))


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
  (define (equ? x y)
    (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
  ;; интерфейс к остальной системе
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'equ? '(rectangular rectangular) equ?)
  (put '=zero? '(rectangular) (lambda (x) (and (= (real-part x) 0) (= (imag-part x) 0))))
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'change-sign '(rectangular) (lambda (x) (make-from-real-imag (change-sign (real-part x)) (change-sign (imag-part x)))))
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
  (define (equ? x y)
    (and (= (magnitude x) (magnitude y)) (= (angle x) (angle y))))
  
  
  ;; интерфейс к остальной системе
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'equ? '(polar polar) equ?)
  (put '=zero? '(polar) (lambda (x) (and (= ('magnitude x) 0) (= ('angle x) 0))))
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'change-sign '(polar) (lambda (x) (make-from-mag-ang (change-sign (magnitude x)) (change-sign (angle x)))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
  ;; процедуры, импортируемые из декартова и полярного пакетов
  (define (make-from-real-imag x y)
    ((get 'make-from-real-image 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; внутренние процедуры
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
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  ;; интерфейс к остальной системе
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex) (lambda (x y) (apply-generic 'equ? x y)))
  (put '=zero? '(complex) (lambda (x) (apply-generic '=zero? x)))
  (put 'change-sign 'complex (lambda (x) (change-sign x)))
  (put-coercion 'scheme-number 'complex scheme-number->complex)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-complex-from-mag-ang 'complex) r a))

(define (args-coercion args type)
  (cond ((null? args) '())
        ((eq? (type-tag (car args)) type) (append (list (car args)) (args-coercion (cdr args) type)))
        ((get-coercion (type-tag (car args)) type)
         (append (list ((get-coercion (type-tag (car args)) type) (car args))) (args-coercion (cdr args) type)))
        (else false)))

(define (apply-generic-2 op . args)
  (define (coercion rest)
    (cond ((null? rest) (error "Нет метода для этих типов -- APPLY-GENERIC" (list op type-tags)))
          ((args-coercion args (car rest)) (args-coercion args (car rest)))
          (else (coercion (cdr rest)))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (coercion type-tags)))))

(define (gt? a b)
  (let ((r1 (raise a))
        (r2 (raise b))
        (type1 (type-tag a)))
    (let ((r2-type (type-tag r2)))
      (and r1 r2 (or (eq? type1 r2-type)
                  (gt? a r2))))))

(define (apply-generic-3 op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "Нет метода для этих типов -- APPLY-GENERIC"
                           (list op type-tags))
                    (cond ((gt? a1 a2) (apply-generic-3 op a1 (raise a2)))
                          ((gt? a2 a1) (apply-generic-3 op (raise a1) a2))
                          (else (error "Нет метода для этих типов -- APPLY-GENERIC"
                           (list op type-tags))))))
              (error "Нет метода для этих типов -- APPLY-GENERIC"
                     (list op type-tags)))))))


(define (install-polynominal-package)
  ;; внутренние процедуры
  ;; представление poly
  (define (make-poly var terms)
    (cons var terms))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? x y)
    (and (variable? x) (variable? y) (eq? x y)))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Многочлен от разных переменных -- ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable p1 p2)
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Многочлен от разных переменных -- MUL-POLY" (list p1 p2))))
  (define (sub-poly p1 p2)
    (if (same-variable p1 p2)
        (make-poly (variable p1)
                   (sub-terms (term-list p1)
                              (term-list p2)))
        (error "Многочлен от разных переменных -- SUB-POLY" (list p1 p2))))
  ;; интерфейс к остальной системе
  (define (tag p) (attach-tag 'polynominal p))
  (put '=zero? '(polynominal) (lambda (x) (empty-termlist? (term-list x))))
  (put 'change-sign '(polynominal)
       (lambda (x) (make-poly
                    (variable x)
                    (map (lambda (term) (make-term (order term) (change-sign (coeff term)))) (term-list x)))))
  (put 'add '(polynominal polynominal)
       (lambda (x y) (tag (add-poly p1 p2))))
  (put 'mul '(polynominal polynominal)
       (lambda (x y) (tag (mul-poly p1 p2))))
  (put 'mul '(polynominal polynominal)
       (lambda (x y) (tag (sub-poly p1 p2))))
  (put 'make 'polyominal
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1))
               (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))

(define (sub-terms L1 L2)
  (cond ((empty-termlist? L1) (map change-sign L2))
        ((empty-termlist? L2) L1)
        (else
         (let ((changedL2 (change-signs L2)))
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (sub-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     (change-sign t2) (sub-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (sub (coeff t1) (coeff t2)))
                     (sub-terms (rest-terms L1)
                                (rest-terms L2))))))))))

(define (=zero? x) (apply-generic '=zero? x))
(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-terms-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coef t1) (coef t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))
(define (change-sign x) (apply-generic 'change-sign x))


