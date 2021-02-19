#lang sicp

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Недостаточно денег"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Недостаточно денег"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Недостаточно денег")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(define (make-account balance pwd)
  (let ((attempts 0)) 
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Недостаточно денег"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops x) 'ВиуВиуВиуВиу)
    (define (dispatch password message)
      (cond ((and (not (eq? password pwd)) (>= attempts 6)) call-the-cops)
            ((not (eq? password pwd)) (set! attempts (+ 1 attempts))
                                      (lambda (x) "Неверный пароль"))
            ((eq? message 'withdraw) (begin (set! attempts 0)
                                            withdraw))
            ((eq? message 'deposit) (begin (set! attempts 0)
                                           deposit))
            (else (error "Неизвестный вызов -- MAKE-ACCOUNT"
                         message))))
    dispatch)
  )

(define (make-joint acc pwd new-pwd)
  (lambda (npwd m)
    (if (not (eq? npwd new-pwd))
        (lambda (x) "Неверный пароль")
        (acc pwd m))))

(define account (make-account 100 'master))

(define (f x)
  (let ((count 0))
    (set! count (+ count 1))
    count
    ))

(define (make-accumulator initial)
  (lambda (x)
    (set! initial (+ initial x))
    initial))


(define (square x) (* x x))

(define (make-monitored f)
  (let ((times 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) times)
            ((eq? x 'reset-count) (set! times 0)
                                  times)
            (else (set! times (+ 1 times))
                  (f x))))))

(define s (make-monitored sqrt))

(define random-init 10)
(define (rand-update x)
  (random x))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (monte-carlo trials (lambda () (p (random x1 x2) (random y1 y2)))))

(define (rand-gen m)
  (let ((x random-init))
    (cond ((eq? m 'generate) (set! x (rand-update x))
                             x)
          ((eq? m 'reset) (lambda (new-init)
                            (set! x new-init)
                            "Установлено новое стартовое значение")))))


(define a '(a b))
(define b '(c d))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(define (reverse x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
(define w (reverse a))

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define fst (list 'a))
(define snd (list 'b))
(define trd (list 'c))


(define (x3)
  (set-cdr! fst snd)
  (set-cdr! snd trd)
  fst)

(define (infinity)
  (set-car! (x3) fst)
  fst)

(define (x4)
  (set-cdr! snd trd)
  (set-car! fst snd)
  (set-cdr! fst trd)
  fst)

(define (x7)
  (set-cdr! snd fst)
  (set-car! snd fst)
  (set-cdr! trd snd)
  (set-car! trd snd)
  trd)

(define (check items el)
  (cond ((null? items) false)
        ((eq? (car items) el) true)
        (else (check (cdr items) el))))

(define (count-pairs-corrected x)
  (define (count-pairs x store)
    (if (or (not (pair? x)) (check store x))
      0
      (+ (count-pairs (car x) (append store x))
         (count-pairs (cdr x) (append store x))
         1)))
  (count-pairs x '()))


