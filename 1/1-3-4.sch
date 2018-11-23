
(load "../lib/util.sch")

;; samples

(define (average-dump f)
  (lambda (x) (average x (f x))))

(define (sqrt-fixpoint x)
  (fixed-point-display (lambda (y) (/ x y))
                       1.00))

(define (sqrt-fix x)
  ;; y^2 = x
  ;; => y = x/y
  (fixed-point (average-dump (lambda (y) (/ x y)))
               1.0))

(define (sqrt-ave-test x)
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2))
               1.00))

(define (cube-root x)
  ;; y^3 = x
  ;; => y = x/y^2
  (fixed-point (average-dump (lambda (y) (/ x (square y))))
               1.0))


(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)


(define (cube x) (* x x x))

;; ((deriv cube) 5)
;; 75.00014 ...

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-newton x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))


(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-trans x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-dump
                            1.0))

(define (sqrt-newton-transform x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))


;; exercise 1.40

(define (fixed-point-display f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess step)
    (let ((next (f guess)))
      (display guess) (newline)
      (cond ((close-enough? guess next)
             (display step)
             next)
            (else (try next (+ step 1))))))
  (try first-guess 1))

(define (cubic-fixed-point a b c)
  (define (num-cubic x)
    (+ (* x x x) (* a x x) (* b x) c x))
  (fixed-point num-cubic 3.00))

(define (d-cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

(define (cubic-0-point a b c guess)
  (newtons-method (d-cubic a b c) guess))

;; samples of cubic = 0
;; -6 +11 -6 => (or 1 2 3)

;; (cubic-0-point -6 11 -6 2.03)
;; Value: 1.999999999999997


;;exercise 1.41

(define (double func)
  (lambda (x)
    (func
     (func x))))


(double double)
(lambda (x) (double (double x)))


((double double) inc)
(lambda (x) (inc (inc (inc (inc x)))))

;; (((double (double double)) inc) 5)
;; 21


;; exercise 1.42

(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose square inc) 6)


;; exercise 1.43


(define (repeated f count)
  (lambda (x)
    (define (iter count)
      (cond ((<= count 0) x)
            ((= count 1) (f x))
            (else (f (iter (- count 1))))))
    (iter count)))

;; ((repeated square 2) 5)
;; 625


;; exercise 1.44
(define (smooth f dx)
  (lambda (x)
    (/ (+  (f (- x dx)) (f (+ x 0)) (f (+ x dx)))
       3)))

(define (n-fold-smoothed-function f dx n)
  (repeated (smooth f dx) n))


;; exercise 1.45

(define (nd-sqrt x n)
  "x^(1/n)"
  (fixed-point ((repeated average-dump (- n 1))
                (lambda (y) (/ x
                               (expt y (- n 1)))))
                       1.00))

(define (4d-root x)
  (fixed-point ((repeated average-dump 2)
                (lambda (y) (/ x
                               (* y y y))))
               1.00))

;; exercise 1.46

(define (iterative-improve is-enough? f)
  (define (iter guess)
    (let ((next (f guess)))
      (if (is-enough? guess next) next
          (iter next))))
  (lambda (guess)
    (iter guess)))


(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve close-enough? f) first-guess))


(define (sqrt-itertive x)
  (define (good-enough? next ignore)
    (< (abs (- (square next) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) x))

;; (exact->inexact (sqrt-itertive 5))
