
;; samples 1.1.
(define (m-square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

;; exercise 1.1.
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
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

;; exercise 1.3
(define (larger_two n1 n2 n3)
  (cond ((and (> n1 n3) (> n2 n3))
         (+ n1 n2))
        ((and (> n1 n2) (> n3 n2))
         (+ n1 n3))
        ((and (> n2 n1) (> n3 n1))
         (+ n3 n2))))


;; exercise 1.4.

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;;;; a-(-b)=a+b, a+(+b)=a+b
;;;; function as combinated, minus or plus is if b is bigger than 0 or not

;; exercise 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;;;; (test 0 (p))
;;;; procedure does not end.  because, (= x 0) in if form eval x while to be not functional primirative object. but, (p) calls (p) every times.


