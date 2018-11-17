;; samples 1.2.1

(define (factional n)
  (if (= n 1)
      n
      (* n (factional (- n 1)))))

(define (factional n)
  (define (iter product counter)
    ;; (display product) (newline)
    (if (> counter n)
        product
        (iter (* product counter)
              (+ counter 1))))
  (iter 1 1))


;; exercise 1.9
(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (ex+1 a b)
  (if (= a 0)
      b
      (begin
        (format #t "~A ~A~%" a b)
        (inc (ex+1 (dec a) b)))))
;; recursive
;; after end of ex+1 each step, interpreter required to process inc.
;; a = 0 means end of (dec a), and after recursive of (dec a), a nests of (1+ b)

(define (ex+2 a b)
  (if (= a 0)
      b
      (begin
        (format #t "(ex+2 ~A ~A)~%" (dec a) (inc b))
        (ex+2 (dec a) (inc b)))))

;; iterative
;; each step of b is current sum value of a+b of integer
;; a = 0 means end of addition


;; exercise 1.10

(define (A x y)
  (format #t "~A ~A~%" x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (A x y)
  (define (A-iter x* y* iter)
    (format #t "~A ~A ~A ~%" x* y* iter)
    (cond ((= y* 0) 0)
          ((= x* 0) (* 2 y*))
          ((= y* 1) 2)
          (else (A-iter (- x* 1)
                        (A-iter x* (- y* 1) (+ 1 iter))
                        (+ 1 iter)))))
    (A-iter x y 0))

;; (A 1 10) => 1024
;; (A 2 4) => 65536
;; (A 3 3) => 65536

(define (f11 n) (A 0 n))
;; 2n :: (* 2 (y == n)) , because x of A is 0.

(define (g11 n) (A 1 n))
;; 2^n
;; 
;; (g11 n)
;; = (A 1 n)
;; = (A 0 (A 1 (- n 1))) ;; in else form of cond
;; = (* 2 (A 1 (- n 1)))
;;   . (A 1 (- n 1))
;;     = (A 0 (A 1 (- (- n 1) 1)))
;; ...
;; at last, y=1, returns 2
;; means
;; ((n-1) times of 2*) * 2

(define (h11 n) (A 2 n))
;; h11. \1 :
;; 0 => 0
;; 1 => 2
;; 2 => 4
;; 3 => 16
;; 4 => 65536
;; 
;; (h11 n)
;; = (A 2 n)
;; = (A 1 (A 2 (- n 1)) ;; in else cond form.
;; = (g11 (A 2 (- n 1))) ;; apply g11
;; = (g11 (h11 (- n 1))) ;; equal
;; = (g11 (g11 (h11 (- n 2)))) ;; equal
;; = (g11 (g11 (g11 (h11 ( - n 3)))))
;; ... (g11 (g11 (g11 (g11 ... while n=0
;;
;; ... 2^(2^.. after n times of 2^(2^ form ..(2^(2^n)))

(define (k11 n) (* 5 (* n n)))



;; sample 1.2.2 fibonacci
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

;; sample 1.2.2 counting change

(define (first-denomination)
  (list 50 25 10 5 1))

(define (change amount denomination)
  (define (aux amo den)
    ;;(format #t "~A ~A ~%" amo den)
    (cond ((= 0 amo) 1)
          ((null? den) 0)
          ((> 0 amo) 0)
          (else (+ (aux (- amo (car den))
                        den)
                   (aux amo
                        (cdr den))))))
  (aux amount denomination))

;; (change 100 (first-denomination)) => 292


;; exercise 1.11

(define (f n) ;; recursive
  (if (< n 3) n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

;; 0 1 2 4 11 25 59 142

(define (f n) ;; iterative
  (define (aux m3 m2 m1 count)
    ;;(format #t "~A : ~A ~A ~A~%" count m3 m2 m1)
    (cond ((< n 2) n)
          ((= n count) m1)
          (else (aux m2 m1
                     (+ (* m1 1) (* m2 2) (* m3 3))
                     (+ count 1)))))
  (aux 0 1 2 2))


;; exercise 1.12

(define (pascal row col)
  ;; pascal triangle
  (cond ((= col 0) 1)
        ((= col row) 1)
        (else (+ (pascal (- row 1) col)
                 (pascal (- row 1) (- col 1))))))


;; exercise 1.13

(define fai
  (/ (+ 1 (sqrt 5)) 2))

(define (fib-about n)
  (/ (expt fai n) (sqrt 5)))


;; exercise 1.14

;; ref : 1.2.2

;; >> change 11 (50 25 10 5 1)
;; | change -39 (50 25 10 5 1) => fail
;; | change 11 (25 10 5 1)
;;   | change -14 (25 10 5 1) => fail
;;   | change 11 (10 5 1)
;;     | change 1 (10 5 1)
;;     | | change -9 (10 5 1) => fail
;;     | | change 1 (5 1)
;;     |   | change -4 (1) => fail
;;     |   | change 1 (1)
;;     |     | change 0 (1) => just
;;     |     | change 1 () => fail
;;     | change 11 (5 1)
;;       | change 6 (5 1)
;; ...
;; ...
;; ...


;; exercise 1.15

(define (cube x)
  (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (< (abs angle) 0.1) ;; 0.1 :: threshold
      angle
      (begin
        (format #t "p call~%")
        (p (sine (/ angle 3.0))))))

;; (sine 12.5) => 5 times
;;
;; n is steps and also spaces of sine(a), n such as first n from 0,
;;((|a|*(1/3)^n) < (t::threshold))
;; => (1/3)^n < t/|a|
;; => n > log(1/3, t/|a|)
;; = n > log(3, |a|/t)
;; =>> n = SITA(a)



;; samples 1.2.4

(define (expt-s1 b n)
  (define (iter num count)
    (if (= 0 count) num
        (iter (* num b) (- count 1))))
  (iter 1 n))

(define (expt-s2 b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt-s2 b (/ n 2))))
        (else (* b (expt-s2 b (- n 1))))))

;; remainder :: n mod m === n%m


;; exercise 1.16


;; (expt-s2 1 (expt 100 50000)) => out of memory

;; (define (expt-s2i base num) ;; iterative expt SITA(log n)
;;   (define (iter n current state)
;;     (format #t "~A ~A ~A~%" n current state)
;;     (cond ((= n 0) (* current state))
;;           ((= current 1)
;;            (iter (- n 1) base (* state base)))
;;           ((even? n)
;;            (iter (/ n 2) (square (* current state)) 1))
;;           (else
;;            (iter (- n 1) current (* state base)))))
;;   (iter num 1 1))


;; cheat : http://www.billthelizard.com/2010/01/sicp-exercise-116-fast-exponentiation.html

(define (expt-s2i base num)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter (square b) (/ n 2) a))
          (else (iter  b (- n 1) (* a b)))))
  (iter base num 1))



;; exercise 1.17
(define (*-1 a b)
  (format #t "~A ~A ~%" a b)
  (if (= b 0)
      0
      (+ a (*-1 a (- b 1)))))

(define (*-2 a b)
  (define (iter a-num b-num state)
    (cond ((= b-num 0) state)
          ((even? b-num)
           (iter (* 2 a-num) (/ b-num 2) state))
          (else
           (iter a-num (- b-num 1) (+ state a-num)))))
  (iter a b 0))

;; all 2x have more than once iters.


;; exercise 1.18
;; first n such as , f(_, a) => a*2*(1/2)^n <= 1
;; => (1/2)^n <= 1/(2*a) and first n
;; => n = log (1/2, 1/(2*a))
;; => n = log(2, 2*a)
;; => n = log(a-1)
;; => SITA(log(a))


;; exercise 1.19


(define (fib n)
  (define (iter a b p q count)
    ;; (format #t "~A ~A ~A ~A ~A~%" a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (iter a b
                 (+ (* p p) (* q q))
                 (+ (* 2 p q) (* q q))
                 (/ count 2)))
          (else (iter (+ (* b q) (* a q) (* a p))
                      (+ (* b p) (* a q))
                      p
                      q
                      (- count 1)))))
  (iter 1 0 0 1 n))

;; (map fib '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

