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


;; 
