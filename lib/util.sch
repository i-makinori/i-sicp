

;; samples 1.3.1

(define (identity x) x)


(define (cube x)
  (* x x x))


(define (inc n) (+ n 1))

(define (dec n) (- n 2))


(define (average a b)
  (/ (+ a b) 2))


;; exercise 1.32

(define (accumulate combiner null-value term a next b)
  (define (iter a state)
    (if (> a b) state
        (iter (next a)
              (combiner (term a) state))))
  (iter a null-value))


(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))


;; exercise 1.29

(define (integral-dx f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (integral-num-sections f a b n)
  (define (delta-x) (/ (- b a) n))
  (define (apply-f k)
    (f (+ a (* k (delta-x)))))
  (define (term  x)
    (cond ((or (= x 0) (= x n))
           (* 1 (apply-f x)))
          ((odd? x)
           (* 4 (apply-f x)))
          (else
           (* 2 (apply-f x)))))
  (* (/ (- b a) n)
     (sum term 0 inc n)
     1/3))


(define (integral f a b dx)
    (integral-dx f a b dx))


;; exercise 1.33

(define (filterd-accumulate combiner null-value term a next b filter)
  (define (iter a state)
    (cond ((> a b) state)
          ((filter a)
           (iter (next a)
                 (combiner (term a) state)))
          (else
           (iter (next a)
                 state))))
  (iter a null-value))



;; great common divisor
;; samples 1.2.5
(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))



;; chapter 1.3.3
;; fixed-point

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))



;; display

(define (display-params params-list)
  (define (iter list)
    (cond ((null? list) (format #t "~%"))
          (else
           (format #t " ~A" (car list))
           (iter (cdr list)))))
  (iter params-list))

(define (tabs num)
  (if (> num 0)
      (begin (format #t " ")
             (tabs (- num 1)))))
