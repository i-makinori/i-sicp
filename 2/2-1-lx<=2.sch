
;; samples 2.1.1

(define make-rat
  cons)
(define numer car)
(define denom cdr)


(define (add-rat x y)
  "n1/d1+n2/d2 = (n1*d2+n2*d1) / d1*d2"
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  "n1/d1-n2/d2 = (n1*d2-n2*d1) / d1*d2"
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  "(n1/d1)*(n2/d2) = (n1*n2) / (d1*d2)"
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  "(n1/d1)/(n2/d2) = (n1*d2) / (d1*n2)"
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  "x=y == x(n)/x(d)=y(n)/y(d) => x(n)*y(d)=x(d)*y(n)"
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))


(define (print-rat x)
  (format #t "~A/~A~%" (numer x) (denom x))
  (format #f "~A/~A" (numer x) (denom x)))


;; great common divisor
;; samples 1.2.5
(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;; tests for rat

(define one-half (make-rat 1 2))

(define one-third (make-rat 1 3))

;; +
(print-rat (add-rat one-half one-third)) ;; 5/6
;; -
(print-rat (sub-rat one-half one-third)) ;; 1/6
(print-rat (sub-rat one-third one-half)) ;; 1/6
(print-rat (sub-rat one-third one-third)) ;; 0/1
;; *
(print-rat (mul-rat one-half one-third)) ;; 1/6
;; /
(print-rat (div-rat one-half one-third)) ;; 3/2



;; exercise 2.1

(define (make-rat n d)
  (let ((sign (cond ((and (positive? n) (positive? d)) +1)
                    ((and (negative? n) (negative? d)) +1)
                    (else -1)))
        (g (gcd (abs n) (abs d))))
    (cons (/ n g) (/ d g))))



