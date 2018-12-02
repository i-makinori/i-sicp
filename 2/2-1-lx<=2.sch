
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


;; exercise 2.2

(define (point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))


(define (make-segment point1 point2)
  (cons point1 point2))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (make-segment
   (* 1/2 (+ (x-point (start-segment segment)) (x-point (end-segment segment))))
   (* 1/2 (+ (y-point (start-segment segment)) (y-point (end-segment segment))))))

(define (print-point point)
  (format #t "(~A, ~A)~%" (x-point point) (y-point point))
  (format #f "(~A, ~A)" (x-point point) (y-point point)))

;;test

(define point-1 (point 1 8))
(define point-2 (point -3 5))
(define point-3 (point -0.5 -1/100))

(define segment-1 (make-segment point-1 point-2))
(start-segment segment-1)
(end-segment segment-1)
(midpoint-segment segment-1)


;; something

;; interpretation 1
(define (length-segment segment)
  (sqrt
   (+ (square (- (x-point (start-segment segment)) (x-point (end-segment segment))))
      (square (- (y-point (start-segment segment)) (y-point (end-segment segment)))))))

(length-segment segment-1) ;; (dx=3, dy=4) => 5


(define (triangle p1 p2 p3)
  (cons p1 (cons p2 p3)))

(define (p1-triangle triangle)
  (car triangle))
(define (p2-triangle triangle)
  (cadr triangle))
(define (p3-triangle triangle)
  (cddr triangle))

#|
(define (def-segment-func segment proc)
  ;; proc :: (lambda (p1-x p1-y p2-x p2-y) &procedure)
  ((proc)
   (x-point (start-segment segment)) (x-point (end-segment segment))
   (x-point (start-segment segment)) (y-point (end-segment segment))))
|#
#|
(define (length-segment segment)
  (def-segment-func segment
    (lambda (p1x p1y p2x p2y)
      (+ (square (- p1x p2x)
         (square (- p1y p2y)))))))
|#


;; exercise 2.3

(define (make-rectangle cent-x cent-y width height)
  (make-segment (point cent-x cent-y)
                (point width height)))

(define (cent-x-rectangle rectangle)
  (x-point (start-segment rectangle)))
(define (cent-y-rectangle rectangle)
  (y-point (start-segment rectangle)))
(define (width-rectangle rectangle)
  (x-point (end-segment rectangle)))
(define (height-rectangle rectangle)
  (y-point (end-segment rectangle)))


(define (perimeter rectangle)
  (+ (* 2 (width-rectangle rectangle))
     (* 2 (height-rectangle rectangle))))

(define (area rectangle)
  (* (width-rectangle rectangle)
     (height-rectangle rectangle)))


(define (make-rectangle-ps-pe point-start point-end)
  (make-rectangle (* 1/2 (+ (x-point point-start) (x-point point-end)))
                  (* 1/2 (+ (y-point point-start) (y-point point-end)))
                  (abs (- (x-point point-start) (x-point point-end)))
                  (abs (- (y-point point-start) (y-point point-end)))))

;; test
(define rect-1 (make-rectangle 5 8 2 4))
(perimeter rect-1) ;; 2*2+2*4 = 12
(area rect-1) ;; 2*4
(define rect-2 (make-rectangle-ps-pe (point 6 10) (point 4 6)))
(perimeter rect-2) ;; 2*2+2*4 = 12
(area rect-2) ;; 2*4






