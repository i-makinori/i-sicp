
;; samples
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 2 2))
       b))

#| 
some functions
|#

;; exercise 1.34

(define (f g)
  (g 2))

(f square)

(f (lambda (z) (* z (+ z 1))))

(f f)

;; error
;; 2 is not applicable

;; (g 2) 2
;; ((lambda (g) ((g 2) 2)) (lambda (x) (lambda (y) (+ x y))))
