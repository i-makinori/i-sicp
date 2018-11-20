
(load "../lib/util.sch")

(define (search f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        mid-point
        (let ((test-value (f mid-point)))
          (cond ((positive? test-value)
                 (search f neg-point mid-point))
                ((negative? test-value)
                 (search f mid-point pos-point))
                (else mid-point))))))

(define (close-enough? a b)
  (let ((threshold 0.001))
    (< (abs (- a b)) threshold)))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      0.0
                      2.0)


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

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)


(define (sqrt_1 x)
  ;; y^2 = x => y = x/y
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2))
               1.0))



;; exercise 1.35

(define (golden-ratio)
  ;; all (fai^2 = fai + 1)
  ;; -> fai = 1+1/fai
  (fixed-point (lambda (y) (+ 1 (/ 1 y)))
               1.0))


;; exercise 1.36

(define (fixed-point f first-guess)
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


(define (x^x=1000)
  ;; x^x = 1000
  ;; => log x 1000 = x
  ;; => f(x) = x
  (fixed-point (lambda (x) ;; (+ 1 (/ 1 x)))
                 (/ (log 1000) (log x)))
               2.0))


;; exercise 1.37

(define (cont-frac n d k)
  (define (recur i)
    (cond ((> i k) 0)
          (else (/ (n i)
                   (+ (d i) (recur (+ i 1)))))))
  (recur 1))


(define (cont-frac n d k)
  (define (iter i state)
    ;; (format #t "~A ~A~%" i state)
    (cond ((<= i 0) state)
          (else
           (iter (- i 1)
                 (/ (n i)
                    (+ (d i) state))))))
  (iter k 0))

(define (golden-ratio_2 k)
  (/ (cont-frac (lambda (i) 1.00)
                (lambda (i) 1.00)
                k)))

(define (k-to-acurate-enough val func)
  (define (acurate-enough? v1 v2)
    (< (abs (- v1 v2)) 0.0001))
  (define (iter count)
    (if (acurate-enough? val (func count)) count
        (iter (+ count 1))))
  (iter 2))

(define (k-golden-ratio)
  (k-to-acurate-enough (golden-ratio) golden-ratio_2))

;; => 12


;; exercise 1.38

(define (natural-log-base)
  (+ 2
     (cont-frac (lambda (n) 1)
                (lambda (n)
                  (cond ((= (remainder n 3) 2)
                         (* 2/3 (+ n 1)))
                        (else 1)))
                10000)))

;; (exact->inexact (natural-log-base)))
;; Value: 2.718281828459045


;; exercise 1.39

(define (cont-frac- n d k)
  (define (iter i state)
    ;; (format #t "~A ~A~%" i state)
    (cond ((<= i 0) state)
          (else
           (iter (- i 1)
                 (/ (n i)
                    (- (d i) state))))))
  (iter k 0))

(define (tan-cf x k)
  (/ (cont-frac- (lambda (i) (square x))
                 (lambda (i) (- (* 2 i) 1))
                 k)
     x))

;; (exact->inexact (tan-cf 3 1000))
;; Value: -.1425465430742778

;; (tan 3)
;;Value: -.1425465430742778







