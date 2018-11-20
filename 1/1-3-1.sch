
;; samples

(define (cube x)
  (* x x x))

(define (sum-intergers a b)
  (if (> a b)
      0
      (+ a (sum-intergers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 2 2) b))))


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))


(define (identity x) x)

(define (sum-intergers a b)
  (sum identity a inc b))


(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 2 2))
  (sum pi-term a pi-next b))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))


;; exercise 1.29

(define (integral f a b n)
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

;; exercise 1.30

(define (sum term a next b)
  (define (iter a state)
    (if (> a b)
        state
        (iter (next a) (+ state (term a)))))
  (iter a 0))

;; exercise 1.31


(define (product term a next b)
  (if (> a b) 1
      (* (term a)
         (product term (next a) next b))))

(define (product term a next b)
  (define (iter state a-num)
    (if (> a-num b) state
        (iter (* state (term a-num))
              (next a-num))))
  (iter 1 a))

(define (pi)
  (define (pi-next a) (+ a 2))
  (* 4 2
     (/ (product square 4 pi-next 100)
        (product square 3 pi-next 100))
     (/ 100)))


;; exercise 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term  (next a) next b))))

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


;; exercise 1.33

(load "../lib/prime-util.sch") ;; prime?

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

(define (test-filterd-accumulate)
  (filterd-accumulate + 0 identity 0 inc 10 even?))

(define (sum-prime-square a b)
  (define (this-next a) (+ a 2))
  (filterd-accumulate + 0 square a this-next b prime?))

(define (product-less-than-n-and-prime-each-other-n n)
  (define (is-prime-each-other a)
    (= (gcd n a) 1))
  (filterd-accumulate * 1 identity 1 inc n is-prime-each-other))

