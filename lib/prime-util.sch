


;; prime test

;; exercise 1.28

;; ref : https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test

(define (mirabiler-recursion n r d) ;; miller rabin prime test
  (define (x-test x r-count ago)
    ;; (format #t " :: ~A~%" x)
    (cond ((= x 1) false)
          ((<= r-count 0) false)
          ((= x (- n 1)) true)
          (else (x-test (remainder (square x) n) (- r-count 1) x))))
  (define (try-it  x)
    (cond ((or (= x 1) (= x (- n 1)))
           true)
          (else
           (x-test (remainder (square x) n) r n))))
  (try-it
   (remainder (expt (+ 2 (random (- n 2 1)))
                    d)
              n)))

(define (find-d num state)
  (cond ((< num state)
         (format #t "fail to find D : num ~A < state ~A~%" num state))
        ((and (= 0 (remainder num state)) (odd? (/ num state)))
         (cons (/ num state)
               (/ (log state) (log 2))))
        (else
         (find-d num (* state 2)))))

(define (mirabiler-prime? n times)
  (define (d-tuple)
    (find-d (- n 1) 2))
  (define (call-mirabiler d r time)
    (cond ((= time 0) true)
          ((mirabiler-recursion n r d)
           (call-mirabiler
            d r
            (- time 1)))
          (else false)))
  (cond ((= n 1) false)
        ((or (= n 2) (= n 3)) true)
        ((even? n) false)
        ;;
        (else
         (call-mirabiler (car (d-tuple))
                         (cdr (d-tuple))
                         times))))

(define (prime? num)
  (mirabiler-prime? num 3))

