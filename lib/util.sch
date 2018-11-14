
(define (tabs num)
  (if (> num 0)
      (begin (format #t " ")
             (tabs (- num 1)))))
