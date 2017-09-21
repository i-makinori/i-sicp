(define true #t)
(define false #f)

; 1.2節，問題3.5等で乱数を使う．使い方は下記URL参照
; http://practical-scheme.net/wiliki/schemexref.cgi?SRFI-27
(use srfi-27) 

; 問題1.22
; http://sicp.g.hatena.ne.jp/n-oohira/?word=*%5Bgauche%5D を参考にした
(define (runtime)
    (use srfi-11)
    (let-values (((a b) (sys-gettimeofday)))
    (+ (* a 1000000) b)))

; 2.2.1節
(define nil '())

; 3.5節 遅延評価で使う
(define-macro (delay x) `(lambda () ,x))

; 5章 レジスタ計算機で使う
(define user-initial-environment interaction-environment)
