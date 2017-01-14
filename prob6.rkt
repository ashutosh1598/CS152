#lang racket
(define (modexp x y n)
  (cond ((= y 0) 1)
        ((= (remainder y 2) 0) (modexp (modulo (* x x) n) (/ y 2)n))
        (#t (modulo (* x (modexp x (- y 1) n)) n))))