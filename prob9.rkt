#lang racket
#lang racket
(define (modexp x y n)
  (cond ((= y 0) 1)
        ((= (remainder y 2) 0) (modexp (modulo (* x x) n) (/ y 2)n))
        (#t (modulo (* x (modexp x (- y 1) n)) n))))
(define (is-prime n)
  (define x (random n))
  (cond ((< n 2) #f)
        ( (= x 0) (is-prime n))
        ( (= (modexp x (- n 1) n) 1) #t)
        ( #t #f)))
