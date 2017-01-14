#lang racket
(define (gcd a b)
  (if (= b 0) a
      (gcd b (modulo a b))))
(define (has-solution a b c)
  (if (= (modulo c (gcd a b)) 0) #t
      #f))
