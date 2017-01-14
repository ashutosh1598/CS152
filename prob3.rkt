#lang racket
(define (ak-mult x y)
  (akmult x y y))
(define (akmult x y prod)
  (cond ( (= x 1) prod)
        ( (= (modulo x 2) 0) (akmult (/ x 2) (* y 2) (* prod 2)))
        ( #t (akmult (- x 1) y (+ prod y)))))