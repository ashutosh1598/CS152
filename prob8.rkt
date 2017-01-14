#lang racket
(define (coeffs a b)
  (cond ( (= b 0) (cons 1 0))
        ( #t
          (let ((x  (coeffs b (modulo a b))))
          (cons (cdr x) (- (car x) (* (quotient a b) (cdr x))))))))
(define (inverse e n)
  (define x (car (coeffs e n)))
  (define y (cdr (coeffs e n)))
 (if (= (+ (* e x) (* n y)) 0)
  (modulo (+ x n) n)
  -1
  ))