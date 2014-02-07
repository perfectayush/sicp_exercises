#lang planet neil/sicp
(define (sqrt-iter guess x)
   (if (good-enough? guess x)
   guess
  (sqrt-iter (improve guess x) x)))


(define (good-enough? guess x)
  (> 0.000007 (absolute (- (improve guess x) guess))))

(define (absolute x) (if (> x 0) x (- x)))

(define (improve guess x) (average guess (/ x guess)))

(define (average a b) (/ (+ a b) 2))


(define (sqrt x) (sqrt-iter 1.0 x))