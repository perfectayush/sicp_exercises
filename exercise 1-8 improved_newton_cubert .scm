#lang planet neil/sicp
(define (cube-iter guess x)
   (if (good-enough? guess x)
   guess
  (cube-iter (improve guess x) x)))


(define (good-enough? guess x)
  (> 0.000007 (absolute (- (improve guess x) guess))))

(define (absolute x) (if (> x 0) x (- x)))

(define (improve y x) (/ (+ (/ x (* y y)) (* 2 y)) 3))

(define (average a b) (/ (+ a b) 2))


(define (cubert x) (cube-iter 1.0 x))