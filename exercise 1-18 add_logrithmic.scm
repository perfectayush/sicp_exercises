#lang planet neil/sicp

(define (even? x) (= 0 (remainder x 2)))
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast-multiply a b)
  (define (fast-iter prod a b)
    (cond ((= b 0) prod)
          ((even? b) (fast-iter prod (double a) (halve b)))
          (else (fast-iter (+ a prod) a (dec b)))))
  (fast-iter 0 a b))
