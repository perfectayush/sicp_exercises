#lang planet neil/sicp

(define (even? x) (= 0 (remainder x 2)))
(define (square x) (* x x))

(define (fast-exp x n)
  (define (fast-iter exp x n)
  (cond ((= n 0) exp)
        ((even? n) (fast-iter exp (square x) (/ n 2)))
        (else (fast-iter (* x exp) x (dec n)))))
  (fast-iter 1 x n))
