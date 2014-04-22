#lang planet neil/sicp

(define (cons a b) (* (expt 2 a) (expt 3 b)))

(define (pow-x a x num) 
  (if (= 0 (remainder num a)) 
      (pow-x a (inc x) (/ num a)) 
      x))

(define (car z) (pow-x 2 0 z))
(define (cdr z) (pow-x 3 0 z))

(car (cons 5 4))
(cdr (cons 4 5))
