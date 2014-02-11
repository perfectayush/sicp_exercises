#lang planet neil/sicp

(define tolerance 0.0000001)
(define dx 0.00001)

(define (close-enough? a b)
  (> tolerance (abs (- b a))))

(define (fixed-point function guess)
  (let ((newguess (function guess)))
    (if (close-enough? guess newguess)
        newguess
        (fixed-point function newguess))))

(define (average-damp f) (lambda(x) (/ (+ x (f x)) 2)))

(define (compose f g)
  (lambda(x) (f (g x))))

(define (repeat f n)
  (if (= 0 n)
      identity (compose f (repeat f (dec n)))))

(define (log2 x) (/ (log x) (log 2)))

define (nth-root x n) 
  (fixed-point ((repeat average-damp (floor (log2 n))) 
                (lambda(y) (/ x (expt y (- n 1))))) 
               1.0))

(nth-root 32 5)
