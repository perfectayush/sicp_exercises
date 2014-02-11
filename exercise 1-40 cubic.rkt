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

(define (derive g)
  (lambda(x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g) 
  (lambda(x) (- x (/ (g x) ((derive g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda(x) (+ (* x x x) (* a x x) (* b x) c) ))

(newtons-method (cubic 1 1 1) 1)
