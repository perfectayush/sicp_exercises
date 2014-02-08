#lang planet neil/sicp
        
(define tolerance 0.0000001)
(define (close-enough? a b)
  (> tolerance (abs (- b a))))

(define (fixed-point function guess)
  (let ((newguess (function guess)))
    (if (close-enough? guess newguess)
        newguess
        (fixed-point function newguess))))

(define golden-ratio
  (fixed-point (lambda(x) (+ 1 (/ 1 x))) 1.0))


