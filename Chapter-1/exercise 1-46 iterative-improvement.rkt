#lang planet neil/sicp

(define tolerance 0.00001)

(define (close-enough? a b)
  (> tolerance (abs (- b a))))

(define (iter-improv good-enough? improve)
  (define (skel guess) 
    (let ((newguess (improve guess)))
      (if (good-enough? guess newguess)
          newguess
          (skel newguess))))
  skel)

(define (fixed-point function guess) ((iter-improv close-enough? function) guess))

(define (sqrt x) ((iter-improv close-enough? (lambda(guess) (/ (+ guess (/ x guess)) 2))) 1.00))


(fixed-point (lambda(y) (+ (cos y) (sin y))) 1.00) ;1.2587315962971173

(sqrt 4)                                      ;2.000000000000002
