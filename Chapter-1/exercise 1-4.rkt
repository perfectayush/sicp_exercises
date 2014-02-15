#lang planet neil/sicp
(define (max3 a b c) 
  (cond ((and (> a b) (> a c)) a)
        ((and (> b a) (> b c)) b)
        (else c)))

(define (max2 a b) (if (> a b) a b))
(define (square x) (* x x))
(define (sum-of-squares x y ) (+ (* x x) (* y y )))

(define (sos-of-maximas a b c)
  (cond ((= a (max3 a b c)) (sum-of squares a (max2 b c)))
        ((= b (max3 a b c)) (sum-of squares b (max2 a c)))
        (else (sum-of-squares c (max2 a b)))))


(sos-of-maximas 2 3 4)


