#lang planet neil/sicp
        
(define pi 3.1416)

(define (cont-frac-recr n d k) 
  (define (recr-skel n d k i)
  (if (= i k)
      0
      (/ (n i) (+ (d i)  (recr-skel n d k (inc i))))))
  (recr-skel n d k 1))

(define (cont-frac-iter n d k) 
  (define (iter-skel n d k result)
  (if (= k 0)
      result 
      (iter-skel n d (dec k) (/ (n k) (+ (d k) result)))))
  (iter-skel n d k 0))

(define (tan-cf x k) 
  (cont-frac-iter (lambda(i) (if (= i 1) x (* x (- x))))
                  (lambda(i) (- (* 2 i) 1)) 
                  k))

