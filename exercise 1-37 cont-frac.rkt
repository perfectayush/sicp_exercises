#lang planet neil/sicp
        
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

(/ 1 (cont-frac-iter (lambda(i) 1.0) (lambda(i) 1.0) 10))
(/ 1 (cont-frac-recr (lambda(i) 1.0) (lambda(i) 1.0) 11))
