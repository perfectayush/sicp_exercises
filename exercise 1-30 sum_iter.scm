#lang planet neil/sicp
        
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result a))))
  (iter a 0))
