#lang planet neil/sicp

(define (last-pair lis) 
  (if (null? (cdr lis))
      (car lis) 
      (last-pair (cdr lis))))

(last-pair (list 1 2 3 4 5 6 7))
(last-pair (list 99 21 12))
