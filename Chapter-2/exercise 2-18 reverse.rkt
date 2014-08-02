#lang planet neil/sicp

(define (myreverse lis) 
  (define (myreverse-recur x y)
    (if (null? x)
        y
        (myreverse-recur (cdr x) (cons (car x) y))))
  (myreverse-recur lis nil))

(list 1 2 3 4 5 6 7)
(myreverse (list 1 2 3 4 5 6 7))
