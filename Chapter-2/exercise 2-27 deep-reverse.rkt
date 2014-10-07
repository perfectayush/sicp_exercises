#lang planet neil/sicp

(define (myreverse lis) 
  (define (myreverse-recur x y)
    (if (null? x)
        y
        (myreverse-recur (cdr x) (cons (car x) y))))
  (myreverse-recur lis nil))

(define (deep-reverse x) 
  (cond ((pair? x) (myreverse (cons (deep-reverse (car x)) (deep-reverse (cdr x))))) 
        (else x)))
  
(define y (list (list 1 2) (list 3 4)))
(display  (deep-reverse y))
