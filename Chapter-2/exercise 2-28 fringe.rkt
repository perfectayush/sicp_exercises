#lang planet neil/sicp

(define (myappend list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (myappend (cdr list1) list2))))


(define (fringe x)
  (define (fringe-r x y) 
    (cond ((null? x) y)
          ((not (pair? (car x)))  (fringe-r (cdr x) (myappend y (list (car x)))))
          (else                   (fringe-r (cdr x) (myappend y (fringe-r (car x) nil))))))
  (fringe-r x nil))

(define y (list (list 1 2) (list 3 4)))
(define z (list y y))

(display (fringe y))
(display (fringe z))
