#lang planet neil/sicp

(define (avg x y) (/ (+ x y) 2))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p) 
  (newline) 
  (display "(") 
  (display (x-point p)) 
  (display ",") 
  (display (y-point p))
  (display ")"))


(define (make-segment p q) (cons p q))
(define (start-segment l) (car l))
(define (end-segment l) (cdr l))

(define (mid-segment l) 
  (let 
      ((p (start-segment l))
       (q (end-segment l)))
    (make-point 
     (avg (x-point p) (x-point q))
     (avg (y-point p) (y-point q)))))

(print-point  (mid-segment (make-segment (make-point 2 2) (make-point 3 3))))
