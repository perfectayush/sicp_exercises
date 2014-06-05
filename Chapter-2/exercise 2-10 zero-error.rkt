#lang planet neil/sicp

(define (make-interval a b) (cons (inexact->exact a) (inexact->exact b)))
(define (lower-bound x) (inexact->exact (car x)))
(define (upper-bound x) (inexact->exact (cdr x)))

(define (add-interval x y) 
  (make-interval
   (+ (lower-bound x) (lower-bound y))
   (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y) 
  (make-interval
   (- (lower-bound x) (lower-bound y))
   (- (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (span-zero? y) 
  (or (<= 0 (upper-bound y)) 
      (>= 0 (lower-bound y))))

(define (div-interval x y)
  (mul-interval x 
                (if  (span-zero? y)
                     (display "zero span0error")
                     (make-interval (/ 1.0 (upper-bound y))
                                    (/ 1.0 (lower-bound y))))))

(define (width x) (/ (- (upper-bound x) (lower-bound x)) 2.0))

(define interval1 (make-interval 6.12 7.48))

(define interval2 (make-interval 2.58 2.97))
