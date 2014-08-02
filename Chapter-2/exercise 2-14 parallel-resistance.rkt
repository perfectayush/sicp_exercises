#lang planet neil/sicp

(define (make-interval a b) (cons (inexact->exact a) (inexact->exact b)))
(define (make-center-width c w) (make-interval (- c w) (+ c w)))
(define (make-center-percent c p) (make-interval (- c (* (/ p 100.0) c)) (+ c (* (/ p 100.0) c))))

(define (display-interval x) (display (cons (exact->inexact (lower-bound x))   
                                          (exact->inexact (upper-bound x)))))

(define (lower-bound x) (inexact->exact (car x)))
(define (upper-bound x) (inexact->exact (cdr x)))

(define (center i) (/ (+ (upper-bound i)(lower-bound i)) 2.0))
(define (width i) (/ (- (upper-bound i) (lower-bound i)) 2.0))

(define (percent i) (* 100 (/ (width i) (center i))))

(define (mul-approx a b) (make-center-percent (* (center a) (center b)) (+ (percent a) (percent b))))

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
        (p4 (* (upper-bound x) (upper-bound y))))
    (if (and (span-zero? x) (span-zero? y))
        (let
           ((p2 (* (lower-bound x) (upper-bound y)))
            (p3 (* (upper-bound x) (lower-bound y))))
          (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4)))
        (make-interval (min p1 p4) (max p1 p4)))))

(define (span-zero? y) 
  (and (<= 0 (upper-bound y)) 
      (>= 0 (lower-bound y))))

(define (div-interval x y)
  (mul-interval x 
                (if  (span-zero? y)
                     (display "zero spanerror")
                     (make-interval (/ 1.0 (upper-bound y))
                                    (/ 1.0 (lower-bound y))))))

(define (par1 r1 r2) (div-interval (mul-interval r1 r2) (add-interval r1 r2)))

(define (par2 r1 r2) 
  (let 
      ((one (make-interval 1 1)))  
    (div-interval one (add-interval 
                       (div-interval one r1)
                       (div-interval one r2)
                       ))))


(define a (make-center-percent 10 1))
(define b (make-center-percent 20 2))

(define c (div-interval a b))
(define d (div-interval a a))


(define apar1a (par1 a a))
(define apar2a (par2 a a))


(define apar1b (par1 a b))
(define apar2b (par2 a b))

(display "a parallel1 a:")
(display-interval apar1a)
(display "\n")

(display "a parallel2 a:")
(display-interval apar2a)
(display "\n")



(display "a parallel1 b:")
(display-interval apar1b)
(display "\n")

(display "a parallel2 b:")
(display-interval apar2b)
(display "\n")
