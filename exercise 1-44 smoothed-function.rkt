#lang planet neil/sicp

(define dx 0.001)

(define (compose f g)
  (lambda(x) (f (g x))))

(define (repeat f n)
  (if (= 0 n)
      identity (compose f (repeat f (dec n)))))

(define (smooth f) (lambda(x) (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))

(define (smooth-nth f n) ((repeat smooth n) f))

((smooth-nth (lambda(x) (* x x)) 3) 3)
