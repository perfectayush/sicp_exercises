#lang planet neil/sicp

(define (compose f g)
  (lambda(x) (f (g x))))

(define (repeat f n)
  (if (= 0 n)
      identity (compose f (repeat f (dec n)))))

(define (square x) (* x x))

((repeat square 2) 5)                ;;625
