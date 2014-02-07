#lang planet neil/sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (simpson-rule func a b n)
  (define h (/ (- b a) n))
  (define (simpson-next x) (+ x h))
  (define (simpson-term x) 
    (let ((k (* n (/ (- x a) (- b a))))) 
             (* (func x) 
                (cond
                ((or (= k 0) (= k n)) 1)
                ((odd? k) 4)
                ((even? k)2)))))
  (* (/ h 3) (sum simpson-term a simpson-next b)))
  ;(display (* n (/ (- (+ h h h a) a) (- b a)))))

(define (cube x) (* x x x))
(simpson-rule cube 0 1 8)