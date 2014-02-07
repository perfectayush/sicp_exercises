#lang planet neil/sicp
        
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (product-recur term a next b)
  (if (> a b)
      1
      (* (term a) (product-recur term (next a) next b))))

(define (approx-pi n)
  (define (pi-term n) (/ (* n (+ n 2)) (* (+ n 1) (+ n 1))))
  (define (pi-next n) (+ n 2))
  (* 4.0 (product-iter pi-term 2 pi-next (* 2 n))))
