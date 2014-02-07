#lang planet neil/sicp
        
(define (filtered-accumulate-iter combiner null-value filter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (if (filter a) (term a) null-value)))))
  (iter a null-value))

(define (filtered-accumulate-recur combiner null-value filter term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter a) (term a) null-value) (filtered-accumulate-recur combiner null-value term (next a) next b))))


(define (next x) (if (= x 2) 
                     3
                     (+ x 2)))
                  
(define (square x) (* x x))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
 (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n) (= n (smallest-divisor n)))
     
(filtered-accumulate-iter + 0 prime? square 0 inc 10)
