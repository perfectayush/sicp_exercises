#lang planet neil/sicp
(define (count-change amount) (cc amount 5))

(define (cc amount kind-of-coin)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kind-of-coin 0)) 0)
        (else (+
               (cc amount (dec kind-of-coin))
               (cc (- amount (first-denomination kind-of-coin)) kind-of-coin)))))

(define (first-denomination kind-of-coin)
  (cond ((= 1 kind-of-coin) 1)
        ((= 2 kind-of-coin) 5)
        ((= 3 kind-of-coin) 10)
        ((= 4 kind-of-coin) 25)
        ((= 5 kind-of-coin) 50)))