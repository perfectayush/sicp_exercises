#lang planet neil/sicp

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

(define (timed-prime-test n)
  (newline) (display n) (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ") (display elapsed-time))

(define (search-for-primes n1 n2)
  (define (sfp n1 n2 t)
    (cond 
      ((prime? n1) (timed-prime-test n1) (sfp (inc n1) n2 (inc t)))
      ((or (>= n1 n2) (= t 3)) (display ""))
      (else (sfp (inc n1) n2 t))))
  (sfp n1 n2 0))
      