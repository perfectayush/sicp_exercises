#lang planet neil/sicp

(define (filtered-list lis func)
  (cond
   ((null? lis) nil)
   ((func (car lis))  (cons (car lis) (filtered-list (cdr lis) func)))
   (else (filtered-list (cdr lis) func))))
     
(define (same-parity g . w) (cons g (filtered-list w (if (odd? g) odd? even?)) ))

(same-parity 1 2 3 4 5 6 6 7 7 88 8888 7 7 8)
