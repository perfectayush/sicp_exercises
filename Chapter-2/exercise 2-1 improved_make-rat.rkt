#lang planet neil/sicp

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d) 
  (cond ((and (> (* n d) 0) (< n 0)) (cons (- n) (- d)))
        ((and (< (* n d) 0) (< d 0)) (cons (- n) (- d)))
        (else (cons n d))))

(print-rat (make-rat 1 2))
(print-rat (make-rat -1 2))
(print-rat (make-rat 1 -2))
(print-rat (make-rat -1 -2))
