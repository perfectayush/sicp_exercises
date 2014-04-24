#lang planet neil/sicp

(define zero (lambda(f) (lambda(x) x)))
(define (add-1 n) (lambda (f) (lambda(x) (f ((n f) x)))))

(define one (lambda(f) (lambda(x) (f x))))
(define two (lambda(f) (lambda(x) (f (f x)))))

(define (add-church a b) (lambda(f) (lambda(x) ((a f) ((b f) x))) ))

(define three (add-church one two))
(define four (add-church two two))
(define five (add-church two three))
(define six (add-church one five))
(define ten (add-church five five))


((zero inc) 0)
((one inc) 0)
((two inc) 0)
((three inc) 0)
((five inc) 0)
((six inc) 0)
((ten inc) 0)
