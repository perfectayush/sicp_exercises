#lang planet neil/sicp


(define (myfor-each proc items)
   (proc (car items))
   (if (not (null? (cdr items)))
       (myfor-each proc (cdr items))))

(myfor-each 
 (lambda (x) 
   (display x)(newline)) 
 (list 1 2 3 4 5 6 7))
