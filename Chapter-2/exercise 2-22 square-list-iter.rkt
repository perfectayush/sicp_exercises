#lang planet neil/sicp


(define (square x) (* x x))

(define (myappend list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (myappend (cdr list1) list2))))

(define (square-list1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                      (list (square (car things)))))))

  (iter items nil))

(square-list1 (list 1 2 3 4 5 6))
(square-list2 (list 1 2 3 4 5 6))
(square-list3 (list 1 2 3 4 5 6))
