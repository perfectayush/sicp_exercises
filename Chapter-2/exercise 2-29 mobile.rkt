#lang planet neil/sicp

(define (myappend list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (myappend (cdr list1) list2))))


(define (make-mobile left right) 
  (list left right))

(define (make-branch len structure) 
  (list len structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch) 
  (car (cdr branch)))

(define (branch-weight branch)
  (if (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(define (branch-torque branch) 
  (* (branch-length branch) 
     (branch-weight branch)))

(define (branch-balanced? branch)
  (if (pair? (branch-structure branch))
      (balanced? (branch-structure branch)) 
      #t))

(define (balanced? mobile)
  (and (= (branch-torque (left-branch mobile)) (branch-torque (right-branch mobile)))
       (branch-balanced? (left-branch mobile)) 
       (branch-balanced? (right-branch mobile))))

(define mobile1 (make-mobile 
                    (make-branch 1 2) 
                    (make-branch 3 (make-mobile 
                                    (make-branch 4 5) 
                                    (make-branch 6 7)))))
(define mobile2 (make-mobile 
                    (make-branch 5 9) 
                    (make-branch 15 (make-mobile 
                                    (make-branch 5 2) 
                                    (make-branch 10 1)))))


(total-weight mobile1)
(display "\n")

(total-weight mobile2)
(display "\n")

(balanced? mobile1)
(display "\n")

(balanced? mobile2)
(display "\n")
