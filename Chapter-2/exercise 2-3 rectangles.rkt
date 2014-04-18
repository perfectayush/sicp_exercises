#lang planet neil/sicp

(define (avg x y) (/ (+ x y) 2))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p) 
  (newline) 
  (display "(") 
  (display (x-point p)) 
  (display ",") 
  (display (y-point p))
  (display ")"))


(define (make-segment p q) (cons p q))
(define (start-segment l) (car l))
(define (end-segment l) (cdr l))

(define (mid-segment l) 
  (let 
      ((p (start-segment l))
       (q (end-segment l)))
    (make-point 
     (avg (x-point p) (x-point q))
     (avg (y-point p) (y-point q)))))


;; Rectangle defined using opposite corner
(define (make-rect-opp-corner p q) (cons p q))
(define (rcorner1 r) (car r))
(define (rcorner2 r) (cdr r))
(define (rcorner3 r) 
  (let ((w (rect-width-opp-corner r))
        (x (x-point (rcorner2 r)))
        (y (y-point (rcorner2 r))))
    (make-point (+ x w) y)))

(define (rcorner4 r) 
  (let ((h (rect-height-opp-corner r))
        (x (x-point (rcorner2 r)))
        (y (y-point (rcorner2 r))))
    (make-point x (+ h y))))

(define (rect-width-opp-corner r)
  (- (x-point (rcorner1 r)) (x-point (rcorner2 r))))


(define (rect-height-opp-corner r)
  (- (y-point (rcorner1 r)) (y-point (rcorner2 r))))

(define (perimeter-rect-opp-corner rect) 
  (* 2  (+ (abs (rect-width-opp-corner rect)) 
           (abs (rect-height-opp-corner rect)))))

(define (area-rect-opp-corner rect)
  (abs  (* (rect-height-opp-corner rect) 
           (rect-width-opp-corner rect))))

(define (print-rectangle r)
  (newline)
  (display "(rectangle ") 
  (print-point (rcorner1 r))
  (display ",") 
  (print-point (rcorner2 r))
  (display ",") 
  (print-point (rcorner3 r))
  (display ",") 
  (print-point (rcorner4 r))
  (display ")")
  (newline))

;; test rectangle definition
(define sample-rect (make-rect-opp-corner (make-point 0 0) (make-point 3 7)))
(area-rect-opp-corner sample-rect)
(perimeter-rect-opp-corner sample-rect)
(print-rectangle sample-rect)

;; Rectangle defined using one corner and height/width
(define (make-rect-alt a h w) (cons a (cons h w)))


(define (rcorner1-alt r) (car r))

(define (rcorner2-alt r) 
  (let ((x (x-point (rcorner1-alt r)))
        (y (y-point (rcorner1-alt r))) 
        (h (rect-height-alt r)))
    (make-point x (+ y h))))

(define (rcorner3-alt r) 
  (let ((x (x-point (rcorner1-alt r)))
        (y (y-point (rcorner1-alt r))) 
        (w (rect-width-alt r)))
    (make-point (+ x w) y )))

(define (rcorner4-alt r) 
  (let ((x (x-point (rcorner1-alt r)))
        (y (y-point (rcorner1-alt r))) 
        (w (rect-width-alt r))
        (h (rect-height-alt r)))
    (make-point (+ x w) (+ y h))))

(define (rect-height-alt r) (car (cdr r)))
(define (rect-width-alt r) (cdr (cdr r)))

(define (rect-area-alt r) (abs (* (rect-height-alt r) (rect-width-alt r))))
(define (rect-perimeter-alt r) (* 2 (+ (abs (rect-height-alt r)) (abs (rect-width-alt r)))))

(define (print-rectangle-alt r)
  (newline)
  (display "(rectangle ") 
  (print-point (rcorner1-alt r))
  (display ",") 
  (print-point (rcorner2-alt r))
  (display ",") 
  (print-point (rcorner3-alt r))
  (display ",") 
  (print-point (rcorner4-alt r))
  (display ")")
  (newline))

;; test alternate rectangle definition
(define sample-rect-alt (make-rect-alt (make-point 0 0) 3 7))
(rect-area-alt sample-rect-alt)
(rect-perimeter-alt sample-rect-alt)
(print-rectangle-alt sample-rect-alt)
