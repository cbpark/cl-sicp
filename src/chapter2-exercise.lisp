(in-package :cl-user)
(defpackage cl-sicp.chapter2-exercise
  (:use :cl
        :cl-sicp.chapter2))
(in-package :cl-sicp.chapter2-exercise)

;;; Exercise 2.1

(defun make-rat (n d)
  (let ((g (gcd n d)))
    (if (< d 0)
        (cons (/ (* n -1) g) (/ (* d -1) g))
        (cons (/ n g) (/ d g)))))

;;; Exercise 2.2

(defun make-segment (x y)
  (cons x y))

(defun start-segment (x)
  (car x))

(defun end-segement (y)
  (cdr y))

(defun make-point (x y)
  (cons x y))

(defun x-point (x)
  (car x))

(defun y-point (y)
  (cdr y))

(defun midpoint-segment (s)
  (make-point
   (/ (+ (x-point (start-segment s)) (x-point (end-segement s))) 2)
   (/ (+ (y-point (start-segment s)) (y-point (end-segement s))) 2)))

(defun print-point (p)
  (format t "~a~a,~a~a~%" "(" (x-point p) (y-point p) ")"))

;;; Exercise 2.3

(defun make-rect (x y)
  (cons x y))

(defun rect-width (r)
  (abs (- (x-point (car r)) (x-point (cdr r)))))

(defun rect-height (r)
  (abs (- (y-point (car r)) (y-point (cdr r)))))

(defun rect-perimeter (r)
  (* 2 (+ (rect-width r) (rect-height r))))

(defun rect-area (r)
  (* (rect-width r) (rect-height r)))

;;; Exercise 2.4

(defun my-cons (x y)
  #'(lambda (m)
      (funcall m x y)))

(defun my-car (z)
  (funcall z #'(lambda (p q)
                 (declare (ignore q))
                 p)))

(defun my-cdr (z)
  (funcall z #'(lambda (p q)
                 (declare (ignore p))
                 q)))

;;; Exercise 2.5

(defun my-cons2 (a b)
  (* (expt 2 a)
     (expt 3 b)))

(defun my-car2 (x)
  (num-divs x 2))

(defun my-cdr2 (x)
  (num-divs x 3))

(defun num-divs (n d)
  (labels ((iter (x result)
             (if (= 0 (rem x d))
                 (iter (/ x d) (1+ result))
                 result)))
    (iter n 0)))

;;; Exercise 2.6

(defparameter *zero* (lambda (f)
                       (declare (ignore f))
                       (lambda (x) x)))

(defun add-1 (n)
  (lambda (f)
    (lambda (x)
      (funcall f (funcall (funcall n f) x)))))

(defparameter *one* (lambda (f)
                      (lambda (x)
                        (funcall f x))))

(defparameter *two* (lambda (f)
                      (lambda (x)
                        (funcall f (funcall f x)))))

(defun add (m n)
  (lambda (f)
    (lambda (x)
      (funcall (funcall m f) (funcall (funcall n f) x)))))

;;; Exercise 2.7

(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defun make-interval (a b)
  (cons a b))

(defun upper-bound (x)
  (cdr x))

(defun lower-bound (x)
  (car x))

;;; Exercise 2.8

(defun sub-interval (x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;;; Exercise 2.9

(defun width-interval (x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

;;; Exercise 2.10

(defun div-interval (x y)
  (if (and (<= (lower-bound y) 0) (>= (upper-bound y) 0))
      (error "The interval spans zero.")
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y))))))

;;; Exercise 2.11

(defun fast-mul-interval (x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((> lx 0)
           (cond ((> ly 0) (make-interval (* lx ly) (* ux uy)))
                 ((< uy 0) (make-interval (* ux ly) (* lx uy)))
                 (t        (make-interval (* ux ly) (* uy ux)))))
          ((> ly 0)
           (cond ((< ux 0) (make-interval (* uy lx) (* ly ux)))
                 (t        (make-interval (* lx uy) (* ux uy)))))
          ((< ux 0)
           (cond ((< uy 0) (make-interval (* ux uy) (* lx ly)))
                 (t        (make-interval (* lx uy) (* lx ly)))))
          ((< uy 0)
           (make-interval (* ux ly) (* lx ly)))
          (t
           (let ((p1 (* (lower-bound x) (lower-bound y)))
                 (p2 (* (lower-bound x) (upper-bound y)))
                 (p3 (* (upper-bound x) (lower-bound y)))
                 (p4 (* (upper-bound x) (upper-bound y))))
             (make-interval (min p1 p2 p3 p4)
                            (max p1 p2 p3 p4)))))))

(defun make-center-width (c w)
  (make-interval (- c w) (+ c w)))

(defun center (i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))

(defun width (i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

;;; Exercise 2.12

(defun make-center-percent (c p)
  (make-center-width c (* c (/ p 100.0))))

(defun percent (i)
  (* 100.0 (/ (width i) (center i))))

;;; Exercise 2.13

(defun par1 (r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(defun par2 (r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
