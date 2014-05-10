(in-package :cl-user)
(defpackage cl-sicp.chapter2-exercise
  (:use :cl
        :cl-sicp.chapter2)
  (:import-from :cl-sicp.chapter1
                :square))
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

;;; Exercise 2.17

(defun last-pair (items)
  (if (null (cdr items))
      (car items)
      (last-pair (cdr items))))

;;; Exercise 2.18

(defun my-reverse (items)
  (if (null (cdr items))
      items
      (append (my-reverse (cdr items)) (list (car items)))))

;;; Exercise 2.19

(defparameter *us-coins* (list 50 25 10 5 1))

(defparameter *uk-coins* (list 100 50 20 10 5 2 1 0.5))

(defun cc (amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more coin-values)) 0)
        (t (+ (cc amount
                  (except-first-denomination coin-values))
              (cc (- amount
                     (first-denomination coin-values))
                  coin-values)))))

(defun no-more (coin-values)
  (if (null coin-values)
      t
      nil))

(defun except-first-denomination (coin-values)
  (cdr coin-values))

(defun first-denomination (coin-values)
  (car coin-values))

;;; Exercise 2.20

(defun same-parity (x &rest lists)
  (labels ((iter (items result)
             (if (null items)
                 result
                 (iter (cdr items) (if (= 0 (rem (+ x (car items)) 2))
                                       (append result (list (car items)))
                                       result)))))
    (iter lists (list x))))

;;; Exercise 2.21

(defun square-list (items)
  (if (null items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(defun square-list-map (items)
  (mapcar #'square items))

;;; Exercise 2.22

(defun square-list-reverse (items)
  (labels ((iter (things answer)
             (if (null things)
                 answer
                 (iter (cdr things)
                       (cons (square (car things)) answer)))))
    (iter items nil)))

(defun square-list-wrong (items)
  (labels ((iter (things answer)
             (if (null things)
                 answer
                 (iter (cdr things)
                       (cons answer (square (car things)))))))
    (iter items nil)))

(defun square-list-iter (items)
  (labels ((iter (things answer)
             (if (null things)
                 answer
                 (iter (cdr things)
                       (append answer (list (square (car things))))))))
    (iter items nil)))

;;; Exercise 2.23

(defun for-each (proc items)
  (if (null items)
      '()
      (progn (funcall proc (car items))
             (for-each proc (cdr items)))))

;;; Exercise 2.27

(defun deep-reverse (items)
  (cond ((null items)        '())
        ((consp (car items)) (append (deep-reverse (cdr items))
                                     (list (deep-reverse (car items)))))
        (t                   (append (deep-reverse (cdr items))
                                     (list (car items))))))

;;; Exercise 2.28

(defun fringe (tree)
  (cond ((null tree)        '())
        ((not (consp tree)) (list tree))
        (t                  (append (fringe (car tree))
                                    (fringe (cdr tree))))))

;;; Exercise 2.29

(defun make-mobile (left right)
  (list left right))

(defun make-branch (length structure)
  (list length structure))

(defun left-branch (mobile)
  (car mobile))

(defun right-branch (mobile)
  (cadr mobile))

(defun branch-length (branch)
  (car branch))

(defun branch-structure (branch)
  (cadr branch))

(defun branch-weight (branch)
  (let ((b (branch-structure branch)))
    (if (consp b)
        (total-weight b)
        b)))

(defun total-weight (mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(defun mobile-balanced (mobile)
  (labels ((branch-torque (branch)
             (* (branch-length branch)
                (branch-weight branch)))
           (branch-balanced (branch)
             (let ((b (branch-structure branch)))
               (if (consp b)
                   (mobile-balanced b)
                   t))))
    (let ((l (left-branch mobile))
          (r (right-branch mobile)))
      (and (= (branch-torque l) (branch-torque r))
           (branch-balanced l)
           (branch-balanced r)))))

;;; Exercise 2.30

(defun square-tree (tree)
  (cond ((null tree)        nil)
        ((not (consp tree)) (square tree))
        (t                  (cons (square-tree (car tree))
                                  (square-tree (cdr tree))))))

(defun square-tree-map (tree)
  (mapcar #'(lambda (sub-tree)
              (if (consp sub-tree)
                  (square-tree-map sub-tree)
                  (square sub-tree))) tree))

;;; Exercise 2.31

(defun tree-map (f tree)
  (mapcar #'(lambda (sub-tree)
              (if (consp sub-tree)
                  (tree-map f sub-tree)
                  (funcall f sub-tree))) tree))

(defun square-tree-map2 (tree)
  (tree-map #'square tree))

;;; Exercise 2.32

(defun subsets (s)
  (if (null s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (mapcar #'(lambda (x)
                                 (cons (car s) x)) rest)))))

;;; Exercise 2.33

(defun my-map (p sequence)
  (accumulate #'(lambda (x y)
                  (cons (funcall p x) y)) nil sequence))

(defun my-append (seq1 seq2)
  (accumulate #'cons seq2 seq1))

(defun my-length (sequence)
  (accumulate #'(lambda (x y)
                  (declare (ignore x))
                  (+ 1 y)) 0 sequence))

;;; Exercise 2.34

(defun horner-eval (x coefficient-sequence)
  (accumulate #'(lambda (this-coeff higher-terms)
                  (+ (* x higher-terms) this-coeff)) 0 coefficient-sequence))

;;; Exercise 2.35

(defun count-leaves (tree)
  (accumulate #'+ 0 (mapcar #'(lambda (x)
                                (declare (ignore x))
                                1) (enumerate-tree tree))))

;;; Exercise 2.36

(defun accumulate-n (op init seqs)
  (if (null (car seqs))
      nil
      (cons (accumulate op init (mapcar #'car seqs))
            (accumulate-n op init (mapcar #'cdr seqs)))))

;;; Exercise 2.37

(defun dot-product (v w)
  (accumulate #'+ 0 (mapcar #'* v w)))

(defun matrix-*-vector (m v)
  (mapcar #'(lambda (row)
              (dot-product row v)) m))

(defun transpose (mat)
  (accumulate-n #'cons nil mat))

(defun matrix-*-matrix (m n)
  (let ((cols (transpose n)))
    (mapcar #'(lambda (row)
                (matrix-*-vector cols row)) m)))

;;; Exercise 2.38

(defun fold-right (op initial sequence)
  (accumulate op initial sequence))

(defun fold-left (op initial sequence)
  (labels ((iter (result rest)
             (if (null rest)
                 result
                 (iter (funcall op result (car rest))
                       (cdr rest)))))
    (iter initial sequence)))

;;; Exercise 2.39

(defun reverse-r (sequence)
  (fold-right #'(lambda (x y)
                  (append y (list x))) nil sequence))

(defun reverse-l (sequence)
  (fold-left #'(lambda (x y)
                 (cons y x)) nil sequence))

;;; Exercise 2.40

(defun unique-pairs (n)
  (flatmap #'(lambda (i)
               (mapcar #'(lambda (j)
                           (list i j)) (enumerate-interval 1 (1- i))))
           (enumerate-interval 1 n)))

(defun prime-sum-pairs (n)
  (mapcar #'make-pair-sum (remove-if-not #'prime-sum (unique-pairs n))))

;;; Exercise 2.41

(defun ordered-triples (n)
  (flatmap #'(lambda (i)
               (flatmap #'(lambda (j)
                            (mapcar #'(lambda (k)
                                        (list i j k))
                                    (enumerate-interval 1 (1- j))))
                        (enumerate-interval 1 (1- i))))
           (enumerate-interval 1 n)))

(defun make-triple-sum (triple)
  (append triple (list (reduce #'+ triple :initial-value 0))))

(defun ordered-triple-sum (n s)
  (labels ((triple-sum (triple)
             (= s (reduce #'+ triple :initial-value 0))))
    (mapcar #'make-triple-sum
            (remove-if-not #'triple-sum (ordered-triples n)))))

;;; Exercise 2.42

(defparameter *empty-board* nil)

(defun queens (board-size)
  (labels ((queens-cols (k)
             (if (= k 0)
                 (list *empty-board*)
                 (remove-if-not
                  #'(lambda (positions)
                      (safe k positions))
                  (flatmap #'(lambda (rest-of-queens)
                               (mapcar #'(lambda (new-row)
                                           (adjoin-position
                                            new-row k rest-of-queens))
                                       (enumerate-interval 1 board-size)))
                           (queens-cols (1- k)))))))
    (queens-cols board-size)))

(defun make-position (row col)
  (cons row col))

(defun position-row (position)
  (car position))

(defun position-col (position)
  (cdr position))

(defun adjoin-position (row col positions)
  (append positions (list (make-position row col))))

(defun safe (col positions)
  (let ((kth-queen (list-ref positions (- col 1)))
        (other-queens (remove-if-not #'(lambda (q)
                                         (not (= col (position-col q))))
                                     positions)))
    (labels ((attacks (q1 q2)
               (or (= (position-row q1) (position-row q2))
                   (= (abs (- (position-row q1) (position-row q2)))
                      (abs (- (position-col q1) (position-col q2))))))
             (iter (q board)
               (or (null board)
                   (and (not (attacks q (car board)))
                        (iter q (cdr board))))))
      (iter kth-queen other-queens))))
