(in-package :cl-user)
(defpackage cl-sicp.chapter2-exercise
  (:use :cl
        :cl-sicp.chapter2)
  (:import-from :cl-sicp.chapter1
                :square)
  (:import-from :cl-sicp.chapter3
                :get-proc
                :put-proc))
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

;;; Exercise 2.54

(defun my-equal (items1 items2)
  (cond ((and (null items1) (null items2)) t)
        ((or (null items1) (null items2)) nil)
        ((and (consp items1) (consp items2))
         (and (my-equal (car items1) (car items2))
              (my-equal (cdr items1) (cdr items2))))
        ((or (consp items1) (consp items2)) nil)
        (t   (eq items1 items2))))

;;; Exercise 2.56

(defun deriv (exp var)
  (cond
    ((numberp exp) 0)
    ((variablep exp) (if (same-variable exp var)
                         1
                         0))
    ((sump exp) (make-sum (deriv (addend exp) var)
                          (deriv (augend exp) var)))
    ((productp exp)
     (make-sum (make-product (multiplier exp)
                             (deriv (multiplicand exp) var))
               (make-product (deriv (multiplier exp) var)
                             (multiplicand exp))))
    ((exponentiationp exp)
     (make-product
      (make-product (exponent exp)
                    (make-exponentiation (base exp)
                                         (make-sum (exponent exp) -1)))
      (deriv (base exp) var)))
    (t (error "unknown expression type: DERIV ~a" exp))))

(defun make-exponentiation (e1 e2)
  (cond ((=numberp e2 0) 1)
        ((=numberp e2 1) e1)
        ((and (numberp e1) (numberp e2)) (expt e1 e2))
        (t (list '** e1 e2))))

(defun exponentiationp (x)
  (and (consp x) (eq (car x) '**)))

(defun base (x)
  (cadr x))

(defun exponent (x)
  (caddr x))

;;; Exercise 2.57

(defun augend (s)
  (if (null (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(defun multiplicand (p)
  (if (null (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

;;; Exercise 2.58

(defun sump-infix (x)
  (and (consp x) (eq (cadr x) '+)))

(defun productp-infix (x)
  (and (consp x) (eq (cadr x) '*)))

(defun make-sum-infix (a1 a2)
  (cond ((=numberp a1 0) a2)
        ((=numberp a2 0) a1)
        ((and (numberp a1) (numberp a2)) (+ a1 a2))
        (t (list a1 '+ a2))))

(defun addend-infix (s)
  (car s))

(defun augend-infix (s)
  ;; (caddr s)
  (simplify (cddr s)))

(defun make-product-infix (m1 m2)
  (cond ((or (=numberp m1 0) (=numberp m2 0)) 0)
        ((=numberp m1 1) m2)
        ((=numberp m2 1) m1)
        ((and (numberp m1) (numberp m2)) (* m1 m2))
        (t (list m1 '* m2))))

(defun multiplier-infix (p)
  (car p))

(defun multiplicand-infix (p)
  ;; (caddr p)
  (simplify (cddr p)))

(defun simplify (exp)
  (if (null (cdr exp))
      (car exp)
      exp))

(defun deriv-infix (exp var)
  (cond
    ((numberp exp) 0)
    ((variablep exp) (if (same-variable exp var)
                         1
                         0))
    ((sump-infix exp) (make-sum-infix (deriv-infix (addend-infix exp) var)
                                      (deriv-infix (augend-infix exp) var)))
    ((productp-infix exp) (make-sum-infix
                           (make-product-infix
                            (multiplier-infix exp)
                            (deriv-infix (multiplicand-infix exp) var))
                           (make-product-infix
                            (deriv-infix (multiplier-infix exp) var)
                            (multiplicand-infix exp))))
    (t (error "unknown expression type: DERIV-INFIX"))))

;;; Exercise 2.59

(defun union-of-set (set1 set2)
  (cond
    ((null set1) set2)
    ((null set2) set1)
    ((member (car set1) set2 :test #'equal) (union-of-set (cdr set1) set2))
    (t (cons (car set1) (union-of-set (cdr set1) set2)))))

;;; Exercise 2.60

(defun adjoin-set-dup (x set)
  (cons x set))

(defun union-of-set-dup (set1 set2)
  (append set1 set2))

;;; Exercise 2.61

(defun adjoin-set2 (x set)
  (cond ((null set) (cons x '()))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        ((> x (car set)) (cons (car set)
                               (adjoin-set2 x (cdr set))))))

;;; Exercise 2.62

(defun union-set2 (set1 set2)
  (cond ((null set1) set2)
        ((null set2) set1)
        ((= (car set1) (car set2)) (cons (car set1)
                                         (union-set2 (cdr set1) (cdr set2))))
        ((< (car set1) (car set2)) (cons (car set1)
                                         (union-set2 (cdr set1) set2)))
        (t (cons (car set2)
                 (union-set2 set1 (cdr set2))))))

;;; Exercise 2.63

(defun left-branch-tree (tree)
  (cadr tree))

(defun right-branch-tree (tree)
  (caddr tree))

(defun tree->list-1 (tree)
  (if (null tree)
      '()
      (append (tree->list-1 (left-branch-tree tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch-tree tree))))))

(defun tree->list-2 (tree)
  (labels ((copy-to-list (tree result-list)
             (if (null tree)
                 result-list
                 (copy-to-list (left-branch-tree tree)
                               (cons (entry tree)
                                     (copy-to-list (right-branch-tree tree)
                                                   result-list))))))
    (copy-to-list tree '())))

;;; Exercise 2.64

(defun list->tree (elements)
  (car (partial-tree elements (length elements))))

(defun partial-tree (elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (floor (1- n) 2))
             (left-result (partial-tree elts left-size))
             (left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (right-size (- n (1+ left-size)))
             (this-entry (car non-left-elts))
             (right-result (partial-tree (cdr non-left-elts)
                                         right-size))
             (right-tree (car right-result))
             (remaining-elts (cdr right-result)))
        (cons (make-tree this-entry left-tree right-tree)
              remaining-elts))))

;;; Exercise 2.65

(defun union-set-tree (tree1 tree2)
  (labels ((union-list (set1 set2)
             (cond
               ((null set1) set2)
               ((null set2) set1)
               ((= (car set1) (car set2)) (cons (car set1)
                                                (union-list (cdr set1) (cdr set2))))
               ((< (car set1) (car set2)) (cons (car set1)
                                                (union-list (cdr set1) set2)))
               (t (cons (car set2)
                        (union-list set1 (cdr set2)))))))
    (list->tree (union-list (tree->list-2 tree1)
                            (tree->list-1 tree2)))))

(defun intersection-set-tree (tree1 tree2)
  (labels ((intersection-list (set1 set2)
             (if (or (null set1) (null set2))
                 '()
                 (let ((x1 (car set1))
                       (x2 (car set2)))
                   (cond
                     ((= x1 x2) (cons x1
                                      (intersection-list (cdr set1) (cdr set2))))
                     ((< x1 x2) (intersection-list (cdr set1) set2))
                     ((> x1 x2) (intersection-list set1 (cdr set2))))))))
    (list->tree (intersection-list (tree->list-2 tree1)
                                   (tree->list-2 tree2)))))

;;; Exercise 2.66

(defun lookup (given-key set-of-records)
  (cond
    ((null set-of-records) nil)
    ((= given-key (key (car set-of-records))) (car set-of-records))
    ((< given-key (key (car set-of-records)))
     (lookup given-key (left-branch-tree set-of-records)))
    ((> given-key (key (car set-of-records)))
     (lookup given-key (right-branch-tree set-of-records)))))

;;; Exercise 2.67

(defparameter *sample-tree* (make-code-tree
                             (make-leaf 'A 4)
                             (make-code-tree (make-leaf 'B 2)
                                             (make-code-tree (make-leaf 'D 1)
                                                             (make-leaf 'C 1)))))

(defparameter *sample-message* '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;;; (decode *sample-message* *sample-tree*)

;;; Exercise 2.68

(defun encode (message tree)
  (if (null message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(defun encode-symbol (symbol tree)
  (cond
    ((leafp tree) '())
    ((member symbol (symbols (left-branch-code tree)))
     (cons 0 (encode-symbol symbol (left-branch-code tree))))
    ((member symbol (symbols (right-branch-code tree)))
     (cons 1 (encode-symbol symbol (right-branch tree))))
    (t (error "bad symbol: ENCODE-SYMBOL"))))

;;; Exercise 2.69

(defun generate-huffman-tree (pairs)
  (successive-merge (make-leaf-set pairs)))

(defun successive-merge (set)
  (cond ((null set) '())
        ((null (cdr set)) (car set))
        (t (successive-merge
            (adjoin-set-code (make-code-tree (car set) (cadr set))
                             (cddr set))))))

;;; Exercise 2.70

(defparameter *song-tree* (generate-huffman-tree
                           '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3)
                             (YIP 9) (WAH 1))))

(defparameter *encoded-song* (encode
                              '(Get a job
                                Sha na na na na na na na na
                                Get a job
                                Sha na na na na na na na na
                                Wah yip yip yip yip yip yip yip yip yip
                                Sha boom)
                              *song-tree*))

;;; Exercise 2.73

(defun deriv-op (exp var)
  (cond ((numberp exp) 0)
        ((variablep exp) (if (same-variable exp var)
                             1
                             0))
        (t (funcall (get-proc 'deriv-op (operator exp)) (operands exp) var))))

(defun operator (exp)
  (car exp))

(defun operands (exp)
  (cdr exp))

(defun install-deriv-sum ()
  (labels ((deriv-sum (exp var)
             (make-sum (deriv-op (car exp) var)
                       (deriv-op (cadr exp) var))))
    (put-proc 'deriv-op '+ #'deriv-sum)))

(install-deriv-sum)

(defun install-deriv-product ()
  (labels ((deriv-product (exp var)
             (make-sum (make-product (car exp)
                                     (deriv-op (cadr exp) var))
                       (make-product (deriv-op (car exp) var)
                                     (cadr exp)))))
    (put-proc 'deriv-op '* #'deriv-product)))

(install-deriv-product)

(defun install-deriv-exponent ()
  (labels ((deriv-exponent (exp var)
             (make-product
              (make-product (cadr exp)
                            (make-exponentiation (car exp)
                                                 (make-sum (cadr exp) -1)))
              (deriv-op (car exp) var))))
    (put-proc 'deriv-op '** #'deriv-exponent)))

(install-deriv-exponent)

;;; Exercise 2.74

(defun get-record (employ-name personnel-file)
  (let ((record (funcall (get-proc 'record (type-tag personnel-file))
                         employ-name (contents personnel-file))))
    (if record
        (attach-tag (type-tag personnel-file) record)
        nil)))

(defun get-salary (record)
  (funcall (get-proc 'salary (type-tag record)) (contents record)))

(defun get-address (record)
  (funcall (get-proc 'address (type-tag record)) (contents record)))

(defun find-employ-record (employ-name personnel-file)
  (if (null personnel-file)
      nil
      (or (get-record employ-name (car personnel-file))
          (find-employ-record employ-name (cdr personnel-file)))))

;;; Exercise 2.75

;; message passing style
(defun make-from-mag-ang (r a)
  (labels ((dispatch (op)
             (cond
               ((eq op 'magnitude) r)
               ((eq op 'angle)     a)
               ((eq op 'real-part) (* r (cos a)))
               ((eq op 'imag-part) (* r (sin a)))
               (t                  (error "Unknown op: MAKE-FROM-MAG-ANG ~a" op)))))
    #'dispatch))

;; Ex: (apply-generic 'magnitude (make-from-mag-ang 10 45))
(defun apply-generic (op arg)
  (funcall arg op))

;;; Exercise 2.77 - 2.80
;;; See chapter2.lisp

;;; Exercise 2.81

(defun apply-generic-coercion (op &rest args)
  (let* ((type-tags (mapcar #'type-tag args))
         (proc (get-proc op type-tags)))
    (if proc
        (apply proc (mapcar #'contents args))
        (if (= (length args) 2)
            (let* ((type1 (car type-tags))
                   (type2 (cadr type-tags))
                   (a1 (car args))
                   (a2 (cadr args)))
              (if (equal type1 type2)
                  (error "No method for these types ~a" (list op type-tags))
                  (let ((t1->t2 (get-coercion type1 type2))
                        (t2->t1 (get-coercion type2 type1)))
                    (cond (t1->t2
                           (apply-generic-coercion op (funcall t1->t2 a1) a2))
                          (t2->t1
                           (apply-generic-coercion op a1 (funcall t2->t1 a2)))
                          (t (error "No method for these types ~a"
                                    (list op type-tags)))))))
            (error "No method for these types ~a" (list op type-tags))))))

;;; Exercise 2.82

(defun apply-generic-coercion-general (op &rest args)
  (labels ((can-coerced-into (types target-type)
             (and (mapcar #'(lambda (type)
                              (or (equal type target-type)
                                  (get-coercion type target-type)))
                          types)))
           (find-coerced-type (types)
             (or (mapcar #'(lambda (target-type)
                             (if (can-coerced-into types target-type)
                                 target-type
                                 nil))
                         types)))
           (coerce-to (target-type)
             (mapcar #'(lambda (arg)
                         (let ((arg-type (type-tag arg)))
                           (if (equal arg-type target-type)
                               arg
                               (funcall (get-coercion arg-type target-type) arg))))
                     args)))
    (let* ((type-tags (mapcar #'type-tag args))
           (proc (get-proc op type-tags)))
      (if proc
          (apply proc (mapcar #'contents args))
          (let ((target-type (find-coerced-type type-tags)))
            (if target-type
                (apply-generic-coercion-general op (coerce-to target-type))
                (error "No method for these types: APPLY-GENERIC-COERCION-GENERAL ~a"
                       (list op type-tags))))))))

;;; Exercise 2.83
;;; See chapter2.lisp

;;; Exercise 2.84

(defun apply-generic-raise (op &rest args)
  (labels ((raise-to (source target)
             (let* ((source-type (type-tag source))
                    (target-type (type-tag target))
                    (raise-proc (get-proc 'raise (list source-type))))
               (cond
                 ((equal source-type target-type) source)
                 (raise-proc (raise-to (funcall raise-proc (contents source))
                                       target))
                 (t nil)))))
    (let* ((type-tags (mapcar #'type-tag args))
           (proc (get-proc op type-tags)))
      (if proc
          (apply proc (mapcar #'contents args))
          (if (= (length args) 2)
              (let* ((a1 (car args))
                     (a2 (cadr args))
                     (raise1 (raise-to a1 a2))
                     (raise2 (raise-to a2 a1)))
                (cond (raise1 (apply-generic-raise op raise1 a2))
                      (raise2 (apply-generic-raise op a1 raise2))
                      (t (error "No method for these types: APPLY-GENERIC-RAISE ~a"
                                (list op type-tags)))))
              (error "No method for these types: APPLY-GENERIC-RAISE ~a"
                     (list op type-tags)))))))

;;; Exercise 2.85
;;; See also chapter2.lisp

(defun apply-generic-drop (op &rest args)
  (labels ((raise-to (source target)
             (let* ((source-type (type-tag source))
                    (target-type (type-tag target))
                    (raise-proc (get-proc 'raise (list source-type))))
               (cond
                 ((equal source-type target-type) source)
                 (raise-proc (raise-to (funcall raise-proc (contents source))
                                       target))
                 (t nil)))))
    (let* ((type-tags (mapcar #'type-tag args))
           (proc (get-proc op type-tags)))
      (if proc
          (drop (apply proc (mapcar #'contents args)))
          (if (= (length args) 2)
              (let* ((a1 (car args))
                     (a2 (cadr args))
                     (raise1 (raise-to a1 a2))
                     (raise2 (raise-to a2 a1)))
                (cond (raise1 (apply-generic-raise op raise1 a2))
                      (raise2 (apply-generic-raise op a1 raise2))
                      (t (error "No method for these types: APPLY-GENERIC-RAISE ~a"
                                (list op type-tags)))))
              (error "No method for these types: APPLY-GENERIC-RAISE ~a"
                     (list op type-tags)))))))

;;; Exercise 2.87 - 2.89, 2.91, 2.93
;;; See chapter2.lisp
