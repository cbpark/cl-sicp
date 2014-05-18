(in-package :cl-user)
(defpackage cl-sicp.chapter2
  (:use :cl)
  (:import-from :cl-sicp.chapter1
                :square
                :fib
                :primep)
  (:export :accumulate
           :enumerate-interval
           :enumerate-tree
           :flatmap
           :prime-sum
           :make-pair-sum
           :list-ref
           :variablep
           :same-variable
           :make-sum
           :make-product
           :sump
           :productp
           :=numberp
           :addend
           :multiplier
           :entry
           :make-tree
           :key
           :make-code-tree
           :make-leaf
           :make-leaf-set
           :decode
           :leafp
           :symbols
           :left-branch-code
           :right-branch-code
           :adjoin-set-code))
(in-package :cl-sicp.chapter2)

;;; Chapter 2. Building Abstractions with Data

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.1 Introduction to Data Abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 2.1.1 Example: Arithmetic Operations for Rational Numbers

(defun add-rat (x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defun sub-rat (x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defun mul-rat (x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defun div-rat (x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defun equal-rat (x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; Constructor
;; (defun make-rat (n d)
;;   (cons n d))
(defun make-rat (n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;; Selectors
(defun numer (x)
  (car x))

(defun denom (x)
  (cdr x))

(defun print-rat (x)
  (fresh-line)
  (format t "~a~a~a" (numer x) "/" (denom x)))

(defparameter *one-half* (make-rat 1 2))

(defparameter *one-third* (make-rat 1 3))

;;; 2.1.2 Abstraction Barriers

;;; 2.1.3 What Is Meant by Data?

(defun my-cons (x y)
  (labels ((dispatch (m)
             (cond ((= m 0) x)
                   ((= m 1) y)
                   (t (error "Argument not 0 or 1: CONS")))))
    #'dispatch))

(defun my-car (z)
  (funcall z 0))

(defun my-cdr (z)
  (funcall z 1))

;;; 2.1.4 Extended Exercise: Interval Arithmetic

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.2 Hierarchial Data and the Closure Property
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 2.2.1 Representing Sequences

(defparameter *one-through-four* (list 1 2 3 4))

(defun list-ref (items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (1- n))))

(defparameter *squares* (list 1 4 9 16 25))

(defun my-length (items)
  (if (null items)
      0
      (1+ (my-length (cdr items)))))

(defparameter *odds* (list 1 3 5 7))

(defun my-length2 (items)
  (labels ((length-iter (a count)
             (if (null a)
                 count
                 (length-iter (cdr a) (1+ count)))))
    (length-iter items 0)))

(defun my-append (list1 list2)
  (if (null list1)
      list2
      (cons (car list1) (my-append (cdr list1) list2))))

(defun scale-list (items factor)
  (if (null items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(defun my-map (proc items)
  (if (null items)
      nil
      (cons (funcall proc (car items))
            (my-map proc (cdr items)))))

(defun scale-list-map (items factor)
  (my-map #'(lambda (x)
              (* x factor)) items))

;;; 2.2.2 Hierarchial Structures

(defun count-leaves (x)
  (cond ((null x)        0)
        ((not (consp x)) 1)
        (t               (+ (count-leaves (car x))
                            (count-leaves (cdr x))))))

(defun scale-tree (tree factor)
  (cond ((null tree)        nil)
        ((not (consp tree)) (* tree factor))
        (t                  (cons (scale-tree (car tree) factor)
                                  (scale-tree (cdr tree) factor)))))

(defun scale-tree-map (tree factor)
  (mapcar #'(lambda (sub-tree)
              (if (consp sub-tree)
                  (scale-tree-map sub-tree factor)
                  (* sub-tree factor))) tree))

;;; 2.2.3 Sequence as Conventional Interfaces

(defun sum-odd-squares (tree)
  (cond ((null tree)        0)
        ((not (consp tree)) (if (oddp tree)
                                (square tree)
                                0))
        (t                  (+ (sum-odd-squares (car tree))
                               (sum-odd-squares (cdr tree))))))

(defun even-fibs (n)
  (labels ((next (k)
             (if (> k n)
                 nil
                 (let ((f (fib k)))
                   (if (evenp f)
                       (cons f (next (+ k 1)))
                       (next (1+ k)))))))
    (next 0)))

(defun filter (predicate sequence)
  (cond
    ((null sequence)                    nil)
    ((funcall predicate (car sequence)) (cons (car sequence)
                                              (filter predicate (cdr sequence))))
    (t                                  (filter predicate (cdr sequence)))))

(defun accumulate (op initial sequence)
  (if (null sequence)
      initial
      (funcall op (car sequence)
               (accumulate op initial (cdr sequence)))))

(defun enumerate-interval (low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (1+ low) high))))

(defun enumerate-tree (tree)
  (cond ((null tree)        nil)
        ((not (consp tree)) (list tree))
        (t                  (append (enumerate-tree (car tree))
                                    (enumerate-tree (cdr tree))))))

(defun sum-odd-squares2 (tree)
  (accumulate #'+ 0
              (mapcar #'square (filter #'oddp (enumerate-tree tree)))))

(defun even-fibs2 (n)
  (accumulate #'cons nil
              (filter #'evenp (mapcar #'fib (enumerate-interval 0 n)))))

(defun list-fib-squares (n)
  (accumulate #'cons nil
              (mapcar #'square (mapcar #'fib (enumerate-interval 0 n)))))

(defun product-of-squares-of-odd-elements (sequence)
  (accumulate #'* 1 (mapcar #'square (filter #'oddp sequence))))

(defun flatmap (proc seq)
  (accumulate #'append nil (mapcar proc seq)))

(defun prime-sum (pair)
  (primep (+ (car pair) (cadr pair))))

(defun make-pair-sum (pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(defun prime-sum-pairs (n)
  (mapcar #'make-pair-sum
          (filter #'prime-sum (flatmap
                               #'(lambda (i)
                                   (mapcar #'(lambda (j)
                                               (list i j))
                                           (enumerate-interval 1 (1- i))))
                               (enumerate-interval 1 n)))))

(defun permutations (s)
  (if (null s)
      (list nil)
      (flatmap #'(lambda (x)
                   (mapcar #'(lambda (p)
                               (cons x p)) (permutations (my-remove x s))))
               s)))

(defun my-remove (item sequence)
  (filter #'(lambda (x)
              (not (= x item))) sequence))

;;; 2.2.4 Example: A Picture Language

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.3 Symbolic Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 2.3.1 Quotation

(defun memq (item x)
  (cond ((null x) nil)
        ((eq item (car x)) x)
        (t        (memq item (cdr x)))))

;;; 2.3.2 Example: Symbolic Differentiation

(defun deriv (exp var)
  (cond
    ((numberp exp) 0)
    ((variablep exp) (if (same-variable exp var)
                         1
                         0))
    ((sump exp) (make-sum (deriv (addend exp) var)
                          (deriv (augend exp) var)))
    ((productp exp) (make-sum (make-product (multiplier exp)
                                            (deriv (multiplicand exp) var))
                              (make-product (deriv (multiplier exp) var)
                                            (multiplicand exp))))
    (t (error "unknown expression type: DERIV"))))

(defun variablep (x)
  (symbolp x))

(defun same-variable (v1 v2)
  (and (variablep v1) (variablep v2) (eq v1 v2)))

(defun make-sum (a1 a2)
  ;; (list '+ a1 a2)
  (cond ((=numberp a1 0) a2)
        ((=numberp a2 0) a1)
        ((and (numberp a1) (numberp a2)) (+ a1 a2))
        (t (list '+ a1 a2))))

(defun =numberp (exp num)
  (and (numberp exp) (= exp num)))

(defun make-product (m1 m2)
  ;; (list '* m1 m2)
  (cond ((or (=numberp m1 0) (=numberp m2 0)) 0)
        ((=numberp m1 1) m2)
        ((=numberp m2 1) m1)
        ((and (numberp m1) (numberp m2)) (* m1 m2))
        (t (list '* m1 m2))))

(defun sump (x)
  (and (consp x) (eq (car x) '+)))

(defun addend (s)
  (cadr s))

(defun augend (s)
  (caddr s))

(defun productp (x)
  (and (consp x) (eq (car x) '*)))

(defun multiplier (p)
  (cadr p))

(defun multiplicand (p)
  (caddr p))

;;; 2.3.3 Example: Representing Sets

;;; Sets as unordered lists

(defun element-of-set (x set)
  (cond ((null set) nil)
        ((equal x (car set)) t)
        (t (element-of-set x (cdr set)))))

(defun adjoin-set (x set)
  (if (element-of-set x set)
      set
      (cons x set)))

(defun intersection-set (set1 set2)
  (cond
    ((or (null set1) (null set2)) '())
    ((element-of-set (car set1) set2) (cons (car set1)
                                            (intersection-set (cdr set1) set2)))
    (t (intersection-set (cdr set1) set2))))

;;; Sets as ordered lists

(defun element-of-set2 (x set)
  (cond ((null set) nil)
        ((= x (car set)) t)
        ((< x (car set)) nil)
        (t (element-of-set2 x (cdr set)))))

(defun intersection-set2 (set1 set2)
  (if (or (null set1) (null set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set2 (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection-set2 (cdr set1) set2))
              ((< x2 x1)
               (intersection-set2 set1 (cdr set2)))))))

;;; Sets as binary trees

(defun entry (tree)
  (car tree))

(defun left-branch (tree)
  (cadr tree))

(defun right-branch (tree)
  (caddr tree))

(defun make-tree (entry left right)
  (list entry left right))

(defun element-of-set3 (x set)
  (cond ((null set) nil)
        ((= x (entry set)) t)
        ((< x (entry set)) (element-of-set3 x (left-branch set)))
        ((> x (entry set)) (element-of-set3 x (right-branch set)))))

(defun adjoin-set2 (x set)
  (cond ((null set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set2 x (left-branch set))
                                      (right-branch set)))
        ((> x (entry set)) (make-tree (entry set)
                                      (left-branch set)
                                      (adjoin-set2 x (right-branch set))))))

;;; Sets and information retrieval

(defun key (record)
  (car record))

(defun lookup (given-key set-of-records)
  (cond ((null set-of-records) nil)
        ((equal given-key (key (car set-of-records))) (car set-of-records))
        (t (lookup given-key (cdr set-of-records)))))

;;; 2.3.4 Example: Huffman Encoding Trees

(defun make-leaf (symbol weight)
  (list 'leaf symbol weight))

(defun leafp (object)
  (eq (car object) 'leaf))

(defun symbol-leaf (x)
  (cadr x))

(defun weight-leaf (x)
  (caddr x))

(defun make-code-tree (left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defun left-branch-code (tree)
  (car tree))

(defun right-branch-code (tree)
  (cadr tree))

(defun symbols (tree)
  (if (leafp tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(defun weight (tree)
  (if (leafp tree)
      (weight-leaf tree)
      (cadddr tree)))

(defun decode (bits tree)
  (labels ((decode-1 (bits current-branch)
             (if (null bits)
                 '()
                 (let ((next-branch (choose-branch (car bits) current-branch)))
                   (if (leafp next-branch)
                       (cons (symbol-leaf next-branch)
                             (decode-1 (cdr bits) tree))
                       (decode-1 (cdr bits) next-branch))))))
    (decode-1 bits tree)))

(defun choose-branch (bits branch)
  (cond ((= bits 0) (left-branch-code branch))
        ((= bits 1) (right-branch-code branch))
        (t          (error "bad bit: CHOOSE-BRANCH"))))

(defun adjoin-set-code (x set)
  (cond ((null set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (t (cons (car set) (adjoin-set-code x (cdr set))))))

(defun make-leaf-set (pairs)
  (if (null pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set-code (make-leaf (car pair)   ; symbol
                                    (cadr pair)) ; frequency
                         (make-leaf-set (cdr pairs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.4 Multiple Representation for Abstract Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
