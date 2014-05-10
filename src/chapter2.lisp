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
           :list-ref))
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
