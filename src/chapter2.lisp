(in-package :cl-user)
(defpackage cl-sicp.chapter2
  (:use :cl))
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
