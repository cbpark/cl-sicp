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
           :adjoin-set-code
           :get-proc
           :put-proc
           :type-tag
           :contents
           :attach-tag))
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
                   (t (error "Argument not 0 or 1: CONS ~a" m)))))
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
    ((numberp exp)   0)
    ((variablep exp) (if (same-variable exp var)
                         1
                         0))
    ((sump exp)      (make-sum (deriv (addend exp) var)
                               (deriv (augend exp) var)))
    ((productp exp)  (make-sum (make-product (multiplier exp)
                                             (deriv (multiplicand exp) var))
                               (make-product (deriv (multiplier exp) var)
                                             (multiplicand exp))))
    (t               (error "unknown expression type: DERIV ~a" exp))))

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

;;; 2.4.1 Representations for Complex Numbers

(defun add-complex (z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(defun sub-complex (z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(defun mul-complex (z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(defun div-complex (z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

;;; 2.4.2 Tagged Data

(defun attach-tag (type-tag contents)
  (cons type-tag contents))

(defun type-tag (datum)
  (if (consp datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG ~a" datum)))

(defun contents (datum)
  (if (consp datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS ~a" datum)))

(defun rectangularp (z)
  (eq (type-tag z) 'rectangular))

(defun polarp (z)
  (eq (type-tag z) 'polar))

(defun real-part-rectangular (z)
  (car z))

(defun imag-part-rectangular (z)
  (cdr z))

(defun magnitude-rectangular (z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(defun angle-rectangular (z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))

(defun make-from-real-imag-rectangular (x y)
  (attach-tag 'rectangular (cons x y)))

(defun make-from-mag-ang-rectangular (r a)
  (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a)))))

(defun real-part-polar (z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(defun imag-part-polar (z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(defun magnitude-polar (z)
  (car z))

(defun angle-polar (z)
  (cdr z))

(defun make-from-real-imag-polar (x y)
  (attach-tag 'polar (cons (sqrt (+ (square x) (square y)))
                           (atan y x))))

(defun make-from-mag-ang-polar (r a)
  (attach-tag 'polar (cons r a)))

;; (defun real-part (z)
;;   (cond ((rectangularp z) (real-part-rectangular (contents z)))
;;         ((polarp z)       (real-part-polar       (contents z)))
;;         (t                (error "Unknown type: REAL-PART"))))

;; (defun imag-part (z)
;;   (cond ((rectangularp z) (imag-part-rectangular (contents z)))
;;         ((polarp z)       (imag-part-polar       (contents z)))
;;         (t                (error "Unknown type: IMAG-PART"))))

;; (defun magnitude (z)
;;   (cond ((rectangularp z) (magnitude-rectangular (contents z)))
;;         ((polarp z)       (magnitude-polar       (contents z)))
;;         (t                (error "Unknown type: MAGNITUDE"))))

;; (defun angle (z)
;;   (cond ((rectangularp z) (angle-rectangular (contents z)))
;;         ((polarp z)       (angle-polar       (contents z)))
;;         (t                (error "Unknown type: ANGLE"))))

;; (defun make-from-real-imag (x y)
;;   (make-from-real-imag-rectangular x y))

;; (defun make-from-mag-ang (r a)
;;   (make-from-mag-ang-polar r a))

;;; 2.4.3 Data-Directed Programming and Additivity

(defun make-table (&key (key-cmp #'eq))
  (let ((local-table (list '*table*)))
    (labels ((find-key-value (key table)
               (assoc key table :test key-cmp))
             (lookup (key-1 key-2)
               (let ((subtable (find-key-value key-1 (cdr local-table))))
                 (if subtable
                     (let ((record (find-key-value key-2 (cdr subtable))))
                       (if record
                           (cdr record)
                           nil))
                     nil)))
             (insert (key-1 key-2 value)
               (let ((subtable (find-key-value key-1 (cdr local-table))))
                 (if subtable
                     (let ((record (find-key-value key-2 (cdr subtable))))
                       (if record
                           (setf (cdr record) value)
                           (setf (cdr subtable) (cons (cons key-2 value)
                                                      (cdr subtable)))))
                     (setf (cdr local-table) (cons (list key-1 (cons key-2 value))
                                                   (cdr local-table)))))
               'ok)
             (dispatch (m)
               (cond ((eq m 'lookup-proc) #'lookup)
                     ((eq m 'insert-proc) #'insert)
                     (t (error "Unknown operation: TABLE")))))
      #'dispatch)))

(defparameter *operation-table* (make-table :key-cmp #'equal))

(defun get-proc (op type)
  (funcall (funcall *operation-table* 'lookup-proc) op type))

(defun put-proc (op type item)
  (funcall (funcall *operation-table* 'insert-proc) op type item))

(defun install-rectangular-package ()
  ;; internal procedures
  (labels ((real-part (z)
             (car z))
           (imag-part (z)
             (cdr z))
           (make-from-real-imag (x y)
             (cons x y))
           (magnitude (z)
             (sqrt (+ (square (real-part z))
                      (square (imag-part z)))))
           (angle (z)
             (atan (imag-part z) (real-part z)))
           (make-from-mag-ang (r a)
             (cons (* r (cos a)) (* r (sin a))))

           ;; interface to the rest of the system
           (tag (x)
             (attach-tag 'rectangular x)))
    (put-proc 'real-part '(rectangular) #'real-part)
    (put-proc 'imag-part '(rectangular) #'imag-part)
    (put-proc 'magnitude '(rectangular) #'magnitude)
    (put-proc 'angle     '(rectangular) #'angle)
    (put-proc 'make-from-real-imag 'rectangular
              #'(lambda (x y)
                  (tag (make-from-real-imag x y))))
    (put-proc 'make-from-mag-ang   'rectangular
              #'(lambda (r a)
                  (tag (make-from-mag-ang r a))))
    'done))

(install-rectangular-package)

(defun install-polar-package ()
  ;; internal procedures
  (labels ((magnitude (z)
             (car z))
           (angle (z)
             (cdr z))
           (make-from-mag-ang (r a)
             (cons r a))
           (real-part (z)
             (* (magnitude z) (cos (angle z))))
           (imag-part (z)
             (* (magnitude z) (sin (angle z))))
           (make-from-real-imag (x y)
             (cons (sqrt (+ (square x) (square y)))
                   (atan y x)))

           ;; interface to the rest of the system
           (tag (x)
             (attach-tag 'polar x)))
    (put-proc 'real-part '(polar) #'real-part)
    (put-proc 'imag-part '(polar) #'imag-part)
    (put-proc 'magnitude '(polar) #'magnitude)
    (put-proc 'angle     '(polar) #'angle)
    (put-proc 'make-from-real-imag 'polar
              #'(lambda (x y)
                  (tag (make-from-real-imag x y))))
    (put-proc 'make-from-mag-ang   'polar
              #'(lambda (r a)
                  (tag (make-from-mag-ang r a))))
    'done))

(install-polar-package)

(defun apply-generic (op &rest args)
  (let* ((type-tags (mapcar #'type-tag args))
         (proc (get-proc op type-tags)))
    (if proc
        (apply proc (mapcar #'contents args))
        (error "No method for these types: APPLY-GENERIC ~a"
               (list op type-tags)))))

(defun real-part (z)
  (apply-generic 'real-part z))

(defun imag-part (z)
  (apply-generic 'imag-part z))

(defun magnitude (z)
  (apply-generic 'magnitude z))

(defun angle (z)
  (apply-generic 'angle z))

;; Ex: (real-part (make-from-real-imag 1 2))
(defun make-from-real-imag (x y)
  (funcall (get-proc 'make-from-real-imag 'rectangular) x y))

;; Ex: (magnitude (make-from-mag-ang 10 45))
(defun make-from-mag-ang (r a)
  (funcall (get-proc 'make-from-mag-ang 'polar) r a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.5 Systems with Generic Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 2.5.1 Generic Arithmetic Operations
