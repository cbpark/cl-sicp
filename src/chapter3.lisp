(in-package :cl-user)
(defpackage cl-sicp.chapter3
  (:use :cl)
  (:export :monte-carlo))
(in-package :cl-sicp.chapter3)

;;; Chapter 3. Modularity, Objects, and State

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3.1 Assignment and Local State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 3.1.1 Local State Variables

(defparameter *balance* 100)

(defun withdraw (amount)
  (if (>= *balance* amount)
      (progn (setf *balance* (- *balance* amount))
             *balance*)
      "Insufficient funds"))

(defparameter *new-withdraw*
  (let ((balance 100))
    #'(lambda (amount)
        (if (>= balance amount)
            (progn (setf balance (- balance amount))
                   balance)
            "Insufficient funds"))))

(defun make-withdraw (balance)
  #'(lambda (amount)
      (if (>= balance amount)
          (progn (setf balance (- balance amount))
                 balance)
          "Insufficient funds")))

(defparameter *W1* (make-withdraw 100))
(defparameter *W2* (make-withdraw 100))
;; (funcall *W1* 50)
;; (funcall *W2* 70)
;; (funcall *W2* 40)
;; (funcall *W1* 40)

(defun make-account (balance)
  (labels ((withdraw (amount)
             (if (>= balance amount)
                 (progn (setf balance (- balance amount))
                        balance)
                 "Insufficient funds"))
           (deposit (amount)
             (setf balance (+ balance amount))
             balance)
           (dispatch (m)
             (cond ((eq m 'withdraw) #'withdraw)
                   ((eq m 'deposit) #'deposit)
                   (t (error "Unknown request: MAKE-ACCOUNT ~a" m)))))
    #'dispatch))

(defparameter *acc* (make-account 100))
;; (funcall (funcall *acc* 'withdraw) 50)
;; (funcall (funcall *acc* 'withdraw) 60)
;; (funcall (funcall *acc* 'deposit) 40)
;; (funcall (funcall *acc* 'withdraw) 60)

;;; 3.1.2 The Benefits of Introducing Assignment

(defun estimate-pi (trials)
  (sqrt (/ 6 (monte-carlo trials #'cesaro-test))))

(defun cesaro-test ()
  (= (gcd (random 10000) (random 10000)) 1))

(defun monte-carlo (trials experiment)
  (labels ((iter (trials-remaining trials-passed)
             (cond ((= trials-remaining 0) (/ trials-passed trials))
                   ((funcall experiment) (iter (1- trials-remaining)
                                               (1+ trials-passed)))
                   (t (iter (1- trials-remaining) trials-passed)))))
    (iter trials 0)))

;;; 3.1.3 The Costs of Introducing Assignment

(defun make-simplified-withdraw (balance)
  #'(lambda (amount)
      (setf balance (- balance amount))
      balance))

(defparameter *W* (make-simplified-withdraw 25))

(defun make-decrementer (balance)
  #'(lambda (amount)
      (- balance amount)))

(defparameter *D* (make-decrementer 25))

(defun factorial-functional (n)
  (labels ((iter (product counter)
             (if (> counter n)
                 product
                 (iter (* counter product) (1+ counter)))))
    (iter 1 1)))

(defun factorial-imperative (n)
  (let ((product 1)
        (counter 1))
    (labels ((iter ()
               (if (> counter n)
                   product
                   (progn
                     (setf product (* counter product))
                     (setf counter (1+ counter))
                     (iter)))))
      (iter))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3.2 The Environment Model of Evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
