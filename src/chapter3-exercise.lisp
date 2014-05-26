(in-package :cl-user)
(defpackage cl-sicp.chapter3-exercise
  (:use :cl
        :cl-sicp.chapter3))
(in-package :cl-sicp.chapter3-exercise)

;;; Exercise 3.1

(defun make-accmulator (init-value)
  (let ((result init-value))
    #'(lambda (x)
        (setf result (+ result x))
        result)))

(defparameter *A* (make-accmulator 5))

;;; Exercise 3.2

(defun make-monitored (proc)
  (let ((count 0))
    (labels ((mf (arg)
               (cond ((eq arg 'how-many-calls?) count)
                     ((eq arg 'reset-count) (setf count 0))
                     (t (progn
                          (setf count (1+ count))
                          (funcall proc arg))))))
      #'mf)))

(defparameter *s* (make-monitored #'sqrt))

;;; Exercise 3.3, 3.4

(defun make-account (balance password)
  (let ((count 1))
    (labels ((withdraw (amount)
               (if (>= balance amount)
                   (progn (setf balance (- balance amount))
                          balance)
                   "Insufficient funds"))
             (deposit (amount)
               (setf balance (+ balance amount))
               balance)
             (call-the-cops (x)
               (declare (ignore x))
               "Call the cops!")
             (dispatch (p m)
               (cond ((not (eq p password)) (if (> count 7)
                                                #'call-the-cops
                                                #'(lambda (x)
                                                    (declare (ignore x))
                                                    (setf count (1+ count))
                                                    "Incorrect password")))
                     ((eq m 'withdraw) #'withdraw)
                     ((eq m 'deposit) #'deposit)
                     (t (error "Unknown request: MAKE-ACCOUNT ~a" m)))))
      #'dispatch)))

(defparameter *acc* (make-account 100 'secret-password))

;;; Exercise 3.5

(defun random-in-range (low high)
  (let ((range (- high low)))
    (+ low (random range))))

(defun circ-pred (cx cy r)
  #'(lambda (x y)
      (<= (+ (square (- x cx)) (square (- y cy))) (square r))))

(defun estimate-integral (P x1 x2 y1 y2 trials)
  (labels ((rect-area (x1 x2 y1 y2)
             (let ((width (- x2 x1))
                   (height (- y2 y1)))
               (* width height)))
           (experiment ()
             (funcall P (random-in-range x1 x2) (random-in-range y1 y2))))
    (* (monte-carlo trials #'experiment) (rect-area x1 x2 y1 y2))))

(defparameter *pi-approx* (/ (estimate-integral (circ-pred 5 7 3)
                                                2.0 8.0 4.0 10.0 10000)
                             (square 3)))

;;; Exercise 3.6

(defparameter *random-init* 0)

(defun rand-update (x)
  (+ x 1))

(defparameter *rand* (let ((x *random-init*))
                       (labels ((dispatch (m)
                                  (cond ((eq m 'generate)
                                         (progn (setf x (rand-update x))
                                                x))
                                        ((eq m 'reset)
                                         #'(lambda (new)
                                             (setf x new))))))
                         #'dispatch)))

;;; Exercise 3.7

(defun make-account2 (balance password)
  (let ((password-list (list password)))
    (labels ((withdraw (amount)
               (if (>= balance amount)
                   (progn (setf balance (- balance amount))
                          balance)
                   "Insufficient funds"))
             (deposit (amount)
               (setf balance (+ balance amount))
               balance)
             (make-joint (new-password)
               (setf password-list (cons new-password
                                         password-list))
               #'dispatch)
             (dispatch (p m)
               (if (member p password-list :test #'equal)
                   (cond ((eq m 'withdraw) #'withdraw)
                         ((eq m 'deposit) #'deposit)
                         ((eq m 'make-joint) #'make-joint)
                         (t (error "Unknown request: MAKE-ACCOUNT2 ~a" m)))
                   #'(lambda (x)
                       (declare (ignore x))
                       "Incorrect password"))))
      #'dispatch)))

(defun make-joint (account old-password new-password)
  (funcall (funcall account old-password 'make-joint) new-password))

(defparameter *peter-acc* (make-account2 100 'open-sesame))
(defparameter *paul-acc* (make-joint *peter-acc* 'open-sesame 'rosebud))

(defparameter *f* (let ((state 0))
                    (labels ((switch-state (x)
                               (let ((old-state state))
                                 (setf state (+ x state))
                                 old-state)))
                      #'switch-state)))

;;; Exercise 3.10

(defun make-withdraw (initial-amount)
  (let ((balance initial-amount))
    #'(lambda (amount)
        (if (>= balance amount)
            (progn (setf balance (- balance amount))
                   balance)
            "Insufficient funds"))))

(defparameter *W1* (make-withdraw 100))
(defparameter *W2* (make-withdraw 100))
