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

;;; Exercise 3.12

(defun nappend (x y)
  (setf (cdr (last-pair x)) y)
  x)

(defun last-pair (x)
  (if (null (cdr x))
      x
      (last-pair (cdr x))))

;;; Exercise 3.13

(defun make-cycle (x)
  (setf (cdr (last-pair x)) x)
  x)

;;; Exercise 3.14

(defun mystery (x)
  (labels ((my-loop (x y)
              (if (null x)
                  y
                  (let ((temp (cdr x)))
                    (setf (cdr x) y)
                    (my-loop temp x)))))
    (my-loop x '())))

;;; Exercise 3.16

(defun count-pairs (x)
  (if (not (consp x))
      0
      (+ (count-pairs (car x)) (count-pairs (cdr x)) 1)))

;;; Exercise 3.17

(defun count-pairs2 (x)
  (let ((counted '()))
    (labels ((counter (x)
               (if (or (not (consp x)) (member x counted :test #'eq))
                   0
                   (progn
                     (setf counted (cons x counted))
                     (+ (counter (car x)) (counter (cdr x)) 1)))))
      (counter x))))

;;; Exercise 3.18

(defun cycle-detection (x)
  (let ((visited '()))
    (labels ((iter (x)
               (setf visited (cons x visited))
               (cond ((null (cdr x)) nil)
                     ((member (cdr x) visited :test #'eq) t)
                     (t (iter (cdr x))))))
      (iter x))))

;;; Exercise 3.19
;;; See Floyd's algorithm for cycle detection, http://en.wikipedia.org/wiki/Cycle_detection

(defun cycle-detection2 (x)
  (labels ((iter (a b)
             (cond ((not (consp a)) nil)
                   ((not (consp b)) nil)
                   ((eq a b)        t)
                   ((eq a (cdr b))  t)
                   (t               (iter (cdr a) (cddr b))))))
    (iter (cdr x) (cddr x))))
