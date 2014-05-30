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

;;; Exercise 3.21
;;; See chapter3.lisp

;;; Exercise 3.22

(defun make-queue ()
  (let ((front-ptr '())
        (rear-ptr '()))
    (labels ((empty-queue ()
               (null front-ptr))
             (set-front-ptr (item)
               (setf front-ptr item))
             (set-rear-ptr (item)
               (setf rear-ptr item))
             (front-queue ()
               (if (empty-queue)
                   (error "FRONT called with an empty queue")
                   (car front-ptr)))
             (insert-queue (item)
               (let ((new-pair (cons item '())))
                 (cond ((empty-queue) (set-front-ptr new-pair)
                        (set-rear-ptr new-pair))
                       (t             (setf (cdr rear-ptr) new-pair)
                                      (set-rear-ptr new-pair)))))
             (delete-queue ()
               (cond ((empty-queue) (error "DELETE called with an empty queue"))
                     (t             (set-front-ptr (cdr front-ptr)))))
             (print-queue ()
               front-ptr)
             (dispatch (m)
               (cond ((eq m 'empty-queue)  #'empty-queue)
                     ((eq m 'front-queue)  #'front-queue)
                     ((eq m 'insert-queue) #'insert-queue)
                     ((eq m 'delete-queue) #'delete-queue)
                     ((eq m 'print-queue)  #'print-queue)
                     (t (error "QUEUE: undefined operation ~a" m)))))
      #'dispatch)))

;;; Exercise 3.23

(defun make-deque ()
  (cons '() '()))

(defun front-ptr (deque)
  (car deque))

(defun rear-ptr (deque)
  (cdr deque))

(defun set-front-ptr (deque item)
  (setf (car deque) item))

(defun set-rear-ptr (deque item)
  (setf (cdr deque) item))

(defun empty-deque (deque)
  (null (front-ptr deque)))

(defun front-deque (deque)
  (if (empty-deque deque)
      (error "FRONT called on empty deque ~a" deque)
      (car (front-ptr deque))))

(defun rear-deque (deque)
  (if (empty-deque deque)
      (error "REAR called on empty deque ~a" deque)
      (car (rear-ptr deque))))

(defun front-insert-deque (deque item)
  (let ((new-item (list item (front-ptr deque) nil)))
    (cond ((empty-deque deque)
           (set-front-ptr deque new-item)
           (set-rear-ptr deque new-item)
           nil)
          (t
           (setf (caddr (front-ptr deque)) new-item)
           (set-front-ptr deque new-item)
           nil))))

(defun rear-insert-deque (deque item)
  (let ((new-item (list item nil (rear-ptr deque))))
    (cond ((empty-deque deque)
           (set-front-ptr deque new-item)
           (set-rear-ptr deque new-item)
           nil)
          (t
           (setf (cadr (rear-ptr deque)) new-item)
           (set-rear-ptr deque new-item)
           nil))))

(defun front-delete-deque (deque)
  (cond ((empty-deque deque)
         (error "DELETE called on empty deque ~a" deque))
        (t
         (let ((new-front (cadr (front-ptr deque))))
           (setf (cadr (front-ptr deque)) nil)
           (setf (caddr (front-ptr deque)) nil)
           (set-front-ptr deque new-front)
           (if (null new-front)
               (set-rear-ptr deque new-front)
               (setf (caddr new-front) nil))
           nil))))

(defun rear-delete-deque (deque)
  (cond ((empty-deque deque)
         (error "DELETE called on empty deque ~a" deque))
        (t
         (let ((new-rear (caddr (rear-ptr deque))))
           (setf (cadr (rear-ptr deque)) nil)
           (setf (caddr (rear-ptr deque)) nil)
           (set-rear-ptr deque new-rear)
           (if (null new-rear)
               (set-front-ptr deque new-rear)
               (setf (cadr new-rear) nil))
           nil))))

(defun print-deque (deque)
  (labels ((iter (item)
             (when (not (null item))
               (format t "~a " (car item))
               (iter (cadr item)))))
    (unless (empty-deque deque)
      (iter (front-ptr deque)))))

;;; Exercise 3.24
;;; See chapter3.lisp

;;; Exercise 3.25

(defun make-table (&key (same-key #'eq))
  (let ((local-table (list '*table*)))
    (labels ((lookup (keys)
               (labels ((iter (table keys)
                          (cond ((null table) nil)
                                ((null keys) nil)
                                (t (iter (assoc (car keys) (cdr table)
                                                :test same-key)
                                         (cdr keys))))))
                 (iter local-table keys)))
             (insert (keys value)
               (labels ((iter (keys table)
                          (if (null keys)
                              (setf (cdr table) value)
                              (let ((subtable (assoc (car keys) (cdr table)
                                                     :test same-key)))
                                (if subtable
                                    (iter (cdr keys) subtable)
                                    (append-item keys value table)))))
                        (append-item (keys value table)
                          (if (null keys)
                              (setf (cdr table) value)
                              (let ((new-record (list (car keys))))
                                (setf (cdr table) (cons new-record (cdr table)))
                                (append-item (cdr keys) value new-record)))))
                 (iter keys local-table))
               'ok)
             (dispatch (m)
               (cond ((eq m 'lookup) #'lookup)
                     ((eq m 'insert) #'insert)
                     (t (error "Unknown operation: TABLE ~a" m)))))
      #'dispatch)))

(defun lookup-table (table keys)
  (funcall (funcall table 'lookup) keys))

(defun insert-table (table keys value)
  (funcall (funcall table 'insert) keys value))

;;; Exercise 3.26

(defun make-tree (entry left right)
  (list entry left right))

(defun make-leaf (entry)
  (list entry nil nil))

(defun entry (tree)
  (car tree))

(defun left-branch (tree)
  (cadr tree))

(defun right-branch (tree)
  (caddr tree))

(defun set-entry (tree entry)
  (setf (car tree) entry))

(defun set-left-branch (tree left-branch)
  (setf (cadr tree) left-branch))

(defun set-right-branch (tree right-branch)
  (setf (caddr tree) right-branch))

(defun make-record (key data)
  (list key data))

(defun key (record)
  (car record))

(defun data (record)
  (cadr record))

(defun make-table2 (&key (compare #'<))
  (let ((local-table (cons '*table* nil)))
    (labels ((tree-root ()
               (cdr local-table))
             (set-tree-root (node)
               (setf (cdr local-table) node))
             (node-lookup (key node)
               (if (null node)
                   nil
                   (let* ((cur-entry (entry node))
                          (cur-key (key cur-entry)))
                     (cond ((funcall compare key cur-key)
                            (node-lookup key (left-branch node)))
                           ((funcall compare cur-key key)
                            (node-lookup key (right-branch node)))
                           (t
                            cur-entry)))))
             (lookup (key)
               (node-lookup key (cdr local-table)))
             (node-insert (key data node)
               (let* ((cur-entry (entry node))
                      (cur-key (key cur-entry)))
                 (cond ((funcall compare key cur-key)
                        (if (null (left-branch node))
                            (set-left-branch node (make-leaf
                                                   (make-record key data)))
                            (node-insert key data (left-branch node))))
                       ((funcall compare cur-key key)
                        (if (null (right-branch node))
                            (set-right-branch node (make-leaf
                                                    (make-record key data)))
                            (node-insert key data (right-branch node))))
                       (t
                        (set-entry node (make-record key data))))))
             (insert (key data)
               (if (null (tree-root))
                   (set-tree-root (make-leaf (make-record key data)))
                   (node-insert key data (tree-root))))
             (dispatch (m)
               (cond ((eq m 'lookup) #'lookup)
                     ((eq m 'insert) #'insert)
                     (t (error "Unknown operation: TABLE ~a" m)))))
      #'dispatch)))

;;; Exercise 3.28
;;; See chapter3.lisp

;;; Exercise 3.29

(defun or-gate (a1 a2 output)
  (let ((x (make-wire))
        (y (make-wire))
        (z (make-wire)))
    (inverter a1 x)
    (inverter a2 y)
    (and-gate x y z)
    (inverter z output)))

;;; Exercise 3.30

(defun ripple-carry-adder (a b s c)
  (let ((c-in (make-wire)))
    (if (null (cdr a))
        (set-signal c-in 0)
        (ripple-carry-adder (cdr a) (cdr b) (cdr s) c-in))
    (full-adder (car a) (car b) c-in (car s) c)))

;;; Exercise 3.33

(defun averager (a b average)
  (let ((sum (make-connector))
        (two (make-connector)))
    (adder a b sum)
    (constant 2 two)
    (multiplier two average sum)))

;;; Exercise 3.35

(defun squarer (a b)
  (labels ((process-new-value ()
             (if (has-value b)
                 (if (< (get-value b) 0)
                     (error "square less than 0: SQUARER ~a" (get-value b))
                     (set-value a (sqrt (get-value b)) #'me))
                 (when (has-value a)
                   (set-value b (square (get-value a)) #'me))))
           (process-forget-value ()
             (forget-value a #'me)
             (forget-value b #'me)
             (process-new-value))
           (me (request)
             (cond ((eq request 'I-have-a-value) (process-new-value))
                   ((eq request 'I-lost-my-value) (process-forget-value))
                   (t (error "Unknown request: SQUARER ~a" request)))))
    (connect a #'me)
    (connect b #'me)
    #'me))

;;; Exercise 3.37

(defun c+ (x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(defun c* (x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(defun c/ (x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(defun cv (n)
  (let ((c (make-connector)))
    (constant n c)
    c))

(defun celsius-farenheit-converter (x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
