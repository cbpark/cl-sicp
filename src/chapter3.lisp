(in-package :cl-user)
(defpackage cl-sicp.chapter3
  (:use :cl)
  (:export :adder
           :and-gate
           :connect
           :constant
           :forget-value
           :full-adder
           :get-proc
           :get-value
           :has-value
           :inverter
           :make-connector
           :make-wire
           :monte-carlo
           :multiplier
           :put-proc
           :set-signal
           :set-value
           :square))
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

;;; 3.2.1 The Rules for Evaluation

;;; 3.2.2 Applying Simple Procedures

(defun square (x)
  (* x x))

(defun sum-of-squares (x y)
  (+ (square x) (square y)))

(defun f (a)
  (sum-of-squares (+ a 1) (* a 2)))

;;; 3.2.3 Frames as the Repository of Local State

;;; 3.2.4 Internal Definitions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3.3 Modeling with Mutable Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 3.3.1 Mutable List Structure

;;; 3.3.2 Representing Queues

(defun front-ptr (queue)
  (car queue))

(defun rear-ptr (queue)
  (cdr queue))

(defun set-front-ptr (queue item)
  (setf (car queue) item))

(defun set-rear-ptr (queue item)
  (setf (cdr queue) item))

(defun empty-queue (queue)
  (null (front-ptr queue)))

(defun make-queue ()
  (cons '() '()))

(defun front-queue (queue)
  (if (empty-queue queue)
      (error "FRONT called with an empty queue ~a" queue)
      (car (front-ptr queue))))

(defun insert-queue (queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue queue)
           (set-front-ptr queue new-pair)
           (set-rear-ptr queue new-pair)
           queue)
          (t
           (setf (cdr (rear-ptr queue)) new-pair)
           (set-rear-ptr queue new-pair)
           queue))))

(defun delete-queue (queue)
  (cond ((empty-queue queue)
         (error "DELETE called with an empty queue ~a" queue))
        (t
         (set-front-ptr queue (cdr (front-ptr queue)))
         queue)))

;; Exercise 3.21
(defun print-queue (queue)
  (format t "~a" (car queue)))

;;; 3.3.3 Representing Tables

(defun lookup (key table)
  (let ((record (my-assoc key (cdr table))))
    (if record
        (cdr record)
        nil)))

(defun my-assoc (key records)
  (cond ((null records) nil)
        ((equal key (caar records)) (car records))
        (t (my-assoc key (cdr records)))))

(defun insert (key value table)
  (let ((record (my-assoc key (cdr table))))
    (if record
        (setf (cdr record) value)
        (setf (cdr table) (cons (cons key value)
                                (cdr table)))))
  'ok)

;; (defun make-table ()
;;   (list '*table*))

;; Exercise 3.24
(defun make-table (&key (same-key #'eq))
  (let ((local-table (list '*table*)))
    (labels ((find-key-value (key table)
               (assoc key table :test same-key))
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
               (cond ((eq m 'lookup) #'lookup)
                     ((eq m 'insert) #'insert)
                     (t (error "Unknown operation: TABLE ~a" m)))))
      #'dispatch)))

(defparameter *operation-table* (make-table :same-key #'equal))

(defun get-proc (op type)
  (funcall (funcall *operation-table* 'lookup) op type))

(defun put-proc (op type item)
  (funcall (funcall *operation-table* 'insert) op type item))

;;; 3.3.4 A Simulation for Digital Circuits

(defun half-adder (a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(defun full-adder (a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(defparameter *inverter-delay* 2)
(defparameter *and-gate-delay* 3)
(defparameter *or-gate-delay* 5)

(defun inverter (input output)
  (labels ((invert-input ()
             (let ((new-value (logical-not (get-signal input))))
               (after-delay *inverter-delay*
                            (lambda ()
                              (set-signal output new-value))))))
    (add-action input #'invert-input)
    'ok))

(defun logical-not (s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (t (error "Invalid signal ~a" s))))

(defun and-gate (a1 a2 output)
  (labels ((and-action-procedure ()
             (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
               (after-delay *and-gate-delay*
                            (lambda ()
                              (set-signal output new-value))))))
    (add-action a1 #'and-action-procedure)
    (add-action a2 #'and-action-procedure)
    'ok))

(defun logical-and (a b)
  (labels ((bool-and (a b)
             (if (= a 1)
                 (= b 1)
                 nil)))
    (if (bool-and a b)
        1
        0)))

;; Exercise 3.28
(defun or-gate (a1 a2 output)
  (labels ((or-gate-procedure ()
             (let ((new-value (logical-or (get-signal a1)
                                          (get-signal a2))))
               (after-delay *or-gate-delay*
                            (lambda ()
                              (set-signal output new-value))))))
    (add-action a1 #'or-gate-procedure)
    (add-action a2 #'or-gate-procedure)
    'ok))

(defun logical-or (a b)
  (labels ((bool-or (a b)
             (if (= a 0)
                 (= b 1)
                 t)))
    (if (bool-or a b)
        1
        0)))

(defun make-wire ()
  (let ((signal-value 0) (action-procedures '()))
    (labels ((set-my-signal (new-value)
               (if (not (= signal-value new-value))
                   (let nil
                     (setf signal-value new-value)
                     (mapcar #'funcall action-procedures))
                   'done))
             (accept-action-procedure (proc)
               (setf action-procedures (cons proc action-procedures))
               (funcall proc))
             (dispatch (m)
               (cond ((eq m 'get-signal) signal-value)
                     ((eq m 'set-signal) #'set-my-signal)
                     ((eq m 'add-action) #'accept-action-procedure)
                     (t (error "Unknown operation: WIRE ~a" m)))))
      #'dispatch)))

;; returns the current value of the signal on the wire.
(defun get-signal (wire)
  (funcall wire 'get-signal))

;; changes the value of the signal on the wire to the new value.
(defun set-signal (wire new-value)
  (funcall (funcall wire 'set-signal) new-value))

;; asserts that the designated procedure should be run whenever the signal on
;; the wire changes value.
(defun add-action (wire action-procedure)
  (funcall (funcall wire 'add-action) action-procedure))

(defun make-agenda ()
  (list 0))

(defparameter *the-agenda* (make-agenda))

(defun after-delay (delay action)
  (add-to-agenda (+ delay (current-time *the-agenda*)) action *the-agenda*))

(defun propagate ()
  (if (empty-agenda *the-agenda*)
      'done
      (let ((first-item (first-agenda-item *the-agenda*)))
        (funcall first-item)
        (remove-first-agenda-item *the-agenda*)
        (propagate))))

(defun probe (name wire)
  (add-action wire
              (lambda ()
                (format t "~a ~a  New-value = ~a ~%"
                        name (current-time *the-agenda*) (get-signal wire)))))

(defun make-time-segment (time queue)
  (cons time queue))

(defun segment-time (s)
  (car s))

(defun segment-queue (s)
  (cdr s))

(defun current-time (agenda)
  (car agenda))

(defun segments (agenda)
  (cdr agenda))

(defun first-segment (agenda)
  (car (segments agenda)))

(defun rest-segments (agenda)
  (cdr (segments agenda)))

(defun empty-agenda (agenda)
  (null (segments agenda)))

(defun add-to-agenda (time action agenda)
  (labels ((belongs-before (segments)
             (or (null segments)
                 (< time (segment-time (car segments)))))
           (make-new-time-segment (time action)
             (let ((q (make-queue)))
               (insert-queue q action)
               (make-time-segment time q)))
           (add-to-segments (segments)
             (if (= (segment-time (car segments)) time)
                 (insert-queue (segment-queue (car segments))
                               action)
                 (let ((rest (cdr segments)))
                   (if (belongs-before rest)
                       (setf (cdr segments)
                             (cons (make-new-time-segment time action)
                                   (cdr segments)))
                       (add-to-segments rest))))))
    (let ((segments (segments agenda)))
      (if (belongs-before segments)
          (setf (cdr agenda) (cons (make-new-time-segment time action)
                                   segments))
          (add-to-segments segments)))))

(defun remove-first-agenda-item (agenda)
  (let ((queue (segment-queue (first-segment agenda))))
    (delete-queue queue)
    (when (empty-queue queue)
      (setf (cdr agenda) (rest-segments agenda)))))

(defun first-agenda-item (agenda)
  (if (empty-agenda agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (setf (car agenda) (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

;;; 3.3.5 Propagation of Constraints

(defun adder (a1 a2 sum)
  (labels ((process-new-value ()
             (cond ((and (has-value a1) (has-value a2))
                    (set-value sum (+ (get-value a1) (get-value a2)) #'me))
                   ((and (has-value a1) (has-value sum))
                    (set-value a2 (- (get-value sum) (get-value a1)) #'me))
                   ((and (has-value a2) (has-value sum))
                    (set-value a1 (- (get-value sum) (get-value a2)) #'me))))
           (process-forget-value ()
             (forget-value sum #'me)
             (forget-value a1 #'me)
             (forget-value a2 #'me)
             (process-new-value))
           (me (request)
             (cond ((eq request 'I-have-a-value) (process-new-value))
                   ((eq request 'I-lost-my-value) (process-forget-value))
                   (t (error "Unknown request: ADDER ~a" request)))))
    (connect a1 #'me)
    (connect a2 #'me)
    (connect sum #'me)
    #'me))

(defun inform-about-value (constraint)
  (funcall constraint 'I-have-a-value))

(defun inform-about-no-value (constraint)
  (funcall constraint 'I-lost-my-value))

(defun multiplier (m1 m2 product)
  (labels ((process-new-value ()
             (cond ((or (and (has-value m1) (= (get-value m1) 0))
                        (and (has-value m2) (= (get-value m2) 0)))
                    (set-value product 0 #'me))
                   ((and (has-value m1) (has-value m2))
                    (set-value product (* (get-value m1) (get-value m2)) #'me))
                   ((and (has-value product) (has-value m1))
                    (set-value m2 (/ (get-value product) (get-value m1)) #'me))
                   ((and (has-value product) (has-value m2))
                    (set-value m1 (/ (get-value product) (get-value m2)) #'me))))
           (process-forget-value ()
             (forget-value product #'me)
             (forget-value m1 #'me)
             (forget-value m2 #'me)
             (process-new-value))
           (me (request)
             (cond ((eq request 'I-have-a-value) (process-new-value))
                   ((eq request 'I-lost-my-value) (process-forget-value))
                   (t (error "Unknown request: MULTIPLIER ~a" request)))))
    (connect m1 #'me)
    (connect m2 #'me)
    (connect product #'me)
    #'me))

(defun constant (value connector)
  (labels ((me (request)
             (error "Unknown request: CONSTANT ~a" request)))
    (connect connector #'me)
    (set-value connector value #'me)
    #'me))

(defun probe2 (name connector)
  (labels ((print-probe (value)
             (format t "Probe: ~a = ~a ~%" name value))
           (process-new-value ()
             (print-probe (get-value connector)))
           (process-forget-value ()
             (print-probe "?"))
           (me (request)
             (cond ((eq request 'I-have-a-value) (process-new-value))
                   ((eq request 'I-lost-my-value) (process-forget-value))
                   (t (error "Unknown request ~a" request)))))
    (connect connector #'me)
    #'me))

(defun make-connector ()
  (let ((value nil) (informant nil) (constraints '()))
    (labels ((set-my-value (newval setter)
               (cond ((not (has-value #'me))
                      (setf value newval)
                      (setf informant setter)
                      (for-each-except setter
                                       #'inform-about-value
                                       constraints))
                     ((not (= value newval))
                      (error "Contradiction ~a ~a" value newval))
                     (t 'ignored)))
             (forget-my-value (retractor)
               (if (eq retractor informant)
                   (let nil
                     (setf informant nil)
                     (for-each-except retractor
                                      #'inform-about-no-value
                                      constraints))
                   'ignored))
             (connect (new-constraint)
               (if (not (member new-constraint constraints))
                   (setf constraints
                         (cons new-constraint constraints)))
               (if (has-value #'me)
                   (inform-about-value new-constraint))
               'done)
             (me (request)
               (cond ((eq request 'has-value) (if informant t nil))
                     ((eq request 'value) value)
                     ((eq request 'set-value) #'set-my-value)
                     ((eq request 'forget) #'forget-my-value)
                     ((eq request 'connect) #'connect)
                     (t (error "Unknown operation: CONNECTOR ~a" request)))))
      #'me)))

(defun for-each-except (exception procedure list)
  (labels ((iter (items)
             (cond ((null items) 'done)
                   ((eq (car items) exception) (iter (cdr items)))
                   (t (funcall procedure (car items))
                      (iter (cdr items))))))
    (iter list)))

(defun has-value (connector)
  (funcall connector 'has-value))

(defun get-value (connector)
  (funcall connector 'value))

(defun set-value (connector new-value informant)
  (funcall (funcall connector 'set-value) new-value informant))

(defun forget-value (connector retractor)
  (funcall (funcall connector 'forget) retractor))

(defun connect (connector new-constraint)
  (funcall (funcall connector 'connect) new-constraint))

(defun celsius-farenheit-converter (c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3.4 Concurrency: Time Is of the Essence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
