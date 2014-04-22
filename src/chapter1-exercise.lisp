(in-package :cl-user)
(defpackage cl-sicp.chapter1-exercise
  (:use :cl)
  (:import-from :cl-sicp.chapter1
                :sum-of-squares
                :improve
                :average
                :square
                :primep
                :fast-primep
                :expmod
                :sum))
(in-package :cl-sicp.chapter1-exercise)

;;; Exercise 1.2

(defun ex12 ()
  (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
     (* 3 (- 6 2) (- 2 7))))

;;; Exercise 1.3

(defun ex13 (x y z)
  (cond ((and (<= x y) (<= x z)) (sum-of-squares y z))
        ((<= y z)                (sum-of-squares x z))
        (t                       (sum-of-squares x y))))

;;; Exercise 1.4

(defun a-plus-abs-b (a b)
  (funcall (if (> b 0)
               #'+
               #'-) a b))

;;; Exercise 1.6

;; Applicative-order evaluation.
(defun new-if (predicate then-clause else-clause)
  (cond (predicate then-clause)
        (t else-clause)))

;;; Exercise 1.7

(defun sqrt-iter (guess prev-guess x)
  (if (good-enough guess prev-guess)
      guess
      (sqrt-iter (improve guess x) guess x)))

(defun good-enough (guess prev-guess)
  (< (/ (abs (- guess prev-guess)) guess) 0.001))

(defun my-sqrt (x)
  (sqrt-iter 1.0 0.0 x))

;;; Exercise 1.8

(defun improve-cube (guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess)) 3))

(defun cbrt-iter (guess prev-guess x)
  (if (good-enough guess prev-guess)
      guess
      (cbrt-iter (improve-cube guess x) guess x)))

(defun cbrt (x)
  (cbrt-iter 1.0 0 x))

;;; Exercise 1.9

;; recursive process
(defun plus1 (a b)
  (if (= a 0)
      b
      (1+ (plus1 (1- a) b))))

;; iterative process
(defun plus2 (a b)
  (if (= a 0)
      b
      (plus2 (1- a) (1+ b))))

;;; Exercise 1.10

(defun A (x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (t (A (- x 1) (A x (- y 1))))))

(defun f (n)
  (A 0 n))

(defun g (n)
  (A 1 n))

(defun h (n)
  (A 2 n))

(defun k (n)
  (* 5 n n))

;;; Exercise 1.11

(defun ex111-1 (n)
  (cond ((< n 3) n)
        (t (+      (ex111-1 (- n 1))
              (* 2 (ex111-1 (- n 2)))
              (* 3 (ex111-1 (- n 3)))))))

(defun ex111-2 (n)
  (labels ((f-iter (a b c counter)
             (if (= counter 0)
                 c
                 (f-iter (+ a (* 2 b) (* 3 c)) a b (1- counter)))))
    (f-iter 2 1 0 n)))

;;; Exercise 1.12

(defun ex112 (row col)
  (cond ((or (> col row) (< col 0)) 0)
        ((= col 1)       1)
        ((+ (ex112 (1- row) (1- col))
            (ex112 (1- row) col)))))

;;; Exercise 1.15

(defun cube (x)
  (* x x x))

(defun p (x)
  (- (* 3 x) (* 4 (cube x))))

(defun sine (angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;;; Exercise 1.16

(defun ex116 (b n)
  (labels ((f-iter (a b n)
             (cond ((= n 0) a)
                   ((evenp n) (f-iter a (square b) (/ n 2)))
                   (t (f-iter (* a b) b (1- n))))))
    (f-iter 1 b n)))

;;; Exercise 1.17

(defun prod (a b)
  (if (= b 0)
      0
      (+ a (prod a (1- b)))))

(defun double (n)
  (+ n n))

(defun halve (n)
  (/ n 2))

(defun ex117 (a b)
  (cond ((= b 0) 0)
        ((evenp b) (double (ex117 a (halve b))))
        (t (+ a (ex117 a (1- b))))))

;;; Exercise 1.18

(defun ex118 (a b)
  (labels ((f-iter (x a b)
             (cond ((= b 0) x)
                   ((evenp b) (f-iter x (double a) (halve b)))
                   (t (f-iter (+ a x) a (1- b))))))
    (f-iter 0 a b)))

;;; Exercise 1.19

(defun fib (n)
  (fib-iter 1 0 0 1 n))

(defun fib-iter (a b p q count)
  (cond ((= count 0) b)
        ((evenp count) (fib-iter a
                                 b
                                 (+ (square p) (square q))
                                 (+ (square q) (double (* p q)))
                                 (/ count 2)))
        (t (fib-iter (+ (* b q) (* a q) (* a p))
                     (+ (* b p) (* a q))
                     p
                     q
                     (1- count)))))

;;; Exercise 1.22

(defun timed-prime-test (n)
  (fresh-line)
  (format t "~a" n)
  (start-prime-test n (get-internal-real-time)))

(defun start-prime-test (n start-time)
  (when (primep n)
    (report-prime (- (get-internal-real-time) start-time))))

(defun report-prime (elapsed-time)
  (format t " *** ~a" (coerce (/ elapsed-time internal-time-units-per-second)
                              'float)))

(defun search-for-primes (a b)
  (if (evenp a)
      (search-for-primes (+ a 1) b)
      (when (< a b)
        (timed-prime-test a)
        (search-for-primes (+ a 2) b))))

;;; Exercise 1.23

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides test-divisor n) test-divisor)
        (t (find-divisor n (next test-divisor)))))

(defun divides (a b)
  (= (rem b a) 0))

(defun next (n)
  (if (= n 2)
      3
      (+ n 2)))

(defun primep2 (n)
  (= n (smallest-divisor n)))

(defun timed-prime-test2 (n)
  (labels ((start-prime-test (n start-time)
             (when (primep2 n)
               (report-prime (- (get-internal-real-time) start-time)))))
    (fresh-line)
    (format t "~a" n)
    (start-prime-test n (get-internal-real-time))))

(defun search-for-primes2 (a b)
  (if (evenp a)
      (search-for-primes2 (+ a 1) b)
      (when (< a b)
        (timed-prime-test2 a)
        (search-for-primes2 (+ a 2) b))))

;;; Exercise 1.24

(defun timed-prime-test3 (n)
  (labels ((start-prime-test (n start-time)
             (when (fast-primep n 100)
               (report-prime (- (get-internal-real-time) start-time)))))
    (fresh-line)
    (format t "~a" n)
    (start-prime-test n (get-internal-real-time))))

(defun search-for-primes3 (a b)
  (if (evenp a)
      (search-for-primes3 (+ a 1) b)
      (when (< a b)
        (timed-prime-test3 a)
        (search-for-primes3 (+ a 2) b))))

;;; Exercise 1.27

(defun fermat-test (n a)
  (= (expmod a n n) a))

(defun fermat-full (n)
  (labels ((f-iter (a)
             (cond ((= a 1) t)
                   ((not (fermat-test n a)) nil)
                   (t (f-iter (1- a))))))
    (f-iter (1- n))))

;;; Exercise 1.28

(defun square-check (x m)
  (if (and (not (or (= x 1) (= x (1- m))))
           (= (rem (square x) m) 1))
      0
      (rem (square x) m)))

(defun expmod2 (base exp m)
  (cond ((= exp 0) 1)
        ((evenp exp) (square-check (expmod2 base (/ exp 2) m) m))
        (t (rem (* base (expmod2 base (1- exp) m))
                m))))

(defun miller-rabin-test (n)
  (labels ((try-it (a)
             (= (expmod2 a (1- n) n) 1)))
    (try-it (+ 2 (random (- n 2))))))

(defun fast-primep2 (n times)
  (cond ((= times 0) t)
        ((miller-rabin-test n) (fast-primep2 n (1- times)))
        (t nil)))

;;; Exercise 1.29

(defun simpsons-rule (f a b n)
  (let ((h (/ (- b a) n)))
    (labels ((term (k)
               (let ((y (funcall f (+ a (* k h)))))
                 (cond ((or (= k 0) (= k n)) y)
                       ((evenp k) (* 2 y))
                       (t (* 4 y))))))
      (* (sum #'term 0 #'1+ n)
         (/ h 3.0)))))

;;; Exercise 1.30

(defun sum-iter (term a next b)
  (labels ((iter (a result)
             (if (> a b)
                 result
                 (iter (funcall next a) (+ (funcall term a) result)))))
    (iter a 0)))

;;; Exercise 1.31

(defun product (term a next b)
  (if (> a b)
      1
      (* (funcall term a)
         (product term (funcall next a) next b))))

(defun factorial (n)
  (product #'identity 1 #'1+ n))

(defun wallis-product (n)
  (labels ((term (x)
             (/ (* 4.0 (square x))
                (1- (* 4.0 (square x))))))
    (* 2.0 (product #'term 1 #'1+ n))))

(defun product-iter (term a next b)
  (labels ((iter (a result)
             (if (> a b)
                 result
                 (iter (funcall next a) (* (funcall term a) result)))))
    (iter a 1)))

;;; Exercise 1.32

(defun accumulate (combiner null-value term a next b)
  (if (> a b)
      null-value
      (funcall combiner
               (funcall term a)
               (accumulate combiner null-value term (funcall next a) next b))))

(defun accumulate-sum (term a next b)
  (accumulate #'+ 0 term a next b))

(defun accumulate-product (term a next b)
  (accumulate #'* 1 term a next b))

(defun accumulate-iter (combiner null-value term a next b)
  (labels ((iter (a result)
             (if (> a b)
                 result
                 (iter (funcall next a)
                       (funcall combiner (funcall term a) result)))))
    (iter a null-value)))

;;; Exercise 1.33

(defun filtered-accumulate (combiner null-value term a next b filter)
  (cond
    ((> a b) null-value)
    ((funcall filter a) (funcall combiner
                                 (funcall term a)
                                 (filtered-accumulate combiner null-value
                                                      term (funcall next a) next b
                                                      filter)))
    (t (filtered-accumulate combiner null-value
                            term (funcall next a) next b
                            filter))))

(defun sum-squares-prime (a b)
  (filtered-accumulate #'+ 0 #'square a #'1+ b #'primep))

(defun product-relatively-prime (n)
  (labels ((relatively-prime (k)
             (= (gcd k n) 1)))
    (filtered-accumulate #'* 1 #'identity 1 #'1+ (1- n)
                         #'relatively-prime)))
