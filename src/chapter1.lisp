(in-package :cl-user)
(defpackage cl-sicp.chapter1
  (:use :cl)
  (:export :sum-of-squares
           :improve
           :average
           :square
           :primep
           :fast-primep
           :expmod
           :cube
           :sum
           :fixed-point
           :*tolerance*
           :average-damp))
(in-package :cl-sicp.chapter1)

;;; Chapter 1. Building Abstractions with Procedures

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.1 The Elements of Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 1.1.1 Expressions

;;; 1.1.2 Naming and the Environment

(defparameter *size* 2)

(defparameter *pi* 3.14159)

(defparameter *radius* 10)

(defparameter *circumference* (* 2 *pi* *radius*))

;;; 1.1.3 Evaluating Combinations

;;; 1.1.4 Compound Procedures

(defun square (x)
  (* x x))

(defun sum-of-squares (x y)
  (+ (square x) (square y)))

(defun f (a)
  (sum-of-squares (+ a 1) (* a 2)))

;;; 1.1.5 The Substitution Model for Procedure Application

;;; 1.1.6 Conditional Expressions and Predicates

(defun abs1 (x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(defun abs2 (x)
  (cond ((< x 0) (- x))
        (t       x)))

(defun abs3 (x)
  (if (< x 0)
      (- x)
      x))

(defun >=1 (x y)
  (or (> x y) (= x y)))

(defun >=2 (x y)
  (not (< x y)))

;;; 1.1.7 Example: Square Roots by Newton's Method

(defun sqrt-iter (guess x)
  (if (good-enough guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun average (x y)
  (/ (+ x y) 2))

(defun good-enough (guess x)
  (< (abs (- (square guess) x)) 0.001))

(defun my-sqrt1 (x)
  (sqrt-iter 1.0 x))

;;; 1.1.8 Procedures as Black-Box Abstractions

(defun my-sqrt2 (x)
  (labels ((good-enough (guess x)
             (< (abs (- (square guess) x)) 0.001))
           (improve (guess x)
             (average guess (/ x guess)))
           (sqrt-iter (guess x)
             (if (good-enough guess x)
                 guess
                 (sqrt-iter (improve guess x) x))))
    (sqrt-iter 1.0 x)))

(defun my-sqrt3 (x)
  (labels ((good-enough (guess)
             (< (abs (- (square guess) x)) 0.001))
           (improve (guess)
             (average guess (/ x guess)))
           (sqrt-iter (guess)
             (if (good-enough guess)
                 guess
                 (sqrt-iter (improve guess)))))
    (sqrt-iter 1.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.2 Procedures and the Processes They Generate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 1.2.1 Linear Recursion and Iteration

(defun factorial1 (n)
  (if (= n 1)
      1
      (* n (factorial1 (1- n)))))

(defun factorial2 (n)
  (fact-iter 1 1 n))

(defun fact-iter (product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (1+ counter)
                 max-count)))

(defun factorial3 (n)
  (labels ((iter (product counter)
             (if (> counter n)
                 product
                 (iter (* counter product)
                       (1+ counter)))))
    (iter 1 1)))

;;; 1.2.2 Tree Recursion

(defun fib1 (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t       (+ (fib1 (- n 1)) (fib1 (- n 2))))))

(defun fib2 (n)
  (fib-iter 1 0 n))

(defun fib-iter (a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(defun count-change (amount)
  (cc amount 5))

(defun cc (amount kinds-of-coins)
  (cond ((= amount 0)
         1)
        ((or (< amount 0) (= kinds-of-coins 0))
         0)
        (t
         (+ (cc amount
                (- kinds-of-coins 1))
            (cc (- amount (first-denomination kinds-of-coins))
                kinds-of-coins)))))

(defun first-denomination (kinds-of-coins)
  (cond ((= kinds-of-coins 1)  1)
        ((= kinds-of-coins 2)  5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;;; 1.2.3 Orders of Growth

;;; 1.2.4 Exponentiation

(defun my-expt1 (b n)
  (if (= n 0)
      1
      (* b (my-expt1 b (1- n)))))

(defun my-expt2 (b n)
  (expt-iter b n 1))

(defun expt-iter (b counter product)
  (if (= counter 0)
      product
      (expt-iter b (1- counter) (* b product))))

(defun fast-expt (b n)
  (cond ((= n 0)   1)
        ((evenp n) (square (fast-expt b (/ n 2))))
        (t         (* b (fast-expt b (1- n))))))

;;; 1.2.5 Greatest Common Divisors

(defun my-gcd (a b)
  (if (= b 0)
      a
      (my-gcd b (rem a b))))

;;; 1.2.6 Example: Testing for Primality

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides test-divisor n) test-divisor)
        (t (find-divisor n (+ test-divisor 1)))))

(defun divides (a b)
  (= (rem b a) 0))

(defun primep (n)
  (= n (smallest-divisor n)))

(defun expmod (base exp m)
  (cond ((= exp 0)   1)
        ((evenp exp) (rem (square (expmod base (/ exp 2) m)) m))
        (t           (rem (* base (expmod base (1- exp) m)) m))))

(defun fermat-test (n)
  (labels ((try-it (a)
             (= (expmod a n n) a)))
    (try-it (1+ (random (1- n))))))

(defun fast-primep (n times)
  (cond ((= times 0)     t)
        ((fermat-test n) (fast-primep n (1- times)))
        (t               nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.3 Formulating Abstractions with Higher-Order Procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cube (x)
  (* x x x))

;;; 1.3.1 Procedures as Arguments

(defun sum-integers (a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(defun sum-cubes (a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))

(defun pi-sum (a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

(defun sum (term a next b)
  (if (> a b)
      0
      (+ (funcall term a)
         (sum term (funcall next a) next b))))

(defun inc (n)
  (+ n 1))

(defun sum-cubes2 (a b)
  (sum #'cube a #'inc b))

(defun my-identity (x) x)

(defun sum-integers2 (a b)
  (sum #'my-identity a #'inc b))

(defun pi-sum2 (a b)
  (labels ((pi-term (x)
             (/ 1.0 (* x (+ x 2))))
           (pi-next (x)
             (+ x 4)))
    (sum #'pi-term a #'pi-next b)))

(defun integral (f a b dx)
  (labels ((add-dx (x)
             (+ x dx)))
    (* (sum f (+ a (/ dx 2.0)) #'add-dx b)
       dx)))

;;; 1.3.2 Constructing Procedures Using Lambda

(defun pi-sum3 (a b)
  (sum #'(lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       #'(lambda (x) (+ x 4))
       b))

(defun integral2 (f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          #'(lambda (x) (+ x dx))
          b)
     dx))

(defun f2 (x y)
  (labels ((f-helper (a b)
             (+ (* x (square a))
                (* y b)
                (* a b))))
    (f-helper (+ 1 (* x y))
              (- 1 y))))

(defun f3 (x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(defun f4 (x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;;; 1.3.3 Procedures as General Methods

(defun search-root (f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough neg-point pos-point)
        midpoint
        (let ((test-value (funcall f midpoint)))
          (cond ((plusp test-value)  (search-root f neg-point midpoint))
                ((minusp test-value) (search-root f midpoint pos-point))
                (t                   midpoint))))))

(defun close-enough (x y)
  (< (abs (- x y)) 0.001))

(defun half-interval-method (f a b)
  (let ((a-value (funcall f a))
        (b-value (funcall f b)))
    (cond ((and (minusp a-value) (plusp b-value))
           (search-root f a b))
          ((and (minusp b-value) (plusp a-value))
           (search-root f b a))
          (t
           (error "Values are not of opposite sign")))))

(defparameter *tolerance* 0.00001)

(defun fixed-point (f first-guess)
  (labels ((close-enough (v1 v2)
             (< (abs (- v1 v2)) *tolerance*))
           (try (guess)
             (let ((next (funcall f guess)))
               (if (close-enough guess next)
                   next
                   (try next)))))
    (try first-guess)))

(defun fixed-point-sqrt (x)
  (fixed-point #'(lambda (y)
                   (average y (/ x y))) 1.0))

;;; 1.3.4 Procedures as Returned Values

(defun average-damp (f)
  #'(lambda (x)
      (average x (funcall f x))))

(defun fixed-point-sqrt2 (x)
  (fixed-point (average-damp #'(lambda (y)
                                 (/ x y))) 1.0))

(defun cube-root (x)
  (fixed-point (average-damp #'(lambda (y)
                                 (/ x (square y)))) 1.0))

(defparameter *dx* 0.00001)

(defun deriv (g)
  #'(lambda (x)
      (/ (- (funcall g (+ x *dx*)) (funcall g x)) *dx*)))

(defun newton-transform (g)
  #'(lambda (x)
      (- x (/ (funcall g x) (funcall (deriv g) x)))))

(defun newtons-method (g guess)
  (fixed-point (newton-transform g) guess))

(defun fixed-point-sqrt3 (x)
  (newtons-method #'(lambda (y)
                      (- (square y) x)) 1.0))

(defun fixed-point-of-transform (g transform guess)
  (fixed-point (funcall transform g) guess))

(defun fixed-point-sqrt4 (x)
  (fixed-point-of-transform #'(lambda (y)
                                (/ x y))
                            #'average-damp
                            1.0))

(defun fixed-point-sqrt5 (x)
  (fixed-point-of-transform #'(lambda (y)
                                (- (square y) x))
                            #'newton-transform
                            1.0))
