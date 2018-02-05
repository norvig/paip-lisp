;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File cmacsyma.lisp: Canonical Form version of Macsyma.

;;; Bug Fix by dst, Dave_Touretzky@CS.CMU.EDU

(requires "macsyma") ; Only for the infix parser

;;;; rule and expression definitions from "student.lisp"

(defstruct (rule (:type list)) pattern response)
(defstruct (exp (:type list)
                (:constructor mkexp (lhs op rhs)))
  op lhs rhs)

(defun exp-p (x) (consp x))
(defun exp-args (x) (rest x))

(defun binary-exp-p (x)
  (and (exp-p x) (= (length (exp-args x)) 2)))

(proclaim '(inline main-var degree coef
                   var= var> poly make-poly))

(deftype polynomial () 'simple-vector)

(defsetf main-var (p) (val) 
  `(setf (svref (the polynomial ,p) 0) ,val))

(defsetf coef (p i) (val)
  `(setf (svref (the polynomial ,p) (+ ,i 1)) ,val))

(defun main-var (p) (svref (the polynomial p) 0))
(defun coef (p i)   (svref (the polynomial p) (+ i 1)))
(defun degree (p)   (- (length (the polynomial p)) 2))

(defun poly (x &rest coefs)
  "Make a polynomial with main variable x 
  and coefficients in increasing order."
  (apply #'vector x coefs))

(defun make-poly (x degree)
  "Make the polynomial 0 + 0*x + 0*x^2 + ... 0*x^degree"
  (let ((p (make-array (+ degree 2) :initial-element 0)))
    (setf (main-var p) x)
    p))

(defun prefix->canon (x)
  "Convert a prefix Lisp expression to canonical form.
  Exs: (+ (^ x 2) (* 3 x)) => #(x 0 3 1)
       (- (* (- x 1) (+ x 1)) (- (^ x 2) 1)) => 0"
  (cond ((numberp x) x)
        ((symbolp x) (poly x 0 1))
        ((and (exp-p x) (get (exp-op x) 'prefix->canon))
         (apply (get (exp-op x) 'prefix->canon)
                (mapcar #'prefix->canon (exp-args x))))
        (t (error "Not a polynomial: ~a" x))))

(dolist (item '((+ poly+) (- poly-) (* poly*poly) 
                (^ poly^n) (D deriv-poly)))
  (setf (get (first item) 'prefix->canon) (second item)))

(defun poly+ (&rest args)
  "Unary or binary polynomial addition."
  (ecase (length args)
    (1 (first args))
    (2 (poly+poly (first args) (second args)))))

(defun poly- (&rest args)
  "Unary or binary polynomial subtraction."
  (ecase (length args)
    (0 0)
    (1 (poly*poly -1 (first args)))
    (2 (poly+poly (first args) (poly*poly -1 (second args))))))

(defun var= (x y) (eq x y))

(defun var> (x y) (string> x y))

(defun poly+poly (p q)
  "Add two polynomials."
  (normalize-poly
    (cond
      ((numberp p)                      (k+poly p q))
      ((numberp q)                      (k+poly q p))
      ((var= (main-var p) (main-var q)) (poly+same p q))
      ((var> (main-var q) (main-var p)) (k+poly q p))
      (t                                (k+poly p q)))))

(defun k+poly (k p)
  "Add a constant k to a polynomial p."
  (cond ((eql k 0) p)                 ;; 0 + p = p
        ((and (numberp k) (numberp p))
         (+ k p))                     ;; Add numbers
        (t (let ((r (copy-poly p)))   ;; Add k to x^0 term of p
             (setf (coef r 0) (poly+poly (coef r 0) k))
             r))))

(defun poly+same (p q)
  "Add two polynomials with the same main variable."
  ;; First assure that q is the higher degree polynomial
  (if (> (degree p) (degree q))
      (poly+same q p)
      ;; Add each element of p into r (which is a copy of q).
      (let ((r (copy-poly q)))
        (loop for i from 0 to (degree p) do
              (setf (coef r i) (poly+poly (coef r i) (coef p i))))
        r)))

(defun copy-poly (p)
  "Make a copy a polynomial."
  (copy-seq p))

(defun poly*poly (p q)
  "Multiply two polynomials."
  (normalize-poly
    (cond
      ((numberp p)                      (k*poly p q))
      ((numberp q)                      (k*poly q p))
      ((var= (main-var p) (main-var q)) (poly*same p q))
      ((var> (main-var q) (main-var p)) (k*poly q p))
      (t                                (k*poly p q)))))

(defun k*poly (k p)
  "Multiply a polynomial p by a constant factor k."
  (cond
    ((eql k 0)         0)       ;; 0 * p = 0
    ((eql k 1)         p)       ;; 1 * p = p
    ((and (numberp k)
          (numberp p)) (* k p)) ;; Multiply numbers
    (t ;; Multiply each coefficient
     (let ((r (make-poly (main-var p) (degree p))))
       ;; Accumulate result in r;  r[i] = k*p[i]
       (loop for i from 0 to (degree p) do
             (setf (coef r i) (poly*poly k (coef p i))))
       r))))

(defun poly*same (p q)
  "Multiply two polynomials with the same variable."
  ;; r[i] = p[0]*q[i] + p[1]*q[i-1] + ...
  (let* ((r-degree (+ (degree p) (degree q)))
         (r (make-poly (main-var p) r-degree)))
    (loop for i from 0 to (degree p) do
          (unless (eql (coef p i) 0)
            (loop for j from 0 to (degree q) do
                  (setf (coef r (+ i j))
                        (poly+poly (coef r (+ i j))
                                   (poly*poly (coef p i)
                                              (coef q j)))))))
    r))

(defun normalize-poly (p)
  "Alter a polynomial by dropping trailing zeros."
  (if (numberp p)
      p
      (let ((p-degree (- (position 0 p :test (complement #'eql)
                                       :from-end t)
                         1)))
        (cond ((<= p-degree 0) (normalize-poly (coef p 0)))
              ((< p-degree (degree p))
               (delete 0 p :start p-degree))
              (t p)))))

(defun deriv-poly (p x)
  "Return the derivative, dp/dx, of the polynomial p."
  ;; If p is a number or a polynomial with main-var > x,
  ;; then p is free of x, and the derivative is zero;
  ;; otherwise do real work.
  ;; But first, make sure X is a simple variable,
  ;; of the form #(X 0 1).
  (assert (and (typep x 'polynomial) (= (degree x) 1)
	       (eql (coef x 0) 0) (eql (coef x 1) 1)))
  (cond
    ((numberp p) 0)
    ((var> (main-var p) (main-var x)) 0)
    ((var= (main-var p) (main-var x))
     ;; d(a + bx + cx^2 + dx^3)/dx = b + 2cx + 3dx^2
     ;; So, shift the sequence p over by 1, then
     ;; put x back in, and multiply by the exponents
     (let ((r (subseq p 1)))
       (setf (main-var r) (main-var x))
       (loop for i from 1 to (degree r) do
             (setf (coef r i) (poly*poly (+ i 1) (coef r i))))
       (normalize-poly r)))
    (t ;; Otherwise some coefficient may contain x.  Ex:
     ;; d(z + 3x + 3zx^2 + z^2x^3)/dz
     ;; = 1 +  0 +  3x^2 +  2zx^3
     ;; So copy p, and differentiate the coefficients.
     (let ((r (copy-poly p)))
       (loop for i from 0 to (degree p) do
             (setf (coef r i) (deriv-poly (coef r i) x)))
       (normalize-poly r)))))

(defun prefix->infix (exp)
  "Translate prefix to infix expressions.
  Handles operators with any number of args."
  (if (atom exp)
      exp
      (intersperse
        (exp-op exp)
        (mapcar #'prefix->infix (exp-args exp)))))

(defun intersperse (op args)
  "Place op between each element of args.
  Ex: (intersperse '+ '(a b c)) => '(a + b + c)"
  (if (length=1 args)
      (first args)
      (rest (loop for arg in args
               collect op
               collect arg))))

(defun canon->prefix (p)
  "Convert a canonical polynomial to a lisp expression."
  (if (numberp p)
      p
      (args->prefix
        '+ 0
        (loop for i from (degree p) downto 0
              collect (args->prefix
                        '* 1
                        (list (canon->prefix (coef p i))
                              (exponent->prefix
                                (main-var p) i)))))))

(defun exponent->prefix (base exponent)
  "Convert canonical base^exponent to prefix form."
  (case exponent
    (0 1)
    (1 base)
    (t `(^ ,base ,exponent))))

(defun args->prefix (op identity args)
  "Convert arg1 op arg2 op ... to prefix form."
  (let ((useful-args (remove identity args)))
    (cond ((null useful-args) identity)
          ((and (eq op '*) (member 0 args)) 0)
          ((length=1 args) (first useful-args))
          (t (cons op (mappend
                        #'(lambda (exp)
                            (if (starts-with exp op)
                                (exp-args exp)
                                (list exp)))
                        useful-args))))))

(defun canon (infix-exp)
  "Canonicalize argument and convert it back to infix"
  (prefix->infix (canon->prefix (prefix->canon (infix->prefix infix-exp)))))

(defun canon-simplifier ()
  "Read an expression, canonicalize it, and print the result."
  (loop
    (print 'canon>)
    (print (canon (read)))))

(defun poly^n (p n)
  "Raise polynomial p to the nth power, n>=0."
  ;; Uses the binomial theorem
  (check-type n (integer 0 *))
  (cond
    ((= n 0) 1)
    ((integerp p) (expt p n))
    (t ;; First: split the polynomial p = a + b, where
     ;; a = k*x^d and b is the rest of p
     (let ((a (make-poly (main-var p) (degree p)))
           (b (normalize-poly (subseq p 0 (- (length p) 1))))
           ;; Allocate arrays of powers of a and b:
           (a^n (make-array (+ n 1)))
           (b^n (make-array (+ n 1)))
           ;; Initialize the result:
           (result (make-poly (main-var p) (* (degree p) n))))
       (setf (coef a (degree p)) (coef p (degree p)))
       ;; Second: Compute powers of a^i and b^i for i up to n
       (setf (aref a^n 0) 1)
       (setf (aref b^n 0) 1)
       (loop for i from 1 to n do
             (setf (aref a^n i) (poly*poly a (aref a^n (- i 1))))
             (setf (aref b^n i) (poly*poly b (aref b^n (- i 1)))))
       ;; Third: add the products into the result,
       ;; so that result[i] = (n choose i) * a^i * b^(n-i)
       (let ((c 1)) ;; c helps compute (n choose i) incrementally
         (loop for i from 0 to n do
               (p-add-into! result c
                            (poly*poly (aref a^n i)
                                 (aref b^n (- n i))))
               (setf c (/ (* c (- n i)) (+ i 1)))))
       (normalize-poly result)))))

(defun p-add-into! (result c p)
  "Destructively add c*p into result."
  (if (or (numberp p)
          (not (var= (main-var p) (main-var result))))
      (setf (coef result 0)
            (poly+poly (coef result 0) (poly*poly c p)))
      (loop for i from 0 to (degree p) do
            (setf (coef result i)
                  (poly+poly (coef result i) (poly*poly c (coef p i))))))
  result)

(defun make-rat (numerator denominator)
  "Build a rational: a quotient of two polynomials."
  (if (numberp denominator)
      (k*poly (/ 1 denominator) numerator)
      (cons numerator denominator)))

(defun rat-numerator (rat)
  "The numerator of a rational expression."
  (typecase rat
    (cons (car rat))
    (number (numerator rat))
    (t rat)))

(defun rat-denominator (rat)
  "The denominator of a rational expression."
  (typecase rat
    (cons (cdr rat))
    (number (denominator rat))
    (t 1)))

(defun rat*rat (x y)
  "Multiply rationals: a/b * c/d = a*c/b*d"
  (poly/poly (poly*poly (rat-numerator x)
                        (rat-numerator y))
             (poly*poly (rat-denominator x)
                        (rat-denominator y))))

(defun rat+rat (x y)
  "Add rationals: a/b + c/d = (a*d + c*b)/b*d"
  ;; Bug fix by dst 4/6/92; b and c were switched
  (let ((a (rat-numerator x))
        (b (rat-denominator x))
        (c (rat-numerator y))
        (d (rat-denominator y)))
    (poly/poly (poly+poly (poly*poly a d) (poly*poly c b))
               (poly*poly b d))))

(defun rat/rat (x y)
  "Divide rationals: a/b / c/d = a*d/b*c"
  (rat*rat x (make-rat (rat-denominator y) (rat-numerator y))))

