;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File compile1.lisp: Simplest version of Scheme compiler

(requires "interp1") ; Uses the Scheme macro facility

(defun comp (x env)
  "Compile the expression x into a list of instructions"
  (cond
    ((symbolp x) (gen-var x env))
    ((atom x) (gen 'CONST x))
    ((scheme-macro (first x)) (comp (scheme-macro-expand x) env))
    ((case (first x)
       (QUOTE  (gen 'CONST (second x)))
       (BEGIN  (comp-begin (rest x) env))
       (SET!   (seq (comp (third x) env) (gen-set (second x) env)))
       (IF     (comp-if (second x) (third x) (fourth x) env))
       (LAMBDA (gen 'FN (comp-lambda (second x) (rest (rest x)) env)))
       ;; Procedure application:
       ;; Compile args, then fn, then the call
       (t      (seq (mappend #'(lambda (y) (comp y env)) (rest x))
                    (comp (first x) env)
                              (gen 'call (length (rest x)))))))))

;;; ==============================

(defun comp-begin (exps env)
  "Compile a sequence of expressions, popping all but the last."
  (cond ((null exps) (gen 'CONST nil))
        ((length=1 exps) (comp (first exps) env))
        (t (seq (comp (first exps) env)
                (gen 'POP)
                (comp-begin (rest exps) env)))))

;;; ==============================

(defun comp-if (pred then else env)
  "Compile a conditional expression."
  (let ((L1 (gen-label))
        (L2 (gen-label)))
    (seq (comp pred env) (gen 'FJUMP L1)
         (comp then env) (gen 'JUMP L2)
         (list L1) (comp else env)
         (list L2))))

;;; ==============================

(defstruct (fn (:print-function print-fn))
  code (env nil) (name nil) (args nil))

(defun comp-lambda (args body env)
  "Compile a lambda form into a closure with compiled code."
  (assert (and (listp args) (every #'symbolp args)) ()
          "Lambda arglist must be a list of symbols, not ~a" args)
  ;; For now, no &rest parameters.  
  ;; The next version will support Scheme's version of &rest
  (make-fn
    :env env :args args
    :code (seq (gen 'ARGS (length args))
               (comp-begin body (cons args env))
               (gen 'RETURN))))

;;; ==============================

(defvar *label-num* 0)

(defun compiler (x)
  "Compile an expression as if it were in a parameterless lambda."
  (setf *label-num* 0)
  (comp-lambda '() (list x) nil))

(defun comp-show (x)
  "Compile an expression and show the resulting code"
   (show-fn (compiler x))
  (values))

;;; ==============================

(defun gen (opcode &rest args)
  "Return a one-element list of the specified instruction."
  (list (cons opcode args)))

(defun seq (&rest code)
  "Return a sequence of instructions"
  (apply #'append code))

(defun gen-label (&optional (label 'L))
  "Generate a label (a symbol of the form Lnnn)"
  (intern (format nil "~a~d" label (incf *label-num*))))

;;; ==============================

(defun gen-var (var env)
  "Generate an instruction to reference a variable's value."
  (let ((p (in-env-p var env)))
    (if p
        (gen 'LVAR (first p) (second p) ";" var)
        (gen 'GVAR var))))

(defun gen-set (var env)
  "Generate an instruction to set a variable to top-of-stack."
  (let ((p (in-env-p var env)))
    (if p
        (gen 'LSET (first p) (second p) ";" var)
        (gen 'GSET var))))

;;; ==============================

(def-scheme-macro define (name &rest body)
  (if (atom name)
      `(name! (set! ,name . ,body) ',name)
      (scheme-macro-expand
         `(define ,(first name) 
            (lambda ,(rest name) . ,body)))))

(defun name! (fn name)
  "Set the name field of fn, if it is an un-named fn."
  (when (and (fn-p fn) (null (fn-name fn)))
    (setf (fn-name fn) name))
  name)

;; This should also go in init-scheme-interp:
(set-global-var! 'name! #'name!)

(defun print-fn (fn &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (format stream "{~a}" (or (fn-name fn) '??)))

(defun show-fn (fn &optional (stream *standard-output*) (depth 0))
  "Print all the instructions in a function.
  If the argument is not a function, just princ it, 
  but in a column at least 8 spaces wide."
  (if (not (fn-p fn))
      (format stream "~8a" fn)
      (progn
        (fresh-line)
        (incf depth 8)
        (dolist (instr (fn-code fn))
          (if (label-p instr)
              (format stream "~a:" instr)
              (progn
                (format stream "~VT" depth)
                (dolist (arg instr)
                  (show-fn arg stream depth))
                (fresh-line)))))))

(defun label-p (x) "Is x a label?" (atom x))

(defun in-env-p (symbol env)
  "If symbol is in the environment, return its index numbers."
  (let ((frame (find symbol env :test #'find)))
    (if frame (list (position frame env) (position symbol frame)))))
