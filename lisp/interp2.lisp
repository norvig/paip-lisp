;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;; File interp2.lisp: Tail-recursive Scheme interpreter.

(requires "interp1")

(defun interp (x &optional env)
  "Evaluate the expression x in the environment env.
  This version is properly tail-recursive."
  (prog ()
    :INTERP
    (return
      (cond
        ((symbolp x) (get-var x env))
        ((atom x) x)
        ((scheme-macro (first x)) 
         (setf x (scheme-macro-expand x)) (go :INTERP))
        ((case (first x)
           (QUOTE  (second x))
           (BEGIN  (pop x) ; pop off the BEGIN to get at the args
                   ;; Now interpret all but the last expression
                   (loop while (rest x) do (interp (pop x) env))
                   ;; Finally, rename the last expression as x
                   (setf x (first x))
                   (GO :INTERP))
           (SET!   (set-var! (second x) (interp (third x) env) env))
           (IF     (setf x (if (interp (second x) env)
                               (third x)
                               (fourth x)))
                   ;; That is, rename the right expression as x
                   (GO :INTERP))
           (LAMBDA (make-proc :env env :parms (second x)
                              :code (maybe-add 'begin (rest2 x))))
           (t      ;; a procedure application
                   (let ((proc (interp (first x) env))
                         (args (mapcar #'(lambda (v) (interp v env))
                                       (rest x))))
                     (if (proc-p proc)
                         ;; Execute procedure with rename+goto
                         (progn
                           (setf x (proc-code proc))
                           (setf env (extend-env (proc-parms proc) args
                                                 (proc-env proc)))
                           (GO :INTERP))
                         ;; else apply primitive procedure
                         (apply proc args))))))))))

(defstruct (proc (:print-function print-proc))
  "Represent a Scheme procedure"
  code (env nil) (name nil) (parms nil))

(defun print-proc (proc &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (format stream "{~a}" (or (proc-name proc) '??)))
