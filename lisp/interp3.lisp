;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;; File interp3.lisp: Scheme interpreter with explicit continuations

;;; One bug fix by Cheng Lu Hsu, hsuc@cory.Berkeley.EDU

(requires "interp1")

(defun interp (x env cc)
  "Evaluate the expression x in the environment env,
  and pass the result to the continuation cc."
  (cond
    ((symbolp x) (funcall cc (get-var x env)))
    ((atom x) (funcall cc x))
    ((scheme-macro (first x))                 
     (interp (scheme-macro-expand x) env cc)) 
    ((case (first x)
       (QUOTE  (funcall cc (second x)))
       (BEGIN  (interp-begin (rest x) env cc))
       (SET!   (interp (third x) env
                       #'(lambda (val)
                           (funcall cc (set-var! (second x) 
                                                 val env)))))
       (IF     (interp (second x) env
                       #'(lambda (pred)
                           (interp (if pred (third x) (fourth x))
                                   env cc))))
       (LAMBDA (let ((parms (second x))
                     (code (maybe-add 'begin (rest2 x))))
                 (funcall
                   cc
                   #'(lambda (cont &rest args)
                       (interp code
                               (extend-env parms args env)
                               cont)))))
       (t      (interp-call x env cc))))))

;;; ==============================

(defun scheme (&optional x)
  "A Scheme read-eval-print loop (using interp).
  Handles call/cc by explicitly passing continuations."
  ;; Modified by norvig Jun 11 96 to handle optional argument
  ;; instead of always going into a loop.
  (init-scheme-interp)
  (if x
      (interp x nil #'print)
    (loop (format t "~&==> ")
      (interp (read) nil #'print))))

(defun interp-begin (body env cc)
  "Interpret each element of BODY, passing the last to CC."
  (interp (first body) env
          #'(lambda (val)
              (if (null (rest body))
                  (funcall cc val) ;; fix, hsuc 2/20/93; forgot to call cc
                  (interp-begin (rest body) env cc)))))

(defun interp-call (call env cc)
  "Interpret the call (f x...) and pass the result to CC."
  (map-interp call env
              #'(lambda (fn-and-args)
                  (apply (first fn-and-args)
                         cc
                         (rest fn-and-args)))))

(defun map-interp (list env cc)
  "Interpret each element of LIST, and pass the list to CC."
  (if (null list)
      (funcall cc nil)
      (interp (first list) env
              #'(lambda (x)
                  (map-interp (rest list) env
                              #'(lambda (y)
                                  (funcall cc (cons x y))))))))

;;; ==============================

(defun init-scheme-proc (f)
  "Define a Scheme primitive procedure as a CL function."
  (if (listp f)
      (set-global-var! (first f) 
                       #'(lambda (cont &rest args)
                           (funcall cont (apply (second f) args))))
      (init-scheme-proc (list f f))))

;;; ==============================

(defun call/cc (cc computation)
  "Make the continuation accessible to a Scheme procedure."
  (funcall computation cc
           ;; Package up CC into a Scheme function:
           #'(lambda (cont val)
               (declare (ignore cont))
               (funcall cc val))))

;; Now install call/cc in the global environment
(set-global-var! 'call/cc #'call/cc)
(set-global-var! 'call-with-current-continuation #'call/cc)
