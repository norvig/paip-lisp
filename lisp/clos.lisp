;;; -*- Mode: Lisp; Syntax: Common-Lisp;  -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File clos.lisp: Object-oriented programming examples

(defstruct account 
  (name "") (balance 0.00) (interest-rate .06))

(defun account-withdraw (account amt)
  "Make a withdrawal from this account."
  (if (<= amt (account-balance account))
      (decf (account-balance account) amt)
      'insufficient-funds))

(defun account-deposit (account amt)
  "Make a deposit to this account."
  (incf (account-balance account) amt))

(defun account-interest (account)
  "Accumulate interest in this account."
  (incf (account-balance account)
        (* (account-interest-rate account)
           (account-balance account))))

;;; ==============================

(defun new-account (name &optional (balance 0.00)
                    (interest-rate .06))
  "Create a new account that knows the following messages:"
  #'(lambda (message)
      (case message
        (withdraw #'(lambda (amt)
                      (if (<= amt balance)
                          (decf balance amt)
                          'insufficient-funds)))
        (deposit  #'(lambda (amt) (incf balance amt)))
        (balance  #'(lambda () balance))
        (name     #'(lambda () name))
        (interest #'(lambda ()
                      (incf balance
                            (* interest-rate balance)))))))

;;; ==============================

(defun get-method (object message)
  "Return the method that implements message for this object."
  (funcall object message))

(defun send (object message &rest args)
  "Get the function to implement the message,
  and apply the function to the args."
  (apply (get-method object message) args))

;;; ==============================

(defun withdraw (object &rest args)
  "Define withdraw as a generic function on objects."
  (apply (get-method object 'withdraw) args))

;;; ==============================

(defmacro define-class (class inst-vars class-vars &body methods)
  "Define a class for object-oriented programming."
  ;; Define constructor and generic functions for methods
  `(let ,class-vars
     (mapcar #'ensure-generic-fn ',(mapcar #'first methods))
     (defun ,class ,inst-vars
       #'(lambda (message)
           (case message
             ,@(mapcar #'make-clause methods))))))

(defun make-clause (clause)
  "Translate a message from define-class into a case clause."
  `(,(first clause) #'(lambda ,(second clause) .,(rest2 clause))))

(defun ensure-generic-fn (message)
  "Define an object-oriented dispatch function for a message,
  unless it has already been defined as one."
  (unless (generic-fn-p message)
    (let ((fn #'(lambda (object &rest args)
                  (apply (get-method object message) args))))
      (setf (symbol-function message) fn)
      (setf (get message 'generic-fn) fn))))

(defun generic-fn-p (fn-name)
  "Is this a generic function?"
  (and (fboundp fn-name) 
       (eq (get fn-name 'generic-fn) (symbol-function fn-name))))

;;; ==============================

