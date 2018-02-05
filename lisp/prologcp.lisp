;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File prologcp.lisp:  Primitives for the prolog compiler
;;;; needed to actually run some functions.

;;; Bug fix by Adam Farquhar, farquhar@cs.utexas.edu.
;;; Trivia: Farquhar is Norvig's cousin.

(requires "prologc")

(defun read/1 (exp cont)
  (if (unify! exp (read))
      (funcall cont)))

(defun write/1 (exp cont)
  (write (deref-exp exp) :pretty t)
  (funcall cont))

(defun nl/0 (cont) (terpri) (funcall cont))

(defun =/2 (?arg1 ?arg2 cont)
  (if (unify! ?arg1 ?arg2)
      (funcall cont)))

(defun ==/2 (?arg1 ?arg2 cont)
  "Are the two arguments EQUAL with no unification,
  but with dereferencing?  If so, succeed."
  (if (deref-equal ?arg1 ?arg2)
      (funcall cont)))

(defun deref-equal (x y)
  "Are the two arguments EQUAL with no unification,
  but with dereferencing?"
  (or (eql (deref x) (deref y))
      (and (consp x)
           (consp y)
           (deref-equal (first x) (first y))
           (deref-equal (rest x) (rest y)))))

(defun call/1 (goal cont)
  "Try to prove goal by calling it."
  (deref goal)
  (apply (make-predicate (first goal) 
                         (length (args goal)))
         (append (args goal) (list cont))))

(<- (or ?a ?b) (call ?a))
(<- (or ?a ?b) (call ?b))

(<- (and ?a ?b) (call ?a) (call ?b))

(defmacro with-undo-bindings (&body body)
  "Undo bindings after each expression in body except the last."
  (if (length=1 body)
      (first body)
      `(let ((old-trail (fill-pointer *trail*)))
         ,(first body)
         ,@(loop for exp in (rest body)
                 collect '(undo-bindings! old-trail)
                 collect exp))))

(defun not/1 (relation cont)
  "Negation by failure: If you can't prove G, then (not G) true."
  ;; Either way, undo the bindings.
  (with-undo-bindings
    (call/1 relation #'(lambda () (return-from not/1 nil)))
    (funcall cont)))

(defun bagof/3 (exp goal result cont)
  "Find all solutions to GOAL, and for each solution,
  collect the value of EXP into the list RESULT."
  ;; Ex: Assume (p 1) (p 2) (p 3).  Then:
  ;;     (bagof ?x (p ?x) ?l) ==> ?l = (1 2 3)
  (let ((answers nil))
    (call/1 goal #'(lambda ()
		     ;; Bug fix by mdf0%shemesh@gte.com (Mark Feblowitz)
		     ;; on 25 Jan 1996; was deref-COPY
                     (push (deref-EXP exp) answers))) 
    (if (and (not (null answers))
             (unify! result (nreverse answers)))
        (funcall cont))))

(defun deref-copy (exp)
  "Copy the expression, replacing variables with new ones.
  The part without variables can be returned as is."
  ;; Bug fix by farquhar and norvig, 12/12/92.  Forgot to deref var.
  (sublis (mapcar #'(lambda (var) (cons (deref var) (?)))
                  (unique-find-anywhere-if #'var-p exp))
          exp))

(defun setof/3 (exp goal result cont)
  "Find all unique solutions to GOAL, and for each solution,
  collect the value of EXP into the list RESULT."
  ;; Ex: Assume (p 1) (p 2) (p 3).  Then:
  ;;     (setof ?x (p ?x) ?l) ==> ?l = (1 2 3)
  (let ((answers nil))
    (call/1 goal #'(lambda ()
                     (push (deref-copy exp) answers)))
    (if (and (not (null answers))
             (unify! result (delete-duplicates
                              answers
                              :test #'deref-equal)))
        (funcall cont))))

(defun is/2 (var exp cont)
  ;; Example: (is ?x (+ 3 (* ?y (+ ?z 4))))
  ;; Or even: (is (?x ?y ?x) (cons (first ?z) ?l))
  (if (and (not (find-if-anywhere #'unbound-var-p exp))
           (unify! var (eval (deref-exp exp))))
      (funcall cont)))

(defun unbound-var-p (exp)
  "Is EXP an unbound var?"
  (and (var-p exp) (not (bound-p exp))))

(defun var/1 (?arg1 cont)
  "Succeeds if ?arg1 is an uninstantiated variable."
  (if (unbound-var-p ?arg1)
      (funcall cont)))

(defun lisp/2 (?result exp cont)
  "Apply (first exp) to (rest exp), and return the result."
  (if (and (consp (deref exp))
           (unify! ?result (apply (first exp) (rest exp))))
      (funcall cont)))

(defun repeat/0 (cont) 
  (loop (funcall cont)))

(<- (if ?test ?then) (if ?then ?else (fail)))

(<- (if ?test ?then ?else)
    (call ?test)
    !
    (call ?then))

(<- (if ?test ?then ?else)
    (call ?else))

(<- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))

(<- (length () 0))
(<- (length (?x . ?y) (1+ ?n)) (length ?y ?n))

(defun numberp/1 (x cont)
  (when (numberp (deref x))
    (funcall cont)))

(defun atom/1 (x cont)
  (when (atom (deref x))
    (funcall cont)))

