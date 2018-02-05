;;; -*- Mode: Lisp; Syntax: Common-Lisp;  -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;; krep1.lisp: Knowledge representation code; second version.
;;; Fixes problem with renaming variables; adds conjunctions.

(requires "krep1") ; Need some functions from previous version

(defun index (key)
  "Store key in a dtree node.  Key must be (predicate . args);
  it is stored in the predicate's dtree."
  (dtree-index key (rename-variables key)    ; store unique vars
               (get-dtree (predicate key))))

;;; ==============================

;;; The following iterated-deepening code is not used, but is
;;; included for those who want to incorporate it into prolog.

(defvar *search-cut-off* nil "Has the search been stopped?")

(defun prove-all (goals bindings depth)
  "Find a solution to the conjunction of goals."
  ;; This version just passes the depth on to PROVE.
  (cond ((eq bindings fail) fail)
        ((null goals) bindings)
        (t (prove (first goals) bindings (rest goals) depth))))

(defun prove (goal bindings other-goals depth)
  "Return a list of possible solutions to goal."
  ;; Check if the depth bound has been exceeded
  (if (= depth 0)                            ;***
      (progn (setf *search-cut-off* t)       ;***
             fail)                           ;***
      (let ((clauses (get-clauses (predicate goal))))
        (if (listp clauses)
            (some
              #'(lambda (clause)
                  (let ((new-clause (rename-variables clause)))
                    (prove-all
                      (append (clause-body new-clause) other-goals)
                      (unify goal (clause-head new-clause) bindings)
                      (- depth 1))))          ;***
              clauses)
            ;; The predicate's "clauses" can be an atom:
            ;; a primitive function to call
            (funcall clauses (rest goal) bindings
                     other-goals depth)))))   ;***

;;; ==============================

(defparameter *depth-start* 5
  "The depth of the first round of iterative search.")
(defparameter *depth-incr* 5 
  "Increase each iteration of the search by this amount.")
(defparameter *depth-max* most-positive-fixnum
  "The deepest we will ever search.")

;;; ==============================

(defun top-level-prove (goals)
  (let ((all-goals
          `(,@goals (show-prolog-vars ,@(variables-in goals)))))
    (loop for depth from *depth-start* to *depth-max* by *depth-incr*
          while (let ((*search-cut-off* nil))
                  (prove-all all-goals no-bindings depth)
                  *search-cut-off*)))
  (format t "~&No.")
  (values))

;;; ==============================

(defun show-prolog-vars (vars bindings other-goals depth)
  "Print each variable with its binding.
  Then ask the user if more solutions are desired."
  (if (> depth *depth-incr*)
      fail
      (progn
        (if (null vars)
            (format t "~&Yes")
            (dolist (var vars)
              (format t "~&~a = ~a" var
                      (subst-bindings bindings var))))
        (if (continue-p)
            fail
            (prove-all other-goals bindings depth)))))

;;; ==============================

;;;; Adding support for conjunctions:

(defun add-fact (fact)
  "Add the fact to the data base."
  (if (eq (predicate fact) 'and)
      (mapc #'add-fact (args fact))
      (index fact)))

;;; ==============================

(defun retrieve-fact (query &optional (bindings no-bindings))
  "Find all facts that match query.  Return a list of bindings."
  (if (eq (predicate query) 'and)
      (retrieve-conjunction (args query) (list bindings))
      (retrieve query bindings)))

(defun retrieve-conjunction (conjuncts bindings-lists)
  "Return a list of binding lists satisfying the conjuncts."
  (mapcan
    #'(lambda (bindings)
        (cond ((eq bindings fail) nil)
              ((null conjuncts) (list bindings))
              (t (retrieve-conjunction
                   (rest conjuncts)
                   (retrieve-fact
                     (subst-bindings bindings (first conjuncts))
                     bindings)))))
    bindings-lists))

;;; ==============================

(defun mapc-retrieve (fn query &optional (bindings no-bindings))
  "For every fact that matches the query,
  apply the function to the binding list."
  (dolist (bucket (fetch query))
    (dolist (answer bucket)
      (let ((new-bindings (unify query answer bindings)))
        (unless (eq new-bindings fail)
          (funcall fn new-bindings))))))

(defun retrieve (query &optional (bindings no-bindings))
  "Find all facts that match query.  Return a list of bindings."
  (let ((answers nil))
    (mapc-retrieve #'(lambda (bindings) (push bindings answers))
                   query bindings)
    answers))


;;; ==============================

(defun retrieve-bagof (query)
  "Find all facts that match query.
  Return a list of queries with bindings filled in."
  (mapcar #'(lambda (bindings) (subst-bindings bindings query))
          (retrieve-fact query)))

(defun retrieve-setof (query)
  "Find all facts that match query.
  Return a list of unique queries with bindings filled in."
  (remove-duplicates (retrieve-bagof query) :test #'equal))

;;; ==============================

;;;; Get ready for attached functions in the next version:

(defmacro def-attached-fn (pred args &body body)
  "Define the attached function for a primitive."
  `(setf (get ',pred 'attached-fn)
         #'(lambda ,args .,body)))
