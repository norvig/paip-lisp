;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; syntax2.lisp: The PSG-based natural language parser.
;;;; This version handles semantics as described in Section 19.5.
;;;; Includes *grammar5* and *grammar6*; USE one of these.

(defvar *grammar* "The grammar used by GENERATE.")

(defstruct (rule (:type list)) lhs -> rhs sem)

(defstruct (tree (:type list) (:include rule) (:copier nil)
                 (:constructor new-tree (lhs sem rhs))))

(defstruct (parse) "A parse tree and a remainder." tree rem)

(defun parse-lhs (parse) (tree-lhs (parse-tree parse)))

(defun lexical-rules (word)
  "Return a list of rules with word on the right hand side."
  (or (find-all word *grammar* :key #'rule-rhs :test #'equal)
      (mapcar #'(lambda (cat) `(,cat -> ,word)) *open-categories*)))

(defun rules-starting-with (cat)
  "Return a list of rules where cat starts the rhs."
  (find-all cat *grammar* 
            :key #'(lambda (rule) (first-or-nil (rule-rhs rule)))))

(defun first-or-nil (x)
  "The first element of x if it is a list; else nil."
  (if (consp x) (first x) nil))

(defun complete-parses (parses)
  "Those parses that are complete (have no remainder)."
  (find-all-if #'null parses :key #'parse-rem))

(defun append1 (items item)
  "Add item to end of list of items."
  (append items (list item)))

(memoize 'lexical-rules)
(memoize 'rules-starting-with)
(memoize 'parse :test #'eq)

(defun parser (words)
  "Return all complete parses of a list of words."
  (clear-memoize 'parse) ;***
  (mapcar #'parse-tree (complete-parses (parse words))))

(defun use (grammar)
  "Switch to a new grammar."
  (clear-memoize 'rules-starting-with)
  (clear-memoize 'lexical-rules)
  (length (setf *grammar* grammar)))

(defparameter *open-categories* '(N V A Name)
  "Categories to consider for unknown words")

(defun parse (words)
  "Bottom-up parse, returning all parses of any prefix of words.
  This version has semantics."
  (unless (null words)
    (mapcan #'(lambda (rule)
                (extend-parse (rule-lhs rule) (rule-sem rule) ;***
                              (list (first words)) (rest words) nil))
            (lexical-rules (first words)))))

(defun extend-parse (lhs sem rhs rem needed) ;***
  "Look for the categories needed to complete the parse.
  This version has semantics."
  (if (null needed)
      ;; If nothing is needed, return this parse and upward extensions,
      ;; unless the semantics fails
      (let ((parse (make-parse :tree (new-tree lhs sem rhs) :rem rem)))
        (unless (null (apply-semantics (parse-tree parse))) ;***
          (cons parse
                (mapcan
                  #'(lambda (rule)
                      (extend-parse (rule-lhs rule) (rule-sem rule) ;***
                                    (list (parse-tree parse)) rem
                                    (rest (rule-rhs rule))))
                  (rules-starting-with lhs)))))
      ;; otherwise try to extend rightward
      (mapcan
        #'(lambda (p)
            (if (eq (parse-lhs p) (first needed))
                (extend-parse lhs sem (append1 rhs (parse-tree p)) ;***
                              (parse-rem p) (rest needed))))
        (parse rem))))

(defun apply-semantics (tree)
  "For terminal nodes, just fetch the semantics.
  Otherwise, apply the sem function to its constituents."
  (if (terminal-tree-p tree)
      (tree-sem tree)
      (setf (tree-sem tree)
            (apply (tree-sem tree)
                   (mapcar #'tree-sem (tree-rhs tree))))))

(defun terminal-tree-p (tree)
  "Does this tree have a single word on the rhs?"
  (and (length=1 (tree-rhs tree))
       (atom (first (tree-rhs tree)))))

(defun meanings (words)
  "Return all possible meanings of a phrase.  Throw away the syntactic part."
  (remove-duplicates (mapcar #'tree-sem (parser words)) :test #'equal))


;;;; Grammars

(defparameter *grammar5*
  '((NP -> (NP CONJ NP) infix-funcall)
    (NP -> (N)          list)
    (NP -> (N P N)      infix-funcall)
    (N ->  (DIGIT)      identity)
    (P ->  to           integers)
    (CONJ -> and        ordered-union)
    (CONJ -> without    ordered-set-difference)
    (N -> 1 1) (N -> 2 2) (N -> 3 3) (N -> 4 4) (N -> 5 5)
    (N -> 6 6) (N -> 7 7) (N -> 8 8) (N -> 9 9) (N -> 0 0)))

(defun infix-funcall (arg1 function arg2)
  "Apply the function to the two arguments"
  (funcall function arg1 arg2))

(defun integers (start end)
  "A list of all the integers in the range [start...end] inclusive."
  (if (> start end) nil
      (cons start (integers (+ start 1) end))))

(defun ordered-union (a b)
  "Add elements of B to A, but preserve order of A (and B)."
  ;; Added by norvig Jun 11 96; some Lisps don't preserve order
 (append a (ordered-set-difference b a)))

(defun ordered-set-difference (a b)
  "Subtact elements of B from A, but preserve order of A."
  ;; Added by norvig Jun 11 96; some Lisps don't preserve order
  (remove-if #'(lambda (x) (member x b)) a))


(defparameter *grammar6*
  '((NP -> (NP CONJ NP) infix-funcall)
    (NP -> (N)          list)
    (NP -> (N P N)      infix-funcall)
    (N ->  (DIGIT)      identity)
    (N ->  (N DIGIT)    10*N+D)
    (P ->  to           integers)
    (CONJ -> and        union*)
    (CONJ -> without    set-diff)
    (DIGIT -> 1 1) (DIGIT -> 2 2) (DIGIT -> 3 3)
    (DIGIT -> 4 4) (DIGIT -> 5 5) (DIGIT -> 6 6)
    (DIGIT -> 7 7) (DIGIT -> 8 8) (DIGIT -> 9 9)
    (DIGIT -> 0 0)))

(defun union* (x y) (if (null (intersection x y)) (append x y)))
(defun set-diff (x y) (if (subsetp y x) (ordered-set-difference x y)))
(defun 10*N+D (N D) (+ (* 10 N) D))


