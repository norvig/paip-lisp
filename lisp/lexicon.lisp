;;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File lexicon.lisp:  Macros and functions to support the entry of
;;;; words into the lexicon.

(defvar *abbrevs* (make-hash-table))

(defmacro abbrev (symbol definition)
  "Make symbol be an abbreviation for definition."
  `(setf (gethash ',symbol *abbrevs*) ',definition))

(defun clear-abbrevs () (clrhash *abbrevs*))
(defun get-abbrev (symbol) (gethash symbol *abbrevs*))

;;; ==============================

(defvar *words* (make-hash-table :size 500))

(defmacro word (word cat &rest info)
  "Put word, with category and subcat info, into lexicon."
  `(add-word ',word ',cat .,(mapcar #'kwote info)))

(defun add-word (word cat &rest info)
  "Put word, with category and other info, into lexicon."
  (push (cons cat (mapcar #'expand-abbrevs-and-variables info))
        (gethash word *words*))
  word)

(defun kwote (x) (list 'quote x))

;;; ==============================

(defun expand-abbrevs-and-variables (exp)
  "Replace all variables in exp with vars, and expand abbrevs."
  (let ((bindings nil))
    (labels
      ((expand (exp)
         (cond
           ((lookup exp bindings))
           ((eq exp '?) (?))
           ((variable-p exp)
            (let ((var (?)))
              (push (cons exp var) bindings)
              var))
           ((consp exp)
            (reuse-cons (expand (first exp))
                        (expand (rest exp))
                        exp))
           (t (multiple-value-bind (expansion found?)
                  (get-abbrev exp)
                (if found?
                    (expand-abbrevs-and-variables expansion)
                    exp))))))
      (expand exp))))

;;; ==============================

(defun word/n (word cat cont &rest info)
  "Retrieve a word from the lexicon."
  (unless (unbound-var-p (deref word))
    (let ((old-trail (fill-pointer *trail*)))
      (dolist (old-entry (gethash word *words*))
        (let ((entry (deref-copy old-entry)))
          (when (and (consp entry)
                     (unify! cat (first entry))
                     (unify! info (rest entry)))
            (funcall cont)))
        (undo-bindings! old-trail)))))

;;; ==============================

(defun word/2 (w cat cont) (word/n w cat cont))
(defun word/3 (w cat a cont) (word/n w cat cont a))
(defun word/4 (w cat a b cont) (word/n w cat cont a b))
(defun word/5 (w cat a b c cont) (word/n w cat cont a b c))
(defun word/6 (w cat a b c d cont) (word/n w cat cont a b c d))

;;; ==============================

(defmacro noun (base &rest args)
  "Add a noun and its plural to the lexicon."
  `(add-noun-form ',base ,@(mapcar #'kwote args)))

(defun add-noun-form (base &optional (plural (symbol base 's))
                      (sem base) &rest slots)
  (if (eq plural '*)
      (add-word base 'noun '? slots sem)
      (progn
        (add-word base 'noun '3sing slots sem)
        (add-word plural 'noun '3plur slots sem))))

(defmacro verb ((base &rest forms) &body senses)
  "Enter a verb into the lexicon."
  `(add-verb ',senses ',base ,@(mapcar #'kwote (mklist forms))))

(defun add-verb (senses base &optional
                 (past (symbol (strip-vowel base) 'ed))
                 (past-part past)
                 (pres-part (symbol (strip-vowel base) 'ing))
                 (plural (symbol base 's)))
  "Enter a verb into the lexicon."
  (add-word base 'verb 'nonfinite senses)
  (add-word base 'verb '(finite ~3sing present) senses)
  (add-word past 'verb '(finite ? past) senses)
  (add-word past-part 'verb '-en senses)
  (add-word pres-part 'verb '-ing senses)
  (add-word plural 'verb '(finite 3sing present) senses)
  (add-word past-part 'verb 'passive
            (mapcar #'passivize-sense
                    (expand-abbrevs-and-variables senses))))

;;; ==============================

(defun strip-vowel (word)
  "Strip off a trailing vowel from a string."
  (let* ((str (string word))
         (end (- (length str) 1)))
    (if (vowel-p (char str end))
        (subseq str 0 end)
        str)))

(defun vowel-p (char) (find char "aeiou" :test #'char-equal))

;;; ==============================

(defun passivize-sense (sense)
  ;; The first element of sense is the semantics; rest are slots
  (cons (first sense) (mapcan #'passivize-subcat (rest sense))))

(defun passivize-subcat (slots)
  "Return a list of passivizations of this subcat frame."
  ;; Whenever the 1 slot is of the form (?any 1 (NP ?)),
  ;; demote the 1 to a (3), and promote any 2 to a 1.
  (when (and (eql (slot-number (first slots)) 1)
             (starts-with (third (first slots)) 'NP))
    (let ((old-1 `(,(first (first slots)) (3) (PP by ?))))
      (loop for slot in slots
            when (eql (slot-number slot) 2)
            collect `((,(first slot) 1 ,(third slot))
                      ,@(remove slot (rest slots))
                      ,old-1)))))

(defun slot-number (slot) (first-or-self (second slot)))

;;; ==============================

(defun copula (senses entries)
  "Copula entries are both aux and main verb."
  ;; They also are used in passive verb phrases and aux-inv-S
  (dolist (entry entries)
    (add-word (first entry) 'aux (second entry) (third entry))
    (add-word (first entry) 'verb (second entry) senses)
    (add-word (first entry) 'aux (second entry) 'passive)
    (add-word (first entry) 'be)))

;;; ==============================

(defun clear-lexicon ()
  (clrhash *words*)
  (clear-abbrevs))

(defun clear-grammar ()
  (clear-examples)
  (clear-db))

;;; ==============================

(defmacro try (&optional cat &rest words)
  "Tries to parse WORDS as a constituent of category CAT.
  With no words, runs all the :ex examples for category.
  With no cat, runs all the examples."
  `(try-dcg ',cat ',words))

(defun try-dcg (&optional cat words)
  "Tries to parse WORDS as a constituent of category CAT.
  With no words, runs all the :ex examples for category.
  With no cat, runs all the examples."
  (if (null words)
      (run-examples cat)
      (let ((args `((gap nil) (gap nil) ?sem ,words ())))
        (mapc #'test-unknown-word words)
        (top-level-prove
          (ecase cat
            (np `((np ? ? ?wh ?x ,@args)))
            (vp `((vp ?infl ?x ?sl ?v ,@args)))
            (pp `((pp ?prep ?role ?wh ?x ,@args)))
            (xp `((xp ?slot ?constituent ?wh ?x ,@args)))
            (s  `((s ? ?sem ,words ())))
            (rel-clause `((rel-clause ? ?x ?sem ,words ())))
            (clause `((clause ?infl ?x ?int-subj ?v ?g1 ?g2
                              ?sem ,words ()))))))))

(defun test-unknown-word (word)
  "Print a warning message if this is an unknown word."
  (unless (or (gethash word *words*) (numberp word))
    (warn "~&Unknown word: ~a" word)))

;;; ==============================

