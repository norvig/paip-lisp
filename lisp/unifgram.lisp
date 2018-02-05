;;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File unifgram.lisp: The DCG parser from Chapter 20.

(requires "prologcp")

(defmacro rule (head &optional (arrow ':-) &body body)
  "Expand one of several types of logic rules into pure Prolog."
  ;; This is data-driven, dispatching on the arrow
  (funcall (get arrow 'rule-function) head body))

(setf (get ':- 'rule-function)
      #'(lambda (head body) `(<- ,head .,body)))

(defun dcg-normal-goal-p (x) (or (starts-with x :test) (eq x '!)))

(defun dcg-word-list-p (x) (starts-with x ':word))

(setf (get '--> 'rule-function) 'make-dcg)

(defun make-dcg (head body)
  (let ((n (count-if (complement #'dcg-normal-goal-p) body)))
    `(<- (,@head ?s0 ,(symbol '?s n))
         .,(make-dcg-body body 0))))

(defun make-dcg-body (body n)
  "Make the body of a Definite Clause Grammar (DCG) clause.
  Add ?string-in and -out variables to each constituent.
  Goals like (:test goal) are ordinary Prolog goals,
  and goals like (:word hello) are literal words to be parsed."
  (if (null body)
      nil
      (let ((goal (first body)))
        (cond
          ((eq goal '!) (cons '! (make-dcg-body (rest body) n)))
          ((dcg-normal-goal-p goal)
           (append (rest goal)
                   (make-dcg-body (rest body) n)))
          ((dcg-word-list-p goal)
           (cons
             `(= ,(symbol '?s n)
                 (,@(rest goal) .,(symbol '?s (+ n 1))))
             (make-dcg-body (rest body) (+ n 1))))
          (t (cons
               (append goal
                       (list (symbol '?s n)
                             (symbol '?s (+ n 1))))
               (make-dcg-body (rest body) (+ n 1))))))))

(setf (get '==> 'rule-function) 'make-augmented-dcg)

(defun make-augmented-dcg (head body)
  "Build an augmented DCG rule that handles :sem, :ex,
  and automatic conjunctiontive constituents."
  (if (eq (last1 head) :sem)
      ;; Handle :sem 
      (let* ((?sem (gensym "?SEM")))
        (make-augmented-dcg
          `(,@(butlast head) ,?sem)
          `(,@(remove :sem body :key #'first-or-nil)
            (:test ,(collect-sems body ?sem)))))
      ;; Separate out examples from body
      (multiple-value-bind (exs new-body)
          (partition-if #'(lambda (x) (starts-with x :ex)) body)
        ;; Handle conjunctions 
        (let ((rule `(rule ,(handle-conj head) --> ,@new-body)))
          (if (null exs)
              rule
              `(progn (:ex ,head .,(mappend #'rest exs))
                      ,rule))))))

(defun collect-sems (body ?sem)
  "Get the semantics out of each constituent in body,
  and combine them together into ?sem."
  (let ((sems (loop for goal in body
                    unless (or (dcg-normal-goal-p goal)
                               (dcg-word-list-p goal)
                               (starts-with goal :ex)
                               (atom goal))
                    collect (last1 goal))))
    (case (length sems)
      (0 `(= ,?sem t))
      (1 `(= ,?sem ,(first sems)))
      (t `(and* ,sems ,?sem)))))

(defun and*/2 (in out cont)
  "IN is a list of conjuncts that are conjoined into OUT."
  ;; E.g.: (and* (t (and a b) t (and c d) t) ?x) ==>
  ;;        ?x = (and a b c d)
  (if (unify! out (maybe-add 'and (conjuncts (cons 'and in)) t))
      (funcall cont)))

(defun conjuncts (exp)
  "Get all the conjuncts from an expression."
  (deref exp)
  (cond ((eq exp t) nil)
        ((atom exp) (list exp))
        ((eq (deref (first exp)) 'nil) nil)
        ((eq (first exp) 'and)
         (mappend #'conjuncts (rest exp)))
        (t (list exp))))

(defmacro :ex ((category . args) &body examples)
  "Add some example phrases, indexed under the category."
  `(add-examples ',category ',args ',examples))

(defvar *examples* (make-hash-table :test #'eq))

(defun get-examples (category) (gethash category *examples*))

(defun clear-examples () (clrhash *examples*))

(defun add-examples (category args examples)
  "Add these example strings to this category,
  and when it comes time to run them, use the args."
  (dolist (example examples)
    (when (stringp example)
      (let ((ex `(,example
                  (,category ,@args
                   ,(string->list
                      (remove-punctuation example)) ()))))
        (unless (member ex (get-examples category)
                        :test #'equal)
          (setf (gethash category *examples*)
                (nconc (get-examples category) (list ex))))))))

(defun run-examples (&optional category)
  "Run all the example phrases stored under a category.
  With no category, run ALL the examples."
  (prolog-compile-symbols)
  (if (null category)
      (maphash #'(lambda (cat val)
                   (declare (ignore val))
                   (format t "~2&Examples of ~a:~&" cat)
                   (run-examples cat))
               *examples*)
      (dolist (example (get-examples category))
        (format t "~2&EXAMPLE: ~{~a~&~9T~a~}" example)
        (top-level-prove (cdr example)))))

(defun remove-punctuation (string)
  "Replace punctuation with spaces in string."
  (substitute-if #\space #'punctuation-p string))

(defun string->list (string)
  "Convert a string to a list of words."
  (read-from-string (concatenate 'string "(" string ")")))

(defun punctuation-p (char) (find char "*_.,;:`!?#-()\\\""))

(defmacro conj-rule ((conj-cat sem1 combined-sem) ==>
                     conj (cat . args))
  "Define this category as an automatic conjunction."
  (assert (eq ==> '==>))
  `(progn
     (setf (get ',cat 'conj-cat) ',(symbol cat '_))
     (rule (,cat ,@(butlast args) ?combined-sem) ==>
       (,(symbol cat '_) ,@(butlast args) ,sem1)
       (,conj-cat ,sem1 ?combined-sem))
     (rule (,conj-cat ,sem1 ,combined-sem) ==>
       ,conj
       (,cat ,@args))
     (rule (,conj-cat ?sem1 ?sem1) ==>)))

(defun handle-conj (head)
  "Replace (Cat ...) with (Cat_ ...) if Cat is declared
  as a conjunctive category."
  (if (and (listp head) (conj-category (predicate head)))
      (cons (conj-category (predicate head)) (args head))
      head))

(defun conj-category (predicate)
  "If this is a conjunctive predicate, return the Cat_ symbol."
  (get predicate 'conj-category))

