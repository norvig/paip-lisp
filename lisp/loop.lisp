;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User; -*-

#|| Examples of Loop expansion:

(loop for i from 1 to n do (print (sqrt i))) ==
(LET* ((I 1)
       (TEMP N))
  (TAGBODY
   LOOP
      (IF (> I TEMP)
          (GO END))
      (PRINT (SQRT I))
      (SETF I (+ I 1))
      (GO LOOP)
   END))


(loop for v in list do (print v)) ==
(LET* ((IN LIST)
       (V (CAR IN)))
  (TAGBODY
   LOOP
      (IF (NULL IN)
          (GO END))
      (PRINT V)
      (SETF IN (CDR IN))
      (SETF V (CAR IN))
      (GO LOOP)
   END))

;;; ==============================

(let* (variables...)
  (tagbody
   loop
      (if exit-tests
          (go end))
      body
      (go loop)
   end))

;;; ==============================

(let* (variables...)
  (block name
    prologue
    (tagbody
     loop
        body
        (go loop)
     end
        epilogue
        (return result))))

||#

;;; ==============================

(defstruct loop
  "A structure to hold parts of a loop as it is built."
  (vars nil) (prologue nil) (body nil) (steps nil)
  (epilogue nil) (result nil) (name nil))

;;; ==============================

(defmacro loop (&rest exps)
  "Supports both ANSI and simple LOOP.
  Warning: Not every loop keyword is supported."
  (if (every #'listp exps)
      ;; No keywords implies simple loop:
      `(block nil (tagbody loop ,@exps (go loop)))
      ;; otherwise process loop keywords:
      (let ((l (make-loop)))    
        (parse-loop-body l exps)
        (fill-loop-template l))))

(defun fill-loop-template (l)
  "Use a loop-structure instance to fill the template."
  `(let* ,(nreverse (loop-vars l))
     (block ,(loop-name l)
       ,@(nreverse (loop-prologue l))
       (tagbody
        loop
           ,@(nreverse (loop-body l))
           ,@(nreverse (loop-steps l))
           (go loop)
        end
           ,@(nreverse (loop-epilogue l))
           (return ,(loop-result l))))))

;;; ==============================

(defun add-body (l exp) (push exp (loop-body l)))

(defun add-test (l test)
  "Put in a test for loop termination."
  (push `(if ,test (go end)) (loop-body l)))

(defun add-var (l var init &optional (update nil update?))
  "Add a variable, maybe including an update step."
  (unless (assoc var (loop-vars l))
    (push (list var init) (loop-vars l)))
  (when update? 
    (push `(setq ,var ,update) (loop-steps l))))

;;; ==============================

(defun parse-loop-body (l exps)
  "Parse the exps based on the first exp being a keyword.
  Continue until all the exps are parsed."
  (unless (null exps)
    (parse-loop-body 
      l (call-loop-fn l (first exps) (rest exps)))))

(defun call-loop-fn (l key exps)
  "Return the loop parsing function for this keyword"
  (if (and (symbolp key) (get key 'loop-fn))
      (funcall (get key 'loop-fn) l (first exps) (rest exps))
      (error "Unknown loop key: ~a" key)))

(defmacro defloop (key args &rest body)
  "Define a new LOOP keyword."
  ;; If the args do not have a third arg, one is supplied.
  ;; Also, we can define an alias with (defloop key other-key)
  `(setf (get ',key 'loop-fn)
         ,(cond ((and (symbolp args) (null body))
                 `#'(lambda (l x y)
                      (call-loop-fn l ',args (cons x y))))
                ((and (listp args) (= (length args) 2))
                 `#'(lambda (,@args -exps-) ,@body -exps-))
                (t `#'(lambda ,args ,@body)))))

;;; ==============================

(defloop repeat (l times)
  "(LOOP REPEAT n ...) does loop body n times" 
  (let ((i (gensym "REPEAT")))
    (add-var l i times `(- ,i 1))
    (add-test l `(<= ,i 0))))

;;; ==============================

(defloop as for)  ;; AS is the same as FOR

(defloop for (l var exps)
  "4 of the 7 cases for FOR are covered here:
  (LOOP FOR i FROM s TO e BY inc ...) does arithemtic iteration
  (LOOP FOR v IN l ...) iterates for each element of l
  (LOOP FOR v ON l ...) iterates for each tail of l
  (LOOP FOR v = expr [THEN step]) initializes and iterates v"
  (let ((key (first exps))
        (source (second exps))
        (rest (rest2 exps)))
    (ecase key
      ((from downfrom upfrom to downto upto by)
       (loop-for-arithmetic l var exps))
      (in (let ((v (gensym "IN")))
            (add-var l v source `(cdr ,v))
            (add-var l var `(car ,v) `(car ,v))
            (add-test l `(null ,v))
            rest))
      (on (add-var l var source `(cdr ,var))
          (add-test l `(null ,var))
          rest)
      (= (if (eq (first rest) 'then)
             (progn
               (pop rest)
               (add-var l var source (pop rest)))
             (progn
               (add-var l var nil)
               (add-body l `(setq ,var ,source))))
         rest)
      ;; ACROSS, BEING clauses omitted
      )))

(defun loop-for-arithmetic (l var exps)
  "Parse loop expressions of the form:
  (LOOP FOR var [FROM|DOWNFROM|UPFROM exp1] [TO|DOWNTO|UPTO exp2]
        [BY exp3]"
  ;; The prepositions BELOW and ABOVE are omitted
  (let ((exp1 0)
        (exp2 nil)
        (exp3 1)
        (down? nil))
    ;; Parse the keywords:
    (when (member (first exps) '(from downfrom upfrom))
      (setf exp1 (second exps)
            down? (eq (first exps) 'downfrom)
            exps (rest2 exps)))
    (when (member (first exps) '(to downto upto))
      (setf exp2 (second exps)
            down? (or down? (eq (first exps) 'downto))
            exps (rest2 exps)))
    (when (eq (first exps) 'by)
      (setf exp3 (second exps)
            exps (rest2 exps)))
    ;; Add variables and tests:
    (add-var l var exp1
             `(,(if down? '- '+) ,var ,(maybe-temp l exp3)))
    (when exp2
      (add-test l `(,(if down? '< '>) ,var ,(maybe-temp l exp2))))
    ;; and return the remaining expressions:
    exps))

(defun maybe-temp (l exp)
  "Generate a temporary variable, if needed."
  (if (constantp exp)
      exp
      (let ((temp (gensym "TEMP")))
        (add-var l temp exp)
        temp)))

;;; ==============================

(defloop until (l test) (add-test l test))

(defloop while (l test) (add-test l `(not ,test)))

(defloop always (l test)
  (setf (loop-result l) t)
  (add-body l `(if (not ,test) (return nil))))

(defloop never (l test)
  (setf (loop-result l) t)
  (add-body l `(if ,test (return nil))))

(defloop thereis (l test) (add-body l `(return-if ,test)))

(defmacro return-if (test)
  "Return TEST if it is non-nil"
  (let ((var (gensym)))
    `(let ((,var ,test))
      (if ,var (return ,var)))))

(defmacro loop-finish () `(go end))

;;; ==============================

(defconstant *acc* (gensym "ACC")
  "Variable used for value accumulation in LOOP.")

;;; INTO preposition is omitted

(defloop collect (l exp)
  (add-var l *acc* '(make-queue))
  (add-body l `(enqueue ,exp ,*acc*))
  (setf (loop-result l) `(queue-contents ,*acc*)))

(defloop nconc (l exp)
  (add-var l *acc* '(make-queue))
  (add-body l `(queue-nconc ,*acc* ,exp))
  (setf (loop-result l) `(queue-contents ,*acc*)))

(defloop append (l exp exps)
  (call-loop-fn l 'nconc `((copy-list ,exp) .,exps)))

(defloop count (l exp)
  (add-var l *acc* 0)
  (add-body l `(when ,exp (incf ,*acc*)))
  (setf (loop-result l) *acc*))

(defloop sum (l exp)
  (add-var l *acc* 0)
  (add-body l `(incf ,*acc* ,exp))
  (setf (loop-result l) *acc*))

(defloop maximize (l exp)
  (add-var l *acc* nil)
  (add-body l `(setf ,*acc*
                     (if ,*acc*
                         (max ,*acc* ,exp)
                         ,exp)))
  (setf (loop-result l) *acc*))

(defloop minimize (l exp)
  (add-var l *acc* nil)
  (add-body l `(setf ,*acc*
                     (if ,*acc*
                         (min ,*acc* ,exp)
                         ,exp)))
  (setf (loop-result l) *acc*))

(defloop collecting collect)
(defloop nconcing   nconc)
(defloop appending  append)
(defloop counting   count)
(defloop summing    sum)
(defloop maximizing maximize)
(defloop minimizing minimize)

;;; ==============================

;;;; 26.9. Variable Initializations ("and" omitted)

(defloop with (l var exps)
  (let ((init nil))
    (when (eq (first exps) '=)
      (setf init (second exps)
            exps (rest2 exps)))
    (add-var l var init)
    exps))

;;; ==============================

(defloop when (l test exps) 
  (loop-unless l `(not ,(maybe-set-it test exps)) exps))

(defloop unless (l test exps)
  (loop-unless l (maybe-set-it test exps) exps))

(defun maybe-set-it (test exps)
  "Return value, but if the variable IT appears in exps,
  then return code that sets IT to value."
  (if (find-anywhere 'it exps)
      `(setq it ,test)
      test))

(defloop if when)

(defun loop-unless (l test exps)
  (let ((label (gensym "L")))
    (add-var l 'it nil)
    ;; Emit code for the test and the THEN part
    (add-body l `(if ,test (go ,label)))
    (setf exps (call-loop-fn l (first exps) (rest exps)))
    ;; Optionally emit code for the ELSE part
    (if (eq (first exps) 'else)
        (progn
          (let ((label2 (gensym "L")))
            (add-body l `(go ,label2))
            (add-body l label)
            (setf exps (call-loop-fn l (second exps) (rest2 exps)))
            (add-body l label2)))
        (add-body l label)))
    exps)

;;; ==============================

(defloop do (l exp exps)
  (add-body l exp)
  (loop (if (symbolp (first exps)) (RETURN exps))
        (add-body l (pop exps))))

(defloop return (l exp) (add-body l `(return ,exp)))

;;; ==============================

(defloop initially (l exp exps)
  (push exp (loop-prologue l))
  (loop (if (symbolp (first exps)) (RETURN exps))
        (push (pop exps) (loop-prologue l))))

(defloop finally (l exp exps)
  (push exp (loop-epilogue l))
  (loop (if (symbolp (first exps)) (RETURN exps))
        (push (pop exps) (loop-epilogue l))))

(defloop named (l exp) (setf (loop-name l) exp))

