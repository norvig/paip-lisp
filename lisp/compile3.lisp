;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File compile3.lisp: Scheme compiler with assembler
;;;; and peephole optimizer.  Also the abstract machine simulator.
;;;; After loading this file, load the optimizers in compopt.lisp.

;;; Bug fixes by Erann Gat, gat@aig.Jpl.Nasa.Gov, November 1992

(requires "interp1" "compile1" "compile2")

;;; ==============================

(defun opcode (instr) (if (label-p instr) :label (first instr)))
(defun args (instr) (if (listp instr) (rest instr)))
(defun arg1 (instr) (if (listp instr) (second instr)))
(defun arg2 (instr) (if (listp instr) (third instr)))
(defun arg3 (instr) (if (listp instr) (fourth instr)))

(defsetf arg1 (instr) (val) `(setf (second ,instr) ,val))

;;; ==============================

(defun assemble (fn)
  "Turn a list of instructions into a vector."
  (multiple-value-bind (length labels)
      (asm-first-pass (fn-code fn))
    (setf (fn-code fn)
          (asm-second-pass (fn-code fn)
                           length labels))
    fn))

(defun asm-first-pass (code)
  "Return the labels and the total code length."
  (let ((length 0)
        (labels nil))
    (dolist (instr code)
      (if (label-p instr)
          (push (cons instr length) labels)
          (incf length)))
    (values length labels)))

(defun asm-second-pass (code length labels)
  "Put code into code-vector, adjusting for labels."
  (let ((addr 0)
        (code-vector (make-array length)))
    (dolist (instr code)
      (unless (label-p instr)
        (if (is instr '(JUMP TJUMP FJUMP SAVE))
            (setf (arg1 instr)
                  (cdr (assoc (arg1 instr) labels))))
        (setf (aref code-vector addr) instr)
        (incf addr)))
    code-vector))

;;; ==============================

(defun show-fn (fn &optional (stream *standard-output*) (indent 2))
  "Print all the instructions in a function.
  If the argument is not a function, just princ it, 
  but in a column at least 8 spaces wide."
  ;; This version handles code that has been assembled into a vector
  (if (not (fn-p fn))
      (format stream "~8a" fn)
      (progn
        (fresh-line)
        (dotimes (i (length (fn-code fn)))
          (let ((instr (elt (fn-code fn) i)))
            (if (label-p instr)
                (format stream "~a:" instr)
                (progn
                  (format stream "~VT~2d: " indent i)
                  (dolist (arg instr)
                    (show-fn arg stream (+ indent 8)))
                  (fresh-line))))))))

;;; ==============================

(defstruct ret-addr fn pc env)

(defun is (instr op)
  "True if instr's opcode is OP, or one of OP when OP is a list."
  (if (listp op) 
      (member (opcode instr) op)
      (eq (opcode instr) op)))

(defun top (stack) (first stack))

(defun machine (f)
  "Run the abstract machine on the code for f."
  (let* ((code (fn-code f))
         (pc 0)
         (env nil)
         (stack nil)
         (n-args 0)
         (instr nil))
    (loop
       (setf instr (elt code pc))
       (incf pc)
       (case (opcode instr)
         
         ;; Variable/stack manipulation instructions:
         (LVAR   (push (elt (elt env (arg1 instr)) (arg2 instr))
                       stack))
         (LSET   (setf (elt (elt env (arg1 instr)) (arg2 instr))
                       (top stack)))
         (GVAR   (push (get (arg1 instr) 'global-val) stack))
         (GSET   (setf (get (arg1 instr) 'global-val) (top stack)))
         (POP    (pop stack))
         (CONST  (push (arg1 instr) stack))
         
         ;; Branching instructions:
         (JUMP   (setf pc (arg1 instr)))
         (FJUMP  (if (null (pop stack)) (setf pc (arg1 instr))))
         (TJUMP  (if (pop stack) (setf pc (arg1 instr))))
         
         ;; Function call/return instructions:
         (SAVE   (push (make-ret-addr :pc (arg1 instr)
                                      :fn f :env env)
                       stack))
         (RETURN ;; return value is top of stack; ret-addr is second
          (setf f (ret-addr-fn (second stack))
                code (fn-code f)
                env (ret-addr-env (second stack))
                pc (ret-addr-pc (second stack)))
          ;; Get rid of the ret-addr, but keep the value
          (setf stack (cons (first stack) (rest2 stack))))
         (CALLJ  (pop env)                 ; discard the top frame
                 (setf f  (pop stack)
                       code (fn-code f)
                       env (fn-env f)
                       pc 0
                       n-args (arg1 instr)))
         (ARGS   (assert (= n-args (arg1 instr)) ()
                         "Wrong number of arguments:~
                         ~d expected, ~d supplied"
                         (arg1 instr) n-args)
                 (push (make-array (arg1 instr)) env)
                 (loop for i from (- n-args 1) downto 0 do
                       (setf (elt (first env) i) (pop stack))))
         (ARGS.  (assert (>= n-args (arg1 instr)) ()
                         "Wrong number of arguments:~
                         ~d or more expected, ~d supplied"
                         (arg1 instr) n-args)
                 (push (make-array (+ 1 (arg1 instr))) env)
                 (loop repeat (- n-args (arg1 instr)) do
                       (push (pop stack) (elt (first env) (arg1 instr))))
                 (loop for i from (- (arg1 instr) 1) downto 0 do
                       (setf (elt (first env) i) (pop stack))))
         (FN     (push (make-fn :code (fn-code (arg1 instr))
                                :env env) stack))
         (PRIM   (push (apply (arg1 instr)
                              (loop with args = nil repeat n-args
                                    do (push (pop stack) args)
                                    finally (return args)))
                       stack))
         
         ;; Continuation instructions:
         (SET-CC (setf stack (top stack)))
         (CC     (push (make-fn
                         :env (list (vector stack))
                         :code '((ARGS 1) (LVAR 1 0 ";" stack) (SET-CC)
                                 (LVAR 0 0) (RETURN)))
                       stack))
         
         ;; Nullary operations:
         ((SCHEME-READ NEWLINE) ; *** fix, gat, 11/9/92
          (push (funcall (opcode instr)) stack))
         
         ;; Unary operations:
         ((CAR CDR CADR NOT LIST1 COMPILER DISPLAY WRITE RANDOM) 
          (push (funcall (opcode instr) (pop stack)) stack))
         
         ;; Binary operations:
         ((+ - * / < > <= >= /= = CONS LIST2 NAME! EQ EQUAL EQL)
          (setf stack (cons (funcall (opcode instr) (second stack)
                                     (first stack))
                            (rest2 stack))))
         
         ;; Ternary operations:
         (LIST3
          (setf stack (cons (funcall (opcode instr) (third stack)
                                     (second stack) (first stack))
                            (rest3 stack))))
         
         ;; Constants:
         ((T NIL -1 0 1 2)
          (push (opcode instr) stack))
         
         ;; Other:
         ((HALT) (RETURN (top stack)))
         (otherwise (error "Unknown opcode: ~a" instr))))))

(defun init-scheme-comp ()
  "Initialize values (including call/cc) for the Scheme compiler."
  (set-global-var! 'exit 
    (new-fn :name 'exit :args '(val) :code '((HALT))))
  (set-global-var! 'call/cc
    (new-fn :name 'call/cc :args '(f)
            :code '((ARGS 1) (CC) (LVAR 0 0 ";" f)
		    (CALLJ 1)))) ; *** Bug fix, gat, 11/9/92
  (dolist (prim *primitive-fns*)
     (setf (get (prim-symbol prim) 'global-val)
           (new-fn :env nil :name (prim-symbol prim)
                   :code (seq (gen 'PRIM (prim-symbol prim))
                              (gen 'RETURN))))))

;;; ==============================

(defconstant scheme-top-level
  '(begin (define (scheme)
            (newline)
            (display "=> ")
            (write ((compiler (read))))
            (scheme))
          (scheme)))

(defun scheme ()
  "A compiled Scheme read-eval-print loop"
  (init-scheme-comp)
  (machine (compiler scheme-top-level)))

(defun comp-go (exp)
  "Compile and execute the expression."
  (machine (compiler `(exit ,exp))))

;;;; Peephole Optimizer


;;; ==============================

(defun optimize (code)
  "Perform peephole optimization on assembly code."
  (let ((any-change nil))
    ;; Optimize each tail  
    (loop for code-tail on code do
          (setf any-change (or (optimize-1 code-tail code)
                               any-change)))
    ;; If any changes were made, call optimize again
    (if any-change
        (optimize code)
        code)))

;;; ==============================

(defun optimize-1 (code all-code)
  "Perform peephole optimization on a tail of the assembly code.
  If a change is made, return true."
  ;; Data-driven by the opcode of the first instruction
  (let* ((instr (first code))
         (optimizer (get-optimizer (opcode instr))))
    (when optimizer
      (funcall optimizer instr code all-code))))

;;; ==============================

(let ((optimizers (make-hash-table :test #'eql)))

  (defun get-optimizer (opcode)
    "Get the assembly language optimizer for this opcode."
    (gethash opcode optimizers))

  (defun put-optimizer (opcode fn)
    "Store an assembly language optimizer for this opcode."
    (setf (gethash opcode optimizers) fn)))

;;; ==============================

(defun gen1 (&rest args) "Generate a single instruction" args)
(defun target (instr code) (second (member (arg1 instr) code)))
(defun next-instr (code) (find-if (complement #'label-p) code))

;;; ==============================

(defmacro def-optimizer (opcodes args &body body)
  "Define assembly language optimizers for these opcodes."
  (assert (and (listp opcodes) (listp args) (= (length args) 3)))
  `(dolist (op ',opcodes)
     (put-optimizer op #'(lambda ,args .,body))))

;;;; Now for some additions and answers to exercises:

;;; ==============================

(defconstant eof "EoF")
(defun eof-object? (x) (eq x eof))
(defvar *scheme-readtable* (copy-readtable))

(defun scheme-read (&optional (stream *standard-input*))
  (let ((*readtable* *scheme-readtable*))
    (read stream nil eof)))

;;; ==============================

(set-dispatch-macro-character #\# #\t 
  #'(lambda (&rest ignore) t)
  *scheme-readtable*)

(set-dispatch-macro-character #\# #\f 
  #'(lambda (&rest ignore) nil)
  *scheme-readtable*)

(set-dispatch-macro-character #\# #\d
  ;; In both Common Lisp and Scheme,
  ;; #x, #o and #b are hexidecimal, octal, and binary,
  ;; e.g. #xff = #o377 = #b11111111 = 255
  ;; In Scheme only, #d255 is decimal 255.
  #'(lambda (stream &rest ignore) 
      (let ((*read-base* 10)) (scheme-read stream)))
  *scheme-readtable*)

(set-macro-character #\` 
  #'(lambda (s ignore) (list 'quasiquote (scheme-read s))) 
  nil *scheme-readtable*)

(set-macro-character #\, 
   #'(lambda (stream ignore)
       (let ((ch (read-char stream)))
         (if (char= ch #\@)
             (list 'unquote-splicing (read stream))
             (progn (unread-char ch stream)
                    (list 'unquote (read stream))))))
   nil *scheme-readtable*)

;;; ==============================

(defparameter *primitive-fns*
  '((+ 2 + true) (- 2 - true) (* 2 * true) (/ 2 / true)
    (< 2 <) (> 2 >) (<= 2 <=) (>= 2 >=) (/= 2 /=) (= 2 =)
    (eq? 2 eq) (equal? 2 equal) (eqv? 2 eql)
    (not 1 not) (null? 1 not)
    (car 1 car) (cdr 1 cdr)  (cadr 1 cadr) (cons 2 cons true)
    (list 1 list1 true) (list 2 list2 true) (list 3 list3 true)
    (read 0 scheme-read nil t) (eof-object? 1 eof-object?) ;***
    (write 1 write nil t) (display 1 display nil t)
    (newline 0 newline nil t) (compiler 1 compiler t) 
    (name! 2 name! true t) (random 1 random true nil)))


;;; ==============================

;(setf (scheme-macro 'quasiquote) 'quasi-q)

(defun quasi-q (x)
  "Expand a quasiquote form into append, list, and cons calls."
  (cond
    ((vectorp x)
     (list 'apply 'vector (quasi-q (coerce x 'list))))
    ((atom x)
     (if (constantp x) x (list 'quote x)))
    ((starts-with x 'unquote)      
     (assert (and (rest x) (null (rest2 x))))
     (second x))
    ((starts-with x 'quasiquote)
     (assert (and (rest x) (null (rest2 x))))
     (quasi-q (quasi-q (second x))))
    ((starts-with (first x) 'unquote-splicing)
     (if (null (rest x))
         (second (first x))
         (list 'append (second (first x)) (quasi-q (rest x)))))
    (t (combine-quasiquote (quasi-q (car x))
                           (quasi-q (cdr x))
                           x))))

(defun combine-quasiquote (left right x)
  "Combine left and right (car and cdr), possibly re-using x."
  (cond ((and (constantp left) (constantp right))
         (if (and (eql (eval left) (first x))
                  (eql (eval right) (rest x)))
             (list 'quote x)
             (list 'quote (cons (eval left) (eval right)))))
        ((null right) (list 'list left))
        ((starts-with right 'list)
         (list* 'list left (rest right)))
        (t (list 'cons left right))))

;;; ==============================

(defun scheme-read (&optional (stream *standard-input*))
  (let ((*readtable* *scheme-readtable*))
    (convert-numbers (read stream nil eof))))

(defun convert-numbers (x)
  "Replace symbols that look like Scheme numbers with their values."
  ;; Don't copy structure, make changes in place.
  (typecase x
    (cons   (setf (car x) (convert-numbers (car x)))
            (setf (cdr x) (convert-numbers (cdr x)))
	    x) ; *** Bug fix, gat, 11/9/92
    (symbol (or (convert-number x) x))
    (vector (dotimes (i (length x))
              (setf (aref x i) (convert-numbers (aref x i))))
	    x) ; *** Bug fix, gat, 11/9/92
    (t x)))

(defun convert-number (symbol)
  "If str looks like a complex number, return the number."
  (let* ((str (symbol-name symbol))
         (pos (position-if #'sign-p str))
         (end (- (length str) 1)))
    (when (and pos (char-equal (char str end) #\i))
      (let ((re (read-from-string str nil nil :start 0 :end pos))
            (im (read-from-string str nil nil :start pos :end end)))
        (when (and (numberp re) (numberp im))
          (complex re im))))))

(defun sign-p (char) (find char "+-"))
