;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Code for Paradigms of AI Programming
;;; Copyright (c) 1996 Peter Norvig

;;;; PAIP TUTOR

(requires "auxfns")

(defvar *chapters* '() "List of chapter structures, one per chapter.")

(defun do-examples (chapters &optional (stream *standard-output*))
  "Run examples from one or more chapters and sum the number of errors.  
  If all is well, this should return 0. If STREAM is nil, very little 
  output is produced."
  (loop for chapter in (cond ((member chapters '(all :all)) *chapters*)
			     ((listp chapters) chapters)
			     (t (list chapters)))
	sum (do-chapter chapter stream)))

(defmacro defexamples (chapter-number title &rest examples)
  "Define a set of test examples.  Each example is of the form 
     (exp [ => result ] [ @ page ] [ :input string ]) 
  where [] indicates an optional part, and the parts can be in any order.
  Evaluate exp and complain if it is not equal to result.  The page is
  the page in the book where the example appears.  An 'example' may also be
  one of the following:
     string                   Serves as documentation
     (:SECTION string)        Says what section of book we're in"
  `(add-chapter ',chapter-number ',title ',examples))

(defun do-chapter (chapter interface)
  "Run the examples in a chapter.  Return the number of unexpected results."
  (let ((chapter (find-chapter chapter)))
    (set-chapter chapter interface)
    (let ((n (count-if-not 
	      #'(lambda (example)
		  (do-example example interface))
	      (chapter-examples chapter))))
      (if (> n 0)
	  (format t "~%**** ~D unexpected result~:p on Chapter ~D"
		  n chapter)
	(format t "~%Chapter ~D done.~%" chapter))
      n)))

(defstruct (chapter (:print-function 
		(lambda (chapter stream depth)
		  (declare (ignore depth))
		  (format stream "~2D. ~A" (chapter-number chapter)
			  (chapter-title chapter)))))
  number title examples)

(defun add-chapter (number title examples)
  "The functional interface for defexamples: adds test examples."
  (let ((chapter (make-chapter :number number :title title 
			       :examples examples)))
    (setf *chapters* 
	  (sort 
	   (cons chapter (delete number *chapters* :key #'chapter-number))
	   #'< :key #'chapter-number))
    chapter))

(defun find-chapter (number)
  "Given a chapter number, find the chapter structure for it."
  (typecase number
    (chapter number) ; If given a chapter, just return it.
    (t (find number *chapters* :key #'chapter-number))))

(defun do-example (example interface)
  "Run an example; print out what's happening unless INTERFACE is nil.
  Return nil if there is a unexpected result."
  (let* ((stream (output-stream interface))
	 (*print-pretty* t)
         (*standard-output* stream)
         (*trace-output* stream)
	 (*debug-io* stream)
	 (expected ':anything)
	 (result nil))
    (cond ((stringp example)
	   (when stream
	     (format stream "~A~%" example)))
	  ((starts-with example ':section)
	   (display-section (second example) interface))
	  ((consp example)
	   (let ((exp (copy-tree (first example))) ;; To avoid NCONC problems
		 (page (getf (rest example) '@))
		 (input (getf (rest example) ':input)))
	     (setf result nil)
	     (setf expected (getf (rest example) '=> ':anything))
	     (set-example example interface)
             (when page
               (set-page page interface))
	     (when stream
	       (let ((*print-case* ':downcase))
		 (display-example exp interface)))
	     (if input
		 (with-input-from-string (*standard-input* input)
		   (setf result (eval exp)))
	         (setf result (eval exp)))
	     (when stream
	       (format stream "~&~S~%" result))
	     (unless (or (equal expected ':anything) 
                         (nearly-equal result expected))
	       (if stream 
		   (format *terminal-io*
			   "~%**** expected ~S" expected)
		   (format *terminal-io*
			   "~%**** For ~S~%     expected ~S~%      got:~S~%"
			   exp expected result)))))
	  ((atom example) (cerror "Bad example: ~A" example example)))
    ;; Return nil if there is a unexpected result:
    (or (eql expected ':anything) (nearly-equal result expected))))

(defun do-documentation-examples (examples interface)
  "Go through any documentation strings or (:SECTION ...) examples."
  (loop (let ((one (pop examples)))
	  (cond ((or (stringp one) (starts-with one ':section))
		 (do-example one interface))
		(t (RETURN)))))
  examples)

(defun nearly-equal (x y)
  "Are two objects nearly equal?  Like equal, except floating point numbers
  need only be within epsilon of each other."
  (let ((epsilon 0.001)) ;; could be more mathematically sophisticated
    (typecase x
      (FLOAT (and (floatp y) (< (abs (- x y)) epsilon)))
      (VECTOR (and (vectorp y) (eql (length x) (length y))
		   (nearly-equal (coerce x 'list) (coerce y 'list))))
      (CONS (and (consp y) 
		 (nearly-equal (car x) (car y)) 
		 (nearly-equal (cdr x) (cdr y))))
      (T (equal x y)))))

;;;; GUI Implementation

;;; We started to implement guis in UNUSED/gui-*

;;; If you want to write a GUI for the tutor, you need to do four things:

;;; (1) Define a class (or structure) which we call an interface -- it
;;; is the window in which the examples will be displayed.  

;;; (2) Define the function PAIP-TUTOR which should start up the interface.

;;; (3) Implement the following six methods on your interface:
;;; SET-CHAPTER, SET-PAGE, SET-EXAMPLE, 
;;; DISPLAY-EXAMPLE, DISPLAY-SECTION, OUTPUT-STREAM

;;; (4) Edit the file "auxfns.lisp" to include your files.

;;; Below we show an implementation for the five methods that is good
;;; for output streams (without any fancy window GUI).  


(defmethod set-chapter (chapter interface)
  ;; Update the interface to display this chapter
  (format (output-stream interface) "~2&Chapter ~A~%" chapter))

(defmethod set-page (page interface)
  ;; Update the interface to display the page number
  (format (output-stream interface) "~&; page ~D" page))

(defmethod set-example (example interface)
  ;; Update the interface to display this example. The idea is that
  ;; this shows the example in a popup menu or something, but does not
  ;; dsiplay it in the output stream.
  (declare (ignore example interface)))

(defmethod display-example (exp interface)
  ;; Display a prompt and the expression on the interface's output stream
  (format (output-stream interface) "~&> ~S~%" exp))

(defmethod display-section (section interface)
  ;; Display the string describing this section somewhere
  (format (output-stream interface) "~2&Section ~A~%" section))

(defmethod output-stream (interface)
  ;; The stream on which output will be printed
  interface)
