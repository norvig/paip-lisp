(defpackage paip
  (:use :cl)
  (:shadow :defconstant)
  (:export :requires :*paip-files* :do-examples))

(in-package :paip)

(defmacro defconstant (symbol value &optional doc)
  (declare (cl:ignore doc))
  `(cl:defconstant ,symbol
     (or (and (boundp ',symbol)
              (symbol-value ',symbol))
         ,value)))

(defclass paip-source-file (asdf:cl-source-file) ())

(defmethod asdf:perform :around ((o asdf:compile-op) (c paip-source-file))
  (let ((*package* (find-package :paip)))
    (call-next-method)))

(asdf:defsystem "paip"
  :default-component-class paip-source-file
  :version "0.1"
  :author "Peter Norvig"
  :license "MIT"
  :serial t
  :components ((:module "lisp"
                :components
                ((:file "auxfns")
                 (:file "tutor")
                 (:file "examples"))))
  :description "Lisp code for the textbook \"Paradigms of Artificial Intelligence Programming\"")
