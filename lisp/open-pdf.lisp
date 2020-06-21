(defvar *paip-pdf-uri* "https://github.com/norvig/paip-lisp/raw/master/")

(defun open-pdf (&optional (part 1))
  (let* ((name (format nil "PAIP-part~A.pdf" part))
         (path (namestring
                (merge-pathnames
                 name
                 (asdf:system-source-directory (asdf:find-system :paip))))))
    (when (<= 1 part 2)
      (unless (probe-file path)
        #+quicklisp(ql:quickload :dexador)
        (uiop:symbol-call :dex :fetch (format nil "~A~A" *paip-pdf-uri* name) path))
      #+quicklisp(ql:quickload :trivial-open-browser)
      (uiop:symbol-call :trivial-open-browser :open-browser path)
      path)))
