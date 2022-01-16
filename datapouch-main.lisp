;;;; datapouch-main.lisp

(in-package :datapouch.main)

(defun call-editor (editor-interface &key ((:initial-text initial-text) nil))
  (with-temporary-file (:stream stream :pathname pathname :prefix "" :suffix "")
    (when initial-text
      (write-string initial-text stream))
    :close-stream
    (run-program (funcall editor-interface pathname)
                 :input :interactive
                 :output :interactive)
    (with-open-file (stream pathname)
      (read-file-string pathname))))

(defparameter *editor* "vim")

(defvar +default-editor-interface+ (lambda (pathname) (list *editor* (namestring pathname))))
(defparameter *editor-interface* +default-editor-interface+)

(defmacro edit (&optional text)
  `(call-editor *editor-interface* :initial-text ,text))

(defun main ()
  (in-package :datapouch.user)
  (load "user.lisp")
  (d.cli:mainloop :print-output t)
  0)
