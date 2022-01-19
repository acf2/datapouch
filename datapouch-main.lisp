;;;; datapouch-main.lisp

(in-package :datapouch.main)

(defvar +config-path+ #P"~/.config/datapouch/user.lisp")
(defvar +config-sample+ '("user.lisp"
                          "This is a sample config file"
                          "It would not be modified automatically once created"))

(defun ensure-config-exists ()
  (or (probe-file +config-path+)
      (progn
        (ensure-directories-exist (make-pathname :directory (pathname-directory +config-path+)))
        (with-open-file (file +config-path+ :direction :output)
          (format file ";;;; ~A~&~{;; ~A~&~}" (first +config-sample+) (rest +config-sample+))))))

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
(defparameter *editor-interface* (lambda (pathname) (list *editor* (namestring pathname))))
(defparameter *print-output* t)

(defmacro edit (&optional text)
  `(call-editor *editor-interface* :initial-text ,text))

(defun main ()
  (in-package :datapouch.user)
  (ensure-config-exists)
  (load (namestring +config-path+))
  (d.cli:mainloop :print-output *print-output*)
  0)
