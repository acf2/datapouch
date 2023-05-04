;;;; load.lisp

;; Cannot delete those loads, because asdf starts to cry about warnings
(loop for p in '(:sqlite :sxql :cl-readline :cl-ppcre) do
      (ql:quickload p :silent t))

(defparameter work-dir (directory-namestring (or *load-truename* *default-pathname-defaults*)))

(pushnew (truename (make-pathname :directory work-dir
                                  :name "datapouch"))
         asdf:*central-registry*
         :test #'equal)

(with-open-stream (*standard-output* (make-broadcast-stream))
  (asdf:load-system :datapouch))
