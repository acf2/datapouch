;;;; load.lisp

;; Cannot delete those loads, because asdf starts to cry about warnings
;; Not enough experience to understand what's wrong
(loop for p in '(:cl-readline :cl-ppcre :sqlite :sxql :cl-reexport :local-time :uiop :ironclad) do
      (ql:quickload p :silent t))

(ql:quickload :cl-package-locks :silent t) ;;; VIOLENCE!!1 KLUDGES FOR THE KLUDGE GOD!!1111 >:(

(defparameter work-dir (directory-namestring (or *load-truename* *default-pathname-defaults*)))

(loop for path in (list (truename (make-pathname :directory work-dir
                                                 :name "datapouch"))
                        (truename (make-pathname :directory work-dir
                                                 :name "zettelkasten-and-accounting")))
      do (pushnew path asdf:*central-registry* :test #'equal))

(with-open-stream (*standard-output* (make-broadcast-stream))
  (asdf:load-system :zettelkasten-and-accounting))
