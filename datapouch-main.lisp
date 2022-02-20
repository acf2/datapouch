;;;; datapouch-main.lisp

(in-package :datapouch.main)

(defvar +config-path+ #P"~/.config/datapouch/user.lisp")
(defvar +config-sample+ '("user.lisp"
                          "This is a sample config file"
                          "It would not be modified automatically once created"))
(defparameter +work-dir+ (directory-namestring (or *load-truename* *default-pathname-defaults*)))


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
    (funcall editor-interface (list pathname))
    (with-open-file (stream pathname)
      (read-file-string pathname))))

;; This is the simplier version, maybe later I'll consider it.
;(defun call-editor (editor-interface &key initial-text/s)
;  (let ((streams))
;    (unwind-protect
;         (progn (dolist (it (alexandria:ensure-list initial-text/s))
;                  (push (cl-fad:open-temporary) streams)
;                  (write-string (or it "") (car streams)))
;                (funcall editor-interface (pathname (car streams))))
;      (itr (for s in streams)
;           (close s)
;           (collect (uiop:read-file-string (pathname s)))))))

;; Also, there is the version with explicit catamorphism.
;; Not that I know how it works. Maybe later too.

(defun call-editor-for-many (editor-interface initial-texts &optional (pathnames nil))
  (if initial-texts
    (let* ((other-results nil)
           (result (call-editor (lambda (new-pathname)
                                  (setf other-results (call-editor-for-many editor-interface
                                                                            (cdr initial-texts)
                                                                            (append pathnames new-pathname))))
                                :initial-text (car initial-texts))))
      (cons result other-results))
    (when pathnames
      (funcall editor-interface pathnames))))

(defparameter *editor-interface* (lambda (pathnames) (run-program (append (list "vim" "-p") (map 'list #'namestring pathnames))
                                                                  :input :interactive
                                                                  :output :interactive)))
(defparameter *print-output* t)
(defparameter *database-path* (make-pathname :directory (pathname-directory +config-path+)
                                             :name "database"))
(defparameter *input* (make-instance 'd.cli:interactive-input))

(defun ensure-database-exists ()
  (or (probe-file *database-path*)
      (progn
        (ensure-directories-exist (make-pathname :directory (pathname-directory *database-path*)))
        (with-open-file (file *database-path* :direction :output)
          nil))))

(defun edit-strings (&rest texts)
  (call-editor-for-many *editor-interface* texts))

(defun main ()
  (in-package :datapouch.user)
  (ensure-config-exists)
  (load (namestring +config-path+))
  (ensure-database-exists)
  (sqlite:with-open-database (db (truename *database-path*))
    (d.sql:with-binded-db db
      (d.cli:mainloop :input *input* :print-output *print-output*)))
  0)
