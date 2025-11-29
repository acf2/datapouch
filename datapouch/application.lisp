;;;; datapouch/application.lisp


;;; A very dumb attempt at creating nesting contexts, to simplify user-designed apps.


(in-package :datapouch.application)


(defparameter *application-stack* nil)


(defclass application ()
  ((application-rmacro-callbacks :initarg :rmacro-callbacks
                                 :reader app-rmacro-callbacks)
   (application-prompt-fun :initarg :prompt-fun
                           :reader app-prompt-fun)))


;;; Returns three values:
;;;   form, read from input
;;;   is-eof, boolean value
;;;   buffer, unused characters
(defun app-read-form (buffer application)
  (let ((*readtable* d.cli:*datapouch-readtable*)
        (d.rmacro:*rmacro-callbacks* (app-rmacro-callbacks application)))
    (d.cli:read-form buffer (app-prompt-fun application))))


(defun get-app-repl-read-form ()
  (d.cli:get-parametrized-repl-read-form
    (lambda (buffer)
      (declare (special *application-stack*))
      (multiple-value-bind (form eof new-buffer) (app-read-form buffer (first *application-stack*))
        (cond ((and eof (rest *application-stack*))
               (setf *application-stack* (rest *application-stack*))
               (values form nil new-buffer))
              (:else
                (values form eof new-buffer)))))))
