;;;; datapouch/application.lisp


;;; A very dumb attempt at creating nesting contexts, to simplify user-designed apps.


(in-package :datapouch.application)


(defparameter *application-stack* nil)


(defclass application ()
  ((application-rmacro-callbacks :initarg :rmacro-callbacks
                                 :reader rmacro-callbacks)
   (application-expander-callbacks :initarg :expander-callbacks
                                   :reader expander-callbacks)
   (application-autocomplete-tree :initarg :autocomplete-tree
                                  :reader autocomplete-tree)
   (application-documentation :initarg :docs ; Should be in the form ((docform short-docform documentation)...)
                              :reader docs)
   (application-prompt-fun :initarg :prompt-fun
                           :reader prompt-fun)
   (application-read-form-fun :initarg :read-form-fun
                              :initform #'d.cli:read-form
                              :reader read-form-fun))
  (:documentation "asdf"))


;; NOTE: Maybe later
;; (defmethod initialize-instance :after ((object application) &key)


;; WARNING: When used inside user handlers/rmacro callbacks, this
;;          [preferrably] should be the last call. Or you should *really* know
;;          what you're doing.
(defun push-new-application (&key ((:prompt-fun prompt-fun) d.cli:*prompt-fun*)
                                  ((:rmacro-callbacks rmacro-callbacks) nil)
                                  ((:expander-callbacks expander-callbacks) nil)
                                  ((:autocomplete-tree autocomplete-tree) nil)
                                  ((:docs docs) nil)
                                  ((:read-form-fun read-form-fun) #'d.cli:read-form))
  (push
    (make-instance 'application
                   :prompt-fun prompt-fun
                   :rmacro-callbacks rmacro-callbacks
                   :expander-callbacks expander-callbacks
                   :autocomplete-tree autocomplete-tree
                   :docs docs
                   :read-form-fun read-form-fun)
    *application-stack*))


(defmacro with-return (return-symbol &body body)
  (with-gensyms
    (old-application-stack)
    `(let ((,old-application-stack d.app:*application-stack*))
       (flet ((,return-symbol (&rest forms)
                              `(progn (setf d.app:*application-stack* ',,old-application-stack)
                                      ,@forms)))
         ,@body))))


;;; Returns three values:
;;;   form, read from input
;;;   is-eof, boolean value
;;;   buffer, unused characters
(defun app-read-form (buffer application)
  (let ((*readtable* d.cli:*datapouch-readtable*)
        (d.rmacro:*rmacro-callbacks* (rmacro-callbacks application))
        (d.cli:*autocomplete-tree* (autocomplete-tree application))
        (d.cli:*expander-callbacks* (expander-callbacks application)))
    (funcall (read-form-fun application) buffer (prompt-fun application))))


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
