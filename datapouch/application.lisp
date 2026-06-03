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
   (application-result-callback :initarg :result-callback
                                :reader result-callback)
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
                                  ((:read-form-fun read-form-fun) #'d.cli:read-form)
                                  ((:result-callback result-callback) nil))
  (push
    (make-instance 'application
                   :prompt-fun prompt-fun
                   :rmacro-callbacks rmacro-callbacks
                   :expander-callbacks expander-callbacks
                   :autocomplete-tree autocomplete-tree
                   :docs docs
                   :result-callback (lambda (result)
                                      (pop *application-stack*)
                                      (when result-callback
                                        (funcall result-callback result)))
                   :read-form-fun read-form-fun)
    *application-stack*))


;; WARNING: Finicky thing, if called many times sequentially, or immediately in
;;          the same place after push-new-application call. Please, try to use it as
;;          intended - to complete application work in the end of some workflow.
(defun get-current-return ()
  (result-callback (first *application-stack*)))


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
