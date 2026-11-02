;;;; datapouch/application.lisp


;;; A very dumb attempt at creating nesting contexts, to simplify user-designed apps.
;;; Be aware:
;;;   This is made from shit and sticks. Any attempt to use this, other
;;;   than with provided push-new-application and with-app-exit-progn,
;;;   *will* fail, and it will fail *spectacularly*, adding swathes of grey hairs
;;;   onto your poor head.
;;;
;;; (These two functions ensure that invariants of application stack are not broken.
;;;  You will be able to create as many applications as you want, when you
;;;  want. But all these applications will be created in a sequence, and all
;;;  their with-app-exit-progn will memorize sequentially growing application
;;;  stack. And in case of pushing new app during exit phase of an old one: old
;;;  app will be cleaned up *before* any attempt at establishing let scope of
;;;  new app's with-app-exit-progn is made.
;;;
;;;  Easy. Just don't touch anything, and everything will be fine.)


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


;; NOTE: Very good for push-new-application also.
;;       Why? Because then invokation will be not just the last call, before return:
;;       It will be called *after* current app is no more.
(defmacro with-app-exit-progn (exit-progn-symbol &body body)
  (with-gensyms
    (old-application-stack)
    `(let ((,old-application-stack d.app:*application-stack*))
       (flet ((,exit-progn-symbol (&rest forms)
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
