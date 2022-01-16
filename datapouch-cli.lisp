;;;; datapouch-cli.lisp

(in-package :d.cli)

(defparameter *msg/expression-error* "Expression error: ~A~&")
(defparameter *msg/undefined-function-error* "Error: unknown function (~A).~&")
(defparameter *msg/unbound-variable-error* "Error: unknown variable (~A).~&")
(defparameter *msg/generic-error* "Error: ~A.~&")

;; Make defconstant when possible
;;   Avoid alexandria like fire, but if it can't be helped
;;   use alexandria:define-constant
;;   https://stackoverflow.com/a/34801400
(defvar +default-command-sign+ ">")
(defvar +default-accumulator-sign+ "*")
(defvar +default-term-separator+ " ")

(defvar +default-prompt-function+ (lambda (prompt-list sign)
                                    (format nil "~{~#[~^~;~A~:;~A-~]~}~A "
                                            (map 'list (lambda (sym) (string-downcase (symbol-name sym)))
                                                 prompt-list)
                                            sign)))

;; returns (form unused-characters is-eof?)
(defun readform (prompt-function prompt-list command-sign accumulator-sign &key ((:accumulator accumulator) nil))
  (flet ((readline (prompt-list sign)
                   (rl:readline :prompt (concatenate 'string (funcall prompt-function prompt-list sign))
                                :erase-empty-line nil
                                :add-history t)))
    (loop named mainloop
          with local-accumulator = accumulator
          unless (null local-accumulator) do
          (handler-case (multiple-value-bind (form last-character) (read-from-string local-accumulator)
                          (return-from mainloop (values form
                                                        (if (< last-character (length local-accumulator))
                                                          (subseq local-accumulator last-character)
                                                          nil)
                                                        nil)))
            (end-of-file () nil))
          do
          (handler-case (let ((line (readline prompt-list (if (null local-accumulator) command-sign accumulator-sign))))
                          (if (null line) ; nil from rl:readline means EOF
                            (return-from mainloop (values nil local-accumulator t))
                            (setf local-accumulator (concatenate 'string local-accumulator +default-term-separator+ line))))
            (sb-sys:interactive-interrupt () (return-from mainloop (values nil nil nil)))))))

(defclass interactive-input ()
  ((buffer :initform nil
           :documentation "Buffer for unfinished s-expressions")
   (prompt-function :initarg :prompt-function
                    :accessor prompt-function
                    :documentation "Generates prompt from prompt-list and signs")
   (prompt-list :initarg :prompt-list
                :accessor prompt-list)
   (command-sign :initarg :command-sign
                 :accessor command-sign)
   (accumulator-sign :initarg :accumulator-sign
                     :accessor accumulator-sign))
  (:default-initargs :prompt-function +default-prompt-function+
                     :prompt-list nil
                     :command-sign +default-command-sign+
                     :accumulator-sign +default-accumulator-sign+)
  (:documentation "Input object is the main input access point for other code"))

(defgeneric read-form-from (object)
  (:documentation "Reading new lisp form from some input device"))

;; return (form is-eof?)
(defmethod read-form-from ((input interactive-input))
  (with-slots (buffer prompt-function prompt-list command-sign accumulator-sign) input
    (multiple-value-bind (form unused-chars is-eof?)
      (handler-case (readform prompt-function prompt-list command-sign accumulator-sign :accumulator buffer)
        (error (c)
               (setf buffer nil)
               (error c)))
        (setf buffer unused-chars)
        (values form is-eof?))))

(defun evaluate-form (form)
  (handler-case (values (multiple-value-list (eval form)) nil)
    (undefined-function (c)
                        (format t *msg/undefined-function-error* (cell-error-name c))
                        (values nil c))
    (unbound-variable (c)
                      (format t *msg/unbound-variable-error* (cell-error-name c))
                      (values nil c))
    (error (c)
           (format t *msg/generic-error* c)
           (values nil c))))

(defun mainloop (&key ((:input input-object) nil) ((:print-output print-output) nil))
  (let ((input (or input-object
                   (make-instance 'interactive-input))))
    (loop
      (handler-case (multiple-value-bind (form is-eof?) (read-form-from input)
                      (when (and (null form) is-eof?)
                        (format t "~&")
                        (return-from mainloop))
                      (unless (null form)
                        (multiple-value-bind (vals c) (evaluate-form form)
                          (when (and print-output (null c))
                            (format t "~{~S~&~}" vals)))))
        (sb-int:simple-reader-error (c) (format t *msg/expression-error* c))))))
