;;;; datapouch-cli.lisp

(in-package :datapouch.cli)

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

;; NOTE: Nice function to test other langs on.
;;       Complex graph for such simple operation.
;; Returns (form unused-chars is-eof?)
(defun try-to-read-form (prompt &key ((:accumulator accumulator) nil))
    (when accumulator
      (handler-case (multiple-value-bind (form last-character) (read-from-string accumulator)
                      (return-from try-to-read-form (values form
                                                            (if (< last-character (length accumulator))
                                                              (subseq accumulator last-character)
                                                              nil)
                                                            nil)))
        ; EOF from read-from-string means that form is not finished
        (end-of-file () nil)))
    (let* ((line (rl:readline :prompt prompt
                              :erase-empty-line nil
                              :add-history t))
           (new-accumulator (string-left-trim '(#\Space #\Newline #\Tab)
                                              (concatenate 'string accumulator +default-term-separator+ line))))
      (if (null line) ; nil from rl:readline means EOF
        (return-from try-to-read-form (values nil accumulator t))
        (handler-case
          (multiple-value-bind (form last-character) (read-from-string new-accumulator)
            (return-from try-to-read-form (values form
                                                  (if (< last-character (length new-accumulator))
                                                    (subseq new-accumulator last-character)
                                                    nil)
                                                  nil)))
          ; EOF from read-from-string means that form is not finished
          (end-of-file () (values nil (if (string= new-accumulator "") nil new-accumulator) nil))))))

(defclass interactive-input ()
  ((buffer :initform nil
           :documentation "Buffer for unfinished s-expressions")
   (prompt-list :initarg :prompt-list
                :accessor prompt-list)
   (command-sign :initarg :command-sign
                 :accessor command-sign)
   (accumulator-sign :initarg :accumulator-sign
                     :accessor accumulator-sign))
  (:default-initargs :prompt-list nil
                     :command-sign +default-command-sign+
                     :accumulator-sign +default-accumulator-sign+)
  (:documentation "Datapouch default interactive input class. Serves as the main input access point for other code."))

(defgeneric get-prompt (input-object)
  (:documentation "Get prompt string from input object"))

(defmethod get-prompt ((input interactive-input))
  (with-slots (buffer prompt-list command-sign accumulator-sign) input
    (format nil "~{~#[~^~;~A~:;~A-~]~}~A "
            (map 'list (lambda (sym) (string-downcase (symbol-name sym)))
                 prompt-list)
            (if buffer
              accumulator-sign
              command-sign))))

(defgeneric read-form (input-object)
  (:documentation "Read one form from input object"))

;; Returns (form is-eof?)
(defmethod read-form ((input interactive-input))
  (with-slots (buffer) input
    (loop named readloop do
          (multiple-value-bind (form unused-chars is-eof?)
            (handler-case (try-to-read-form (get-prompt input) :accumulator buffer)
              (sb-sys:interactive-interrupt ()
                                            (return-from read-form (values nil nil))
                                            (setf buffer nil))
              (error (c)
                     (setf buffer nil)
                     (error c)))
            (setf buffer unused-chars)
            (when (or form is-eof?)
              (return-from readloop (values form is-eof?)))))))

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
  ;; Piece of shit
  ;; https://github.com/hanslub42/rlwrap/issues/108
  (rl:variable-bind "enable-bracketed-paste" "off")
  (let ((input (or input-object
                   (make-instance 'interactive-input))))
    (loop
      (handler-case (multiple-value-bind (form is-eof?) (read-form input)
                      (when (and (null form) is-eof?)
                        (format t "~&")
                        (return-from mainloop))
                      (unless (null form)
                        (multiple-value-bind (vals c) (evaluate-form form)
                          (when (and print-output (null c))
                            (format t "~&~{~S~%~}" vals)))))
        (sb-int:simple-reader-error (c) (format t *msg/expression-error* c))))))
