;;;; datapouch-cli.lisp

(in-package :datapouch.cli)

(defparameter *msg/expression-error* "Expression error: ~A~&")
(defparameter *msg/undefined-function-error* "Error: unknown function (~A).~&")
(defparameter *msg/unbound-variable-error* "Error: unknown variable (~A).~&")
(defparameter *msg/generic-error* "Error: ~A.~&")

(defparameter *msg/generic-warning* "Warning: ~A.~&")

;;; Make defconstant when possible
;;;   Avoid alexandria like fire, but if it can't be helped
;;;   use alexandria:define-constant
;;;  https://stackoverflow.com/a/34801400
(defvar +default-command-sign+ ">")
(defvar +default-accumulator-sign+ "*")
(defvar +default-term-separator+ " ")
(defvar +default-space-characters+ (list #\Space #\Newline #\Tab))

;;; Returns two values: form, unused characters
(defun try-to-read-form (form-string)
  (handler-case
    (multiple-value-bind (form last-character) (read-from-string form-string)
      (values form (if (< last-character (length form-string))
                     (subseq form-string last-character)
                     "")))
    ;; EOF from read-from-string means that form is not complete
    (end-of-file () (values nil form-string))))

(defclass interactive-input ()
  ((buffer :initform ""
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
                 (reverse prompt-list))
            (if (string= buffer "")
              command-sign
              accumulator-sign))))

;;; Must be a list of lists of pairs
;;; Whenever a line has no leading opening bracket
;;; this line will be matched against regexes inside these lists
;;;
;;; Each list of pairs corresponds to one of user modules
;;; Each pair is (regex . pattern). When the line matches regex
;;; parameters from named groups are passed to substitution pattern.
(defparameter *shortcut-lexicon* '())

(defgeneric read-form (input-object)
  (:documentation "Read one form from input object"))

;;; Read one form from interactive-input
;;;   (right now just reads from stdin, but uses interactive-input;
;;;    maybe this will change in the future)
;;; Returns two values:
;;;   form, read from input
;;;   is-eof, boolean value
(defmethod read-form ((input interactive-input))
  (with-slots (buffer) input
    (multiple-value-bind (previous-form unused-chars) (try-to-read-form buffer)
      (if previous-form
        (progn
          (setf buffer unused-chars)
          (values previous-form nil))

        (loop for line = (handler-case (rl:readline :prompt (get-prompt input)
                                                    :erase-empty-line nil
                                                    :add-history t)
                           (sb-sys:interactive-interrupt (c)
                                                         (setf buffer "")
                                                         (error c))
                           (error (c)
                                  (setf buffer "") ; XXX: Should it really be that way? Maybe not every error?..
                                  (error c)))
              with form
              unless line return (values nil t) ; nil from rl:readline means EOF
              do (setf buffer (string-left-trim +default-space-characters+
                                                (concatenate 'string buffer +default-term-separator+ line)))
              (setf form (multiple-value-bind (form unused-chars) (try-to-read-form buffer)
                           (setf buffer unused-chars)
                           form))
              when form return (values form nil))))))

(defun evaluate-form (form)
  (handler-case (values (multiple-value-list (eval form)) nil)
    (undefined-function (c)
                        (format t *msg/undefined-function-error* (cell-error-name c))
                        (values nil c))
    (unbound-variable (c)
                      (format t *msg/unbound-variable-error* (cell-error-name c))
                      (values nil c))
    (warning (c)
             (format t *msg/generic-warning* c)
             (values nil c))
    (error (c)
           (format t *msg/generic-error* c)
           (values nil c))))

(defun mainloop (&key ((:input input-object) nil)
                      ((:print-output print-output) nil)
                      ((:history-file history-file) nil))
  ;; Piece of shit
  ;; https://github.com/hanslub42/rlwrap/issues/108
  (rl:variable-bind "enable-bracketed-paste" "off") ; XXX: Maybe it should remember previous setting?
  (when history-file (rl:read-history (namestring (truename history-file))))
  (unwind-protect
    (let ((input (or input-object
                     (make-instance 'interactive-input))))
      (loop
        (handler-case (multiple-value-bind (form is-eof?) (read-form input)
                        (when (and (null form) is-eof?)
                          (format t "~%")
                          (return-from mainloop))
                        (unless (null form)
                          (multiple-value-bind (vals c) (evaluate-form form)
                            (when (and print-output (null c))
                              (format t "~&~{~S~%~}" vals)))))
          (sb-sys:interactive-interrupt () (format t "~%"))
          (sb-int:simple-reader-error (c) (format t *msg/expression-error* c)))))
    (when history-file (rl:write-history (namestring (truename history-file))))))
