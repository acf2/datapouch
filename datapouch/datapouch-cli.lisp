;;;; datapouch-cli.lisp


(in-package :datapouch.cli)


(defvar +default-line-separator+ (string #\newline))
(defvar +default-space-characters+ (list #\Space #\Newline #\Tab))


(defun default-prompt (buffer)
  (if (string= buffer "")
    "> "
    "* "))
(defparameter *prompt-fun* #'default-prompt)


;;; Returns two values: form, unused characters
(defun try-to-read-form (form-string)
  (declare (type string form-string))
  (handler-case (multiple-value-bind (form last-character) (read-from-string form-string)
                  (values form (subseq form-string last-character)))
    ;; EOF from read-from-string means that form is not complete
    (end-of-file () (values nil form-string))))


;;; Read one form from interactive-input
;;; Returns three values:
;;;   form, read from input
;;;   buffer, unused characters
;;;   is-eof, boolean value
(defun read-form (buffer prompt-fun)
  (declare (type string buffer)
           (type function prompt-fun))
  (multiple-value-bind (previous-form new-buffer) (try-to-read-form buffer)
    (if previous-form
      (values previous-form new-buffer nil)
      (loop for line = (rl:readline :prompt (funcall prompt-fun new-buffer)
                                    :erase-empty-line nil
                                    :add-history t)
            with form
            when (and (null line) (string= new-buffer "")) return (values nil new-buffer t) ; nil from rl:readline means EOF
            when (null line) do (error (make-instance 'end-of-file))
            do (setf new-buffer (string-trim +default-space-characters+
                                             (concatenate 'string new-buffer +default-line-separator+ line)))
            ;; Here linedit, for example, reads to throwaway package. Why? Should it do the same?
            do (multiple-value-setq (form new-buffer) (try-to-read-form new-buffer))
            when form return (values form new-buffer nil)))))


;; Piece of shit
;; https://github.com/hanslub42/rlwrap/issues/108
(let (bracketed-paste)
  (defun disable-bracketed-paste () 
    (unless bracketed-paste
      (setf bracketed-paste (rl:variable-value "enable-bracketed-paste")))
    (rl:variable-bind "enable-bracketed-paste" "off"))

  (defun restore-bracketed-paste ()
    (when bracketed-paste
      (rl:variable-bind "enable-bracketed-paste" bracketed-paste))))


(defparameter *no-newline* t)
(defparameter *buffer* "")


;; XXX: This is the bug ridden hell.
;;      I dunno what to do with this.
;;      [1] and [2] should be fixed somehow, and then this clusterduck can be eliminated
(defun get-repl ()
  (lambda (in out)
    (declare (ignore in))
    ;; Why fresh-line does not work here? [1]
    ;; (With fresh-line here, *no-newline* can be removed entirely)
    (if *no-newline*
      (setf *no-newline* nil)
      (terpri out))
    (multiple-value-bind (form new-buffer eof) (read-form *buffer* *prompt-fun*)
      (when eof
        ;; Why fresh-line does not work here? [2]
        (terpri out)
        (sb-ext:quit))
      (setf *buffer* new-buffer)
      form)))


;; list of pairs:
;; ("command" . command-handler)
(defparameter *command-table* nil)


(defun command-reader-macro (stream char)
  (let* ((command (read stream))
         (command-handler (loop for (command-name . command-handler) in *command-table*
                                if (string= (string-upcase command)
                                            (string-upcase command-name))
                                return command-handler)))
    (if command-handler
      (funcall command-handler stream char)
      (error 'unknown-command))))


;;; Use this in reader macro
(defun read-line-to-semicolon-or-newline (stream char)
  (declare (ignore char))
  (let ((line (make-array 0
                          :element-type 'character
                          :fill-pointer 0
                          :adjustable t)))
    (loop for char = (read-char stream)
          until (or (eql char #\newline)
                    (eql char #\;))
          return line)))
