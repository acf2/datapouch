;;;; datapouch-cli.lisp


(in-package :datapouch.cli)


(defvar +default-line-separator+ (string #\newline))
(defvar +default-space-characters+ (list #\space #\newline #\tab))


(defun default-prompt (buffer)
  (if (string= buffer "")
    "> "
    "* "))
(defparameter *prompt-fun* #'default-prompt)


(defparameter *custom-readtable* (copy-readtable *readtable*))


;;; Read one line from with rl:readline
;;; Returns two values:
;;;   form, read from input
;;;   is-eof, boolean value
;;;
;;; I dunno what to do with output-stream for rl:readline
;;; More here: https://github.com/vindarel/cl-readline/blob/7653bc094c8f9bf151dde8dbfb3e2d261003047e/cl-readline.lisp#L134
;;; Especially when it's a synonym stream
;;; What? (sb-sys:fd-stream-fd (eval (synonym-stream-symbol output-stream)))? Like with naked eval?
(defun readline (buffer prompt-fun)
  (let ((line (rl:readline :prompt (funcall prompt-fun buffer)
                           :erase-empty-line nil
                           :add-history t)))
    ;; nil from rl:readline means EOF
    (values line (and (null line)
                      (string= buffer "")))))

;;; Returns two values: form, unused characters
;;;
;;; linedit uses separate package for reads
;;; I don't really know why, and I'm writing this just by the seat of my own pants.
;;; Maybe because of reader-time shenanigans with sharpsign-dot? Who knows?
;;;
;;; It goes like this:
;;; (let ((*readtable* table)
;;;       (*package* (make-package "DATAPOUCH-READTIME-TEMPORARY")))
;;;   (unwind-protect
;;;     (read-from-string form-string)
;;;     (delete-package *package*)))
;;;
;;; Personally, I like to create special "user" package, where user can have all the fun he wants.
;;; In particular, user wouldn't need to prefix every function and variable with package name.
;;; Why is it worse than temporary packages?
;;;
;;; Reference: https://github.com/sharplispers/linedit/blob/master/main.lisp#L76
(defun try-to-read-form (form-string)
  (declare (type string form-string))
  (handler-case (multiple-value-bind (form last-character)
                  (read-from-string form-string)
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
      (loop :for (line is-eof) = (multiple-value-list (readline new-buffer prompt-fun))
            :with form
            :when is-eof :return (values nil new-buffer t)
            :when (null line) :do (error (make-instance 'end-of-file))
            :do (setf new-buffer (string-left-trim +default-space-characters+
                                                   (concatenate 'string new-buffer +default-line-separator+ line +default-line-separator+)))
            :do (multiple-value-setq (form new-buffer) (try-to-read-form new-buffer))
            :when form :return (values form new-buffer nil)))))


;;; Piece of shit
;;; https://github.com/hanslub42/rlwrap/issues/108
(let (bracketed-paste)
  (defun disable-bracketed-paste ()
    (unless bracketed-paste
      (setf bracketed-paste (rl:variable-value "enable-bracketed-paste")))
    (rl:variable-bind "enable-bracketed-paste" "off"))

  (defun restore-bracketed-paste ()
    (when bracketed-paste
      (rl:variable-bind "enable-bracketed-paste" bracketed-paste))))


(defparameter *there-is-no-fresh-line-now* t)
(defparameter *buffer* "")


;;; XXX: This is the bug ridden hell.
;;;      I dunno what to do with this.
;;;      [1] and [2] should be fixed somehow, and then this clusterduck can be eliminated
(defun get-repl ()
  (lambda (in out)
    (declare (ignore in))
    ;; Why fresh-line does not work here? [1]
    ;; (With fresh-line here, *there-is-no-fresh-line-now* can be removed entirely)
    (if *there-is-no-fresh-line-now*
      (setf *there-is-no-fresh-line-now* nil)
      (terpri out))
    (multiple-value-bind (form new-buffer eof)
      (let ((*readtable* *custom-readtable*))
        (read-form *buffer* *prompt-fun*))
      (when eof
        ;; Why fresh-line does not work here? [2]
        (terpri out)
        (sb-ext:quit))
      (setf *buffer* new-buffer)
      form)))
