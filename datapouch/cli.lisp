;;;; datapouch-cli.lisp


(in-package :datapouch.cli)


(defvar +default-line-separator+ (string #\newline))
(defvar +default-space-characters+ (list #\space #\newline #\tab))


(defun default-prompt (buffer)
  (format nil "~:[*~:;>~] " (string= buffer "")))
(defparameter *prompt-fun* #'default-prompt)


;; Fixes fresh-line bug, if heretical repl is not available
(defparameter *add-fresh-line-after-each-result-print* nil)


(defparameter *custom-readtable* (copy-readtable *readtable*))


(defparameter *noprint-result* t)
(defparameter *noprint-prompt* nil)


(defparameter *buffer* "")


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
  (let ((line (rl:readline :prompt (when (not *noprint-prompt*)
                                     (funcall prompt-fun buffer))
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


;;; Read one form with readline (I cannot really control its input stream?..)
;;; Returns three values:
;;;   form, read from input
;;;   is-eof, boolean value
;;;   buffer, unused characters
(defun read-form (buffer prompt-fun)
  (declare (type string buffer)
           (type function prompt-fun))
  (multiple-value-bind (previous-form new-buffer) (try-to-read-form buffer)
    (if previous-form
      (values previous-form nil new-buffer)
      (loop :for (line is-eof) = (multiple-value-list (readline new-buffer prompt-fun))
            :with form
            :when is-eof :return (values nil t new-buffer)
            :when (null line) :do (error (make-instance 'end-of-file))
            :do (setf new-buffer (string-left-trim +default-space-characters+
                                                   (concatenate 'string new-buffer +default-line-separator+ line +default-line-separator+)))
            :do (multiple-value-setq (form new-buffer) (try-to-read-form new-buffer))
            :when form :return (values form nil new-buffer)))))


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


;;; XXX: Fixed fresh-line bug, by introducing The Harbinger of Kludge God
;;;      More info below.
;;; NOTE: add-fresh-line-after... is not a perfect mitigation in case of broken readline repl function
;;;       e.g. when debugger is called without new repl, additional new line is outputted
;;;       To fix this, revert there-is-no-fresh-line-now to global/special and add debug hook
(let (there-is-no-fresh-line-now)
(defun get-repl-read-form ()
  (lambda (in out)
    (declare (ignore in))
    (when there-is-no-fresh-line-now
      (terpri *standard-output*)
      (setf there-is-no-fresh-line-now nil))
    (handler-case
      (multiple-value-bind (form eof new-buffer)
        (let ((*readtable* *custom-readtable*))
          (read-form *buffer* *prompt-fun*))
        (cond (eof
                (terpri out)
                (sb-ext:quit))
              ((and *add-fresh-line-after-each-result-print*
                    (not *noprint-prompt*)
                    (not *noprint-result*))
               (setf there-is-no-fresh-line-now t)))
        (setf *buffer* new-buffer)
        form)
      (sb-int:simple-reader-error (c)
                                  (setf *buffer* "")
                                  (error c))))))


;; Try to look at this variable, before doing anything rash.
;; repl-fun depends on internal functions heavily,
;; so it is likely to be broken, when SBCL is updated.
(defparameter *heretical-repl-available* t)


;;; XXX: Yes, I'm gonna burn in hell for this. Don't bother complaining.
;;; XXX: Yes, I just removed everything about prompt, because readline will use it automatically.
;;;      And no, using it here is worse, because some other code changes *prompt-fun* on-the-fly,
;;;      and I feel better, when its usage is incapsulated in part of my code.
;;;      ...not this abomination.
(defun repl-fun-with-readline (noprint-global)
  (declare (special *noprint-prompt* *noprint-result*))
  ;(/show0 "entering REPL") ; TBD: Make this work, at least. Its sb-int:/show0
  (let* ((*noprint-prompt* (or noprint-global *noprint-prompt*)) ; Yup, readline uses it
         (*noprint-result* (or noprint-global *noprint-result*)))
    (loop
      (unwind-protect
        (progn
          (sb-sys:scrub-control-stack)
          (sb-thread::get-foreground)
          ;(unless *noprint-prompt*
          ;(sb-int:flush-standard-output-streams)
          ;(funcall sb-impl::*repl-prompt-fun* *standard-output*)
          ;; (Should *REPL-PROMPT-FUN* be responsible for doing its own
          ;; FORCE-OUTPUT? I can't imagine a valid reason for it not to
          ;; be done here, so leaving it up to *REPL-PROMPT-FUN* seems
          ;; odd. But maybe there *is* a valid reason in some
          ;; circumstances? perhaps some deadlock issue when being driven
          ;; by another process or something...)
          ;(force-output *standard-output*)
          ;(let ((real (sb-impl::maybe-resolve-synonym-stream *standard-output*)))
          ;; Because by default *standard-output* is not
          ;; *terminal-io* but STDOUT the column is not reset
          ;; after pressing enter. Reduce confusion by resetting
          ;; the column to 0
          ;(when (sb-sys:fd-stream-p real)
          ;(setf (sb-impl::fd-stream-output-column real) 0))))
          (let* ((form (funcall sb-int:*repl-read-form-fun*
                                *standard-input*
                                *standard-output*))
                 (results (multiple-value-list (sb-impl::interactive-eval form))))
            (unless *noprint-result*
              (dolist (result results)
                (fresh-line)
                (sb-impl::prin1 result))
              (terpri)))) ;; NOTE: ALL THIS HERESY, FOR NEED OF A ONE LITTLE LINE OF CODE
        ;; If we started stepping in the debugger we want to stop now.
        (sb-impl::disable-stepping)))))
