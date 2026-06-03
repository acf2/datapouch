;;;; datapouch/cli.lisp


(in-package :datapouch.cli)


(defvar +default-line-separator+ (string #\newline))
(defvar +default-space-characters+ (list #\space #\newline #\tab))


(defun default-prompt (buffer)
  (format nil "~:[*~:;>~] " (string= buffer "")))
(defparameter *prompt-fun* #'default-prompt)


;; Fixes fresh-line bug, if heretical repl is not available
(defparameter *add-fresh-line-after-each-result-print* nil)


(defparameter *datapouch-readtable* (copy-readtable *readtable*))


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
(defun get-parametrized-repl-read-form (parametrized-read-form)
  (lambda (in out)
    (declare (ignore in))
    (when there-is-no-fresh-line-now
      (terpri *standard-output*)
      (setf there-is-no-fresh-line-now nil))
    (handler-case
      (multiple-value-bind (form eof new-buffer) (funcall parametrized-read-form *buffer*)
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
;; so it is likely to be broken, when this part of SBCL is updated.
(defparameter *heretical-repl-available* t)


;;; XXX: Yes, I'm gonna burn in hell for this. Don't bother complaining.
;;; XXX: Yes, I just removed everything about prompt, because readline will use it automatically.
;;;      And no, using it here is worse, because some other code changes *prompt-fun* on-the-fly,
;;;      and I feel better, when its usage is incapsulated in part of my code.
;;;      ...not this abomination.
(defun repl-fun-with-readline (noprint-global)
  (declare (special *noprint-prompt* *noprint-result*))
  ;(/show0 "entering REPL") ; TODO: Make this work, at least. It's sb-int:/show0
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


; complete function is used to generate list of possible completions for given
; partially entered word. The function must be able to take three arguments:
; partially entered word, start index of the word in *line-buffer*, and end
; index of the word in the buffer. The function must return a list where first
; element is the actual completion (or part of completion if two or more
; completions share common prefix) and the rest arguments are possible
; completions. 


;;; Assoc with strings. Each string - one possible word for autocompletion.
(defparameter *autocomplete-tree* nil)


(let ((word-separator-scanner (ppcre:create-scanner `(:greedy-repetition 1 nil (:char-class ,@+default-space-characters+)))))
  (defun autocomplete-callback (partial-word word-start-index word-end-index)
    (declare (ignore word-end-index))
    ;; XXX: Iteration 0: Just make it work, screw the guidelines
    (labels ((traverse-tree (tree path) (if (or (not (listp path)) (null path))
                                          tree
                                          (traverse-tree (rest (assoc (first path) tree :test #'equal)) (rest path)))))
      (let* ((words (map 'list
                         (lambda (element)
                           (if (listp element)
                             (first element)
                             element))
                         (traverse-tree *autocomplete-tree*
                                        (ppcre:split word-separator-scanner (subseq rl:*line-buffer* 0 word-start-index)))))
             (filtered-words (remove-if-not (lambda (candidate)
                                              (d.aux:prefix? partial-word candidate))
                                            words)))
        (if (rest filtered-words)
          (cons (d.aux:common-string-prefix filtered-words) filtered-words)
          filtered-words)))))


(defun add-command-character-to-autocomplete-tree (tree command-character)
  (map 'list (lambda (subtree)
               (cons (concatenate 'string
                                  (string command-character)
                                  (first subtree))
                     (rest subtree)))
       tree))


;;; List of callbacks.
;;;
;;; Each must take one argument:
;;; 1) String value, representing a possible command to be expanded.
;;;
;;; Each must return two values:
;;; 1) T or NIL as it's first value, indicating: was expanding successful, or wasn't,
;;; 2) And the resulting command string to be substituted, if expanding was successful.
(defparameter *expander-callbacks* nil)


(defun expander-check (partial-word word-start-index word-end-index)
  (declare (ignore word-start-index word-end-index))
  (loop :for callback :in *expander-callbacks*
        :for (success resulting-line) := (multiple-value-list (funcall callback partial-word))
        :when success
        :return (list resulting-line)
        :end))


(defun wrap-expander-callback-with-command-character (callback command-character)
  (lambda (line)
    (multiple-value-bind (success result) (funcall callback
                                                   (subseq line 1))
      (if success
        (values t (concatenate 'string
                               (string command-character)
                               result))
        (values nil nil)))))


(defun register-datapouch-autocomplete ()
  (rl:register-function :complete (lambda (partial-word word-start-index word-end-index)
                                    (setf rl:*completion-append-character* #\nul)
                                    (let ((expanded-line (expander-check partial-word
                                                                         word-start-index
                                                                         word-end-index)))
                                      (if expanded-line
                                        expanded-line
                                        (autocomplete-callback partial-word
                                                               word-start-index
                                                               word-end-index))))))
