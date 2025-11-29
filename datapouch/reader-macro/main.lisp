;;;; reader-macro/main.lisp

;;; Shallow reader-macro support


(in-package :datapouch.reader-macro)


;;; List of callbacks.
;;;
;;; Each must take one argument:
;;; 1) String value, representing a possible command to be parsed and evaluated.
;;;
;;; Each must return two values:
;;; 1) T or NIL as it's first value, indicating: was parsing successful, or not,
;;; 2) And the resulting form to be evaluated/returned from macro, if command was parsed successfully.
(defparameter *rmacro-callbacks* nil)


;;; Use this in reader macro
;;; Ref: https://stackoverflow.com/questions/18045842/appending-character-to-string-in-common-lisp
;;; Ref: https://stackoverflow.com/questions/30942815/read-input-into-string-in-lisp-reader-macro
(defun read-line-up-to (stream stop-characters)
  (let ((line (make-array 0
                          :element-type 'character
                          :fill-pointer 0
                          :adjustable t)))
    (loop :for char = (peek-char nil stream t nil t)
          :until (member char stop-characters :test #'eql)
          :do (vector-push-extend (read-char stream) line))
    (coerce line 'simple-string)))


(defun return-to-stream (string stream)
  (loop :for char :across (reverse string)
        :do (unread-char char stream)))


(defparameter *stop-characters* (list #\newline #\;))


(defun command-reader-macro (stream char)
  "Common wrapper function for all rmacro callbacks."
  (let* ((command-string (read-line-up-to stream *stop-characters*)))
    (loop :for callback :in *rmacro-callbacks*
          :for (success resulting-form) := (multiple-value-list (funcall callback command-string))
          :when success
          :do (return-from command-reader-macro resulting-form)
          :end)
    ;; If no rmacro callback has been called with success - return all chars back
    (progn
      (return-to-stream command-string stream)
      (find-symbol (string char) :cl))))


(defun install-command-reader-macro (&key ((:character character) #\/) ((:readtable table)))
  (set-macro-character character #'command-reader-macro t table))


(defun install-command-reader-autoprint-hook (&key ((:character character) #\/))
  (rl:register-hook :pre-input (lambda ()
                                 (rl:insert-text (string character))
                                 (rl:redisplay))))
