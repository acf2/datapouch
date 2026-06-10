;;;; command/reader-macro.lisp

;;; Shallow reader-macro support


(in-package :datapouch.command.reader-macro)


(defparameter *enable-execution* t)


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
  "Unreads the whole string character by character."
  (loop :for char :across (reverse string)
        :do (unread-char char stream)))


;; Stop characters for command-reader-macro.
;; If you want commands to contain these symbols, then change it.
(defparameter *stop-characters* (list #\newline #\;))


(defun command-reader-macro (stream char)
  "Common wrapper function for all rmacro callbacks. If callback call isn't
successful, returns all characters back, beside reader macro character."
  (let* ((command-string (read-line-up-to stream *stop-characters*)))
    (loop :for callback :in *rmacro-callbacks*
          :for (success resulting-form) := (multiple-value-list (funcall callback command-string))
          :when success
          :do (progn
                (when d.aux:*debug*
                  (format *standard-output* "RMACRO CALLBACK RESULT:~&~S~&" resulting-form))
                (return-from command-reader-macro (if *enable-execution*
                                                    resulting-form
                                                    nil)))
          :end)
    ;; If no rmacro callback has been called with success, then return all chars back.
    (progn
      (return-to-stream command-string stream)
      (find-symbol (string char) :cl))))


(defparameter *control-character* #\/)


(defun install-command-reader-macro (&key ((:character character) *control-character*) ((:readtable table)))
  "Adds reader macro to readtable. Default is *control-character*."
  (set-macro-character character #'command-reader-macro t table))


(defun install-command-reader-autoprint-hook (&key ((:character character) #\/))
  "Adds cl-readline pre-input hook for entering specified character, making it
easier to input commands. Default is #\/ (slash)."
  (rl:register-hook :pre-input (lambda ()
                                 (rl:insert-text (string character))
                                 (rl:redisplay))))
