;;;; reader-macro.lisp


(in-package :datapouch.reader-macro)


;; Command regex can be an object of either regex or regex-scanner classes
;; Command handler must be a function and have at least two arguments:
;;   1. String argument, for full command string
;;   2. List of conses (string . string), for list of named regex matches
(defclass command ()
  ((command-regex :initarg :regex
                  :reader command-regex)
   (command-handler :initarg :handler
                    :reader command-handler)))


;; list of command objects
(defparameter *commands* nil)


(defun command-reader-macro (stream char)
  (let* ((command-string (read-line-to-semicolon-or-newline stream))
         (command-bundle (loop for command in *commands*
                               for (match groups) = (multiple-value-list (scan-named-groups (command-regex command) command-string))
                               when match
                               return (list command groups)))
         (command (when command-bundle (first command-bundle)))
         (match (when command-bundle (second command-bundle))))
    (if command
      `(funcall ,(command-handler command) ,command-string ',match)
      (progn
        (loop for command-char across (reverse command-string)
              do (unread-char command-char stream))
        (find-symbol (string char) :cl)))))


(defun install-command-reader-macro (&key ((:character character) #\/) ((:readtable table) *custom-readtable*))
  (set-macro-character character #'command-reader-macro nil table))


;;; Use this in reader macro
;;; Ref: https://stackoverflow.com/questions/18045842/appending-character-to-string-in-common-lisp
;;; Ref: https://stackoverflow.com/questions/30942815/read-input-into-string-in-lisp-reader-macro
(defun read-line-to-semicolon-or-newline (stream)
  (let ((line (make-array 0
                          :element-type 'character
                          :fill-pointer 0
                          :adjustable t)))
    (loop for char = (peek-char nil stream t nil t)
          until (or (eql char #\newline)
                    (eql char #\;))
          do (vector-push-extend (read-char stream) line))
    (coerce line 'simple-string)))
