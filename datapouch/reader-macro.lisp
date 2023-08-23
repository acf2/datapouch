;;;; reader-macro.lisp


(in-package :datapouch.reader-macro)


;; list of pairs:
;; (command-regex . command-handler)
;; Command regex can be an object of either regex or regex-scanner classes
;; Command handler must be a function and have at least two arguments:
;;   1. String argument, for full command string
;;   2. List of conses (string . string), for list of named regex matches
(defparameter *commands* nil)


(defun command-reader-macro (stream char)
  (let* ((command (read-line-to-semicolon-or-newline stream))
         (command-bundle (loop for (command-regex . command-handler) in *commands*
                               for match = (scan-named-groups command-regex command)
                               when match
                               return (list match command-handler))))
    (if command-bundle
      `(funcall ,(second command-bundle) ,command ',(first command-bundle))
      (progn
        (loop for command-char across (reverse command)
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
