;;;; reader-macro.lisp


(in-package :datapouch.reader-macro)


;; Command regex can be an object of either d.regex:regex or
;; d.regex:regex-scanner class (or analogous one, that implements scan
;; mechanic). If group mode is enabled (default), command-handler will get the
;; group tree from regex-support. If group mode is disabled, command-handler
;; will get group assoc. Additionally, if full-string-is-needed is set, then
;; the first position argument will be a full match string.
(defclass command ()
  ((command-regex :initarg :regex
                  :reader command-regex)
   (command-handler :initarg :handler
                    :reader command-handler)
   (group-mode :initarg :group-mode
               :initform t
               :reader group-mode)
   (full-string-is-needed :initarg :full-string
                          :initform nil
                          :reader full-string)))


;; list of command objects
(defparameter *commands* nil)


;;; Use this in reader macro
;;; Ref: https://stackoverflow.com/questions/18045842/appending-character-to-string-in-common-lisp
;;; Ref: https://stackoverflow.com/questions/30942815/read-input-into-string-in-lisp-reader-macro
(defun read-line-to-semicolon-or-newline (stream)
  (let ((line (make-array 0
                          :element-type 'character
                          :fill-pointer 0
                          :adjustable t)))
    (loop :for char = (peek-char nil stream t nil t)
          :until (or (eql char #\newline)
                     (eql char #\;))
          :do (vector-push-extend (read-char stream) line))
    (coerce line 'simple-string)))


(defun return-to-stream (string stream)
  (loop :for char :across (reverse string)
        :do (unread-char char stream)))


(defun command-reader-macro (stream char)
  (let* ((command-string (read-line-to-semicolon-or-newline stream))
         (command-bundle (loop :for command :in *commands*
                               :for groups := (groups (command-regex command))
                               :for (match-start match-end group-starts group-ends) = (multiple-value-list (scan (command-regex command) command-string))
                               :when match-start
                               :return (list command (funcall (if (group-mode command)
                                                                #'match-to-group-tree
                                                                #'match-to-assoc)
                                                              command-string
                                                              groups
                                                              match-start
                                                              match-end
                                                              group-starts
                                                              group-ends))))
         (command (first command-bundle))
         (match (second command-bundle)))
    (if command
      ;; well, yes, it could've been done more flexible to cover cases with need for in-reader computations
      ;; but i don care. it's hard to juggle characters with read/unread, when error handling arises
      (if (full-string command)
        `(funcall ,(command-handler command) ,command-string ',match) 
        `(funcall ,(command-handler command) ',match))
      (progn
        (return-to-stream command-string stream)
        (find-symbol (string char) :cl)))))


;; DEPRECATED
;(defun old-command-reader-macro (stream char)
;  (let* ((command-string (read-line-to-semicolon-or-newline stream))
;         (command-bundle (loop :for command :in *commands*
;                               :for (match groups) = (multiple-value-list (scan-named-groups (command-regex command) command-string))
;                               :when match
;                               :return (list command groups)))
;         (command (first command-bundle))
;         (match (second command-bundle)))
;    (if command
;      ;; well, yes, it could've been done more flexible to cover cases with need for in-reader computations
;      ;; but i don care. it's hard to juggle characters with read/unread, when error handling arises
;      `(funcall ,(command-handler command) ,command-string ',match) 
;      (progn
;        (return-to-stream command-string stream)
;        (find-symbol (string char) :cl)))))


(defun install-command-reader-macro (&key ((:character character) #\/) ((:readtable table) *custom-readtable*))
  (set-macro-character character #'command-reader-macro t table))
