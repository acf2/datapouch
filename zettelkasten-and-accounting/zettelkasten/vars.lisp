;;;; zettelkasten/vars.lisp


(in-package :zac.box)


(defparameter *choose-note-prompt* (constantly "note> "))
(defparameter *choose-link-prompt* (constantly "link> "))


(defparameter *current-note* nil)
(defparameter *memorized-notes* nil) ; The first one is the target

(defparameter *note-history* nil)
(defparameter *note-future* nil)

(defparameter *option-show-note-after-jump* t)

(defparameter *order-by-text* nil)


(defparameter +question-note-with-number-exists+ (format nil "Link with such number exist~%Override?"))

(defparameter +msg-note-is-not-chosen+ "Note is not chosen.")
(defparameter +msg-notes-are-not-chosen+ "Not a single note is not chosen.")
(defparameter +msg-no-notes+ "There is no notes meeting this criteria.")
(defparameter +msg-abort-note-creation+ "Aborted.")
(defparameter +msg-abort-note-deletion+ "Aborted.")
(defparameter +msg-link-exists+ "Link already exists.")

(defparameter +errfmt-generic-sqlite-error+ "~A~&~%Operation aborted.~&")

(defparameter +intermsg-cannot-find-id+ "~%INTERNAL: No note with such ID~&")

