;;;; zettelkasten/vars.lisp


(in-package :zac.box)


(defparameter *current-note* nil)

;; History
(defparameter *note-history* nil) ; TBD
(defparameter *note-future* nil) ; TBD

(defparameter *option-show-note-after-jump* t)

(defparameter *order-by-text* nil)


(defparameter +msg-note-is-not-chosen+ "~%Note is not chosen.~&")
(defparameter +msg-no-notes+ "There is no notes meeting this criteria.~&")
(defparameter +msg-abort-note-creation+ "~%Aborted.~&")

(defparameter +errmsg-no-note-found+ "~%No such note found.~&")

(defparameter +interrmsg-cannot-find-id+ "~%INTERNAL: No note with such ID~&")

