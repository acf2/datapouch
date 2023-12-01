;;;; packages.lisp


(in-package :cl-user)


(defpackage :zac.command-wrapper
  (:use #:cl #:datapouch)
  (:nicknames :zac.cmd)
  (:export #:command-wrapper
           #:add-command-form
           #:make-command-wrapper
           #:make-commands-from-wrappers))


(defpackage :zac.zettelkasten
  (:use #:cl #:datapouch)
  (:nicknames :zac.box)
  (:export #:+errmsg-note-is-not-chosen+
           #:*current-note*
           #:*note-history*
           #:*option-show-note-after-jump*
           #:create-zettelkasten
           #:max-note-id
           #:get-note-by-id
           #:get-prompt
           #:note-is-not-chosen
           #:show-text
           #:goto-text
           #:edit-note
           #:add-note
           #:remove-note
           #:show-links
           #:choose-note-interactive
           #:choose-link-interactive
           #:add-zettelkasten-commands))


(defpackage :zac.bookkeeping
  (:use #:cl #:datapouch)
  (:nicknames #:zac.book))


(defpackage :zac.main
  (:use #:cl #:datapouch
        #:zac.box #:zac.book)
  (:export #:make-zac))


(defpackage :zac.user
  (:use #:cl #:datapouch
        #:zac.cmd #:zac.box #:zac.book #:zac.main))
