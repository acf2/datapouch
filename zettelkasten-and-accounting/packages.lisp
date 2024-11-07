;;;; packages.lisp


(in-package :cl-user)


(defpackage :zac.auxiliary
  (:use #:cl #:datapouch)
  (:nicknames :zac.aux)
  (:export #:expt-mod
           #:list-existing
           #:build-select
           #:make-name
           #:get-chained-table-expression
           #:get-table-power-expression))


(defpackage :zac.zettelkasten.schema
  (:use #:cl #:datapouch)
  (:nicknames :zac.box.db)
  (:export #:create-zettelkasten
           #:+table-note-fields+
           #:+table-link-fields+))


(defpackage :zac.zettelkasten.pretty-traversal
  (:use #:cl #:datapouch)
  (:nicknames :zac.box.travel)
  (:export #:build-select-notes-through-links
           #:select-notes-through-links
           #:column-names-for-notes-through-links
           #:note-path-to-string
           #:row-transformation-without-pathing
           #:row-transformation-for-pathing
           #:choose-row-from-note-through-links
           #:pretty-print-note-through-links
           #:choose-row-from-note-with-peeking))


(defpackage :zac.zettelkasten
  (:use #:cl #:datapouch)
  (:nicknames :zac.box)
  (:export
    ;; Options
    #:*option-show-note-after-jump*
    ;; Globals
    #:*current-note* #:*memorized-note*
    #:*note-history* #:*note-future*
    ;; Schema
    ;#:create-zettelkasten
    ;; Prompt
    #:get-prompt
    ;; Low level
    #:max-note-id
    #:get-note-by-id #:get-notes-by-id
    #:find-lost-notes
    #:show-note #:show-notes
    ;#:edit-notes
    ;#:add-note
    ;#:remove-note
    ;#:show-links
    ;#:choose-note-interactive
    ;#:choose-link-interactive
    ;; Main exports
    #:add-zettelkasten-commands
    #:zettelkasten-init-hook))


(defpackage :zac.bookkeeping
  (:use #:cl #:datapouch)
  (:nicknames #:zac.book))


(defpackage :zac.main
  (:use #:cl #:datapouch
        #:zac.box #:zac.book)
  (:export #:make-zac))


(defpackage :zac.user
  (:use #:cl #:datapouch
        #:zac.box #:zac.book #:zac.main))
