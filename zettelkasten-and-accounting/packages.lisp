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
           #:pretty-print-note-through-links))


(defpackage :zac.zettelkasten
  (:use #:cl #:datapouch)
  (:nicknames :zac.box)
  (:export #:+errmsg-note-is-not-chosen+
           #:*current-note* #:*memorized-note*
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
           #:get-zettelkasten-commands
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
