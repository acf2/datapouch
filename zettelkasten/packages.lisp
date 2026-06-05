;;;; packages.lisp


(in-package :cl-user)


(defpackage :zettelkasten.schema
  (:use #:cl #:datapouch)
  (:nicknames :zk.db)
  (:export #:create-zettelkasten
           #:+table-note-fields+
           #:+table-link-fields+))


(defpackage :zettelkasten.pretty-traversal
  (:use #:cl #:datapouch)
  (:nicknames :zk.travel)
  (:export #:build-select-notes-through-links
           #:select-notes-through-links
           #:column-names-for-notes-through-links
           #:note-path-to-string
           #:row-transformation-without-pathing
           #:row-transformation-for-pathing
           #:choose-row-from-note-through-links
           #:pretty-print-note-through-links
           #:choose-row-from-note-with-peeking))


(defpackage :zettelkasten
  (:use #:cl #:datapouch)
  (:nicknames :zk)
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
    #:get-zettelkasten-yields
    #:zettelkasten-init-hook))


(defpackage :zettelkasten.plugin
  (:use #:cl #:datapouch
        #:zk)
  (:nicknames :zk.plugin)
  (:export #:make-plugin))
