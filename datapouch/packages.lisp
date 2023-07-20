;;;; packages.lisp


(in-package :cl-user)


(defpackage :datapouch.regex-support
  (:use #:cl #:cl-ppcre)
  (:nicknames :d.regex)
  (:export :allow-named-registers
           :regex :expr :groups
           :wrap-in-noncapturing-group
           :make-named-group
           :concat-two
           :concat
           :combine
           :interchange
           :interchange-three
           :scan-named-groups
           :regex-scanner :scanner
           :make-scanner))


(defpackage :datapouch.cli
  (:use #:cl #:d.regex)
  (:nicknames :d.cli)
  (:export :+default-line-separator+
           :+default-space-characters+
           :*buffer*
           :*no-newline*
           :*prompt-fun*
           :*command-table*
           :read-form
           :disable-bracketed-paste
           :restore-bracketed-paste
           :get-repl
           :command-reader-macro
           :read-line-to-semicolon-or-newline))


(defpackage :datapouch.sql
  (:use #:cl)
  (:nicknames :d.sql)
  (:export :*db*
           :select :union-queries :union-all-queries
           :insert-into :update :delete-from
           :create-table :drop-table :alter-table
           :create-index :drop-index))


(defpackage :datapouch.main
  (:use #:cl #:uiop)
  (:nicknames :d.main)
  (:import-from :d.sql
                :*db*)
  (:import-from :d.cli
                :*buffer*
                :*no-newline*
                :disable-bracketed-paste
                :restore-bracketed-paste
                :get-repl
                :command-reader-macro
                :read-line-to-semicolon-or-newline)
  (:export :*database-path*
           :*history-path*
           :*init-hooks*
           :*exit-hooks*
           :*debugger-hooks*
           :*editor-interface*
           :edit-strings
           :make-image))


(defpackage :datapouch.interaction
  (:use #:cl)
  (:nicknames :d.inter)
  (:import-from :d.cli
                :read-form
                :*prompt-fun*)
  (:export :*max-string-length*
           :*wrap-marker*
           :*table-metaformat*
           :*table-pad-width*
           :*get-table-name-delimiter*
           :find-max-field-widths
           :pretty-print-rows
           :pretty-print-table
           :find-one-row-dialog))


;;; Useful package for user code and/or interactive use
;;; You can copy this package, if you want to use userenvironment
;;; XXX: It's kinda crowded right now, but without reimport I cannot really do anything about sxql clashing with d.sql
;;;      So... let's just leave it alone. I don't like unnecessary dependencies.
(defpackage :datapouch.user
  (:use #:cl #:d.regex #:d.cli #:d.inter #:d.main)
  (:nicknames :d.user)
  (:import-from :sb-ext
                :quit)
  (:import-from :sxql
                :fields
                :from
                :where
                :order-by
                :group-by
                :having
                :returning
                :limit
                :offset
                :set=
                :inner-join :left-join :right-join :full-join
                :primary-key
                :unique-key
                :index-key
                :foreign-key
                :add-column
                ;:modify-column ;; double check if they work in sqlite
                ;:alter-column
                ;:change-column
                ;:drop-column
                ;:add-primary-key
                ;:drop-primary-key
                ;:rename-to
                :on-duplicate-key-update
                :on-conflict-do-nothing
                :on-conflict-do-update)
  (:import-from :datapouch.sql
                :select :union-queries :union-all-queries
                :insert-into :update :delete-from
                :create-table :drop-table :alter-table
                :create-index :drop-index))
