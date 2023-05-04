;;;; packages.lisp

(in-package :cl-user)

(defpackage :datapouch.cli
  (:use #:cl)
  (:nicknames :d.cli)
  (:export :+default-line-separator+
           :+default-space-characters+
           :*buffer*
           :*no-newline*
           :*prompt-fun*
           :read-form
           :disable-bracketed-paste
           :restore-bracketed-paste
           :get-repl))

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
                :get-repl)
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

(defpackage :datapouch.regex-support
  (:use #:cl #:cl-ppcre)
  (:nicknames :d.regex)
  (:export :regex
           :wrap-in-noncapturing-group
           :make-named-group
           :concat-two
           :concat
           :combine
           :interchange
           :interchange-three
           :scan-named-groups))

(defpackage :datapouch.user
  (:use #:cl #:d.cli #:d.inter #:d.main)
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
                :create-index :drop-index)
  (:import-from :datapouch.regex-support
                :regex
                :wrap-in-noncapturing-group
                :make-named-group
                :concat-two
                :concat
                :combine
                :interchange
                :interchange-three
                :scan-named-groups))
