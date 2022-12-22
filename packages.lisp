;;;; packages.lisp

(in-package :cl-user)

(defpackage :datapouch.cli
  (:use :cl)
  (:nicknames :d.cli)
  (:export :mainloop
           :interactive-input :get-prompt :prompt-list :command-sign :accumulator-sign
           :read-form))

(defpackage :datapouch.sql
  (:use :cl)
  (:nicknames :d.sql)
  (:export :*db* :with-binded-db
           :select :union-queries :union-all-queries
           :insert-into :update :delete-from
           :create-table :drop-table :alter-table
           :create-index :drop-index))

(defpackage :datapouch.main
  (:use #:cl #:uiop)
  (:nicknames :d.main)
  (:export :main
           :*editor-interface*
           :*print-output*
           :*input*
           :edit-strings))

(defpackage :datapouch.interaction
  (:use #:cl #:d.cli #:d.main)
  (:nicknames :d.inter)
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
