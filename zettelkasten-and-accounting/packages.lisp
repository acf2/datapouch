;;;; packages.lisp


(in-package :cl-user)


(defpackage :zac.zettelkasten
  (:use #:cl
        #:d.sql #:d.regex #:d.cli #:d.inter #:d.main)
  (:nicknames :zac.box)
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
                :on-duplicate-key-update
                :on-conflict-do-nothing
                :on-conflict-do-update)
  (:export :+errmsg-note-is-not-chosen+
           :*current-note*
           :*note-history*
           :*option-show-note-after-jump*
           :create-zettelkasten
           :max-note-id
           :get-note-by-id
           :get-prompt
           :note-is-not-chosen
           :show-text
           :goto-text
           :edit-note
           :add-note
           :remove-note
           :show-links
           :choose-note-interactive
           :choose-link-interactive))


(defpackage :zac.bookkeeping
  (:use #:cl
        #:d.sql #:d.regex #:d.cli #:d.inter #:d.main)
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
                :on-duplicate-key-update
                :on-conflict-do-nothing
                :on-conflict-do-update)
  (:nicknames :zac.book))


(defpackage :zac.main
  (:use #:cl 
        #:d.sql #:d.regex #:d.cli #:d.inter #:d.main
        #:zac.box #:zac.book)
  (:export :make-zac))


(defpackage :zac.user
  (:use #:cl
        #:d.sql #:d.regex #:d.cli #:d.inter #:d.main
        #:zac.box #:zac.book #:zac.main)
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
                :on-duplicate-key-update
                :on-conflict-do-nothing
                :on-conflict-do-update))
