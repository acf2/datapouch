;;;; packages.lisp

(in-package :cl-user)

(defpackage :datapouch.cli
  (:use :cl)
  (:nicknames :d.cli)
  (:export :interactive-input
           :mainloop
           :*input*
           :prompt-function
           :prompt-list
           :command-sign
           :accumulator-sign))

(defpackage :datapouch.sql
  (:use :cl)
  (:nicknames :d.sql)
  (:export select union-queries union-all-queries
           insert-into update delete-from
           create-table drop-table alter-table
           create-index drop-index))

(defpackage datapouch.main
  (:use #:cl #:uiop)
  (:nicknames :d.main)
  (:export :main
           :*editor*
           :*editor-interface*
           :edit))

(defpackage datapouch.user
  (:use #:cl #:d.cli)
  (:import-from :sb-ext
                :quit)
  (:import-from :datapouch.sql
                select union-queries union-all-queries
                insert-into update delete-from
                create-table drop-table alter-table
                create-index drop-index)
  (:import-from :datapouch.main
                :*editor*
                :*editor-interface*
                :edit))
