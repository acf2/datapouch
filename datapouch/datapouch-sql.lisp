;;;; datapouch-sql.lisp

(in-package :datapouch.sql)

(defparameter *db* nil)

(defun sqlite-execute (function statement)
  (multiple-value-bind (query values) (sxql:yield statement)
    (apply function *db* query values)))

(macrolet ((define-sxql-wrapper (name sqlite-function)
                                `(defmacro ,name (fields &body clauses)
                                   `(sqlite-execute #',',sqlite-function
                                                    (,',(find-symbol (string name) :sxql) ,fields ,@clauses))))
           (define-sxql-wrappers (&body args)
                                 `(progn ,@(loop :for (sfun . names) :in args
                                                 :append (loop :for name :in names :collect
                                                               `(define-sxql-wrapper ,name ,sfun))))))
  (define-sxql-wrappers
    (sqlite:execute-to-list select union-queries union-all-queries insert-into update delete-from)
    (sqlite:execute-non-query create-table drop-table alter-table create-index drop-index)))

(defmacro with-binded-db (db-symbol &body body)
  `(let ((*db* ,db-symbol))
     ;(declare (special *db*))
     (sqlite:execute-non-query *db* "PRAGMA foreign_keys=ON;") ; XXX: Because FUCK YOU foreign key default support
     ,@body))
