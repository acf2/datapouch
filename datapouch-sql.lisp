;;;; datapouch-sql.lisp

(in-package :d.sql)

(defun sqlite-execute (function db statement)
  (multiple-value-bind (query values) (sxql:yield statement)
    (apply function db query values)))

(macrolet ((define-sxql-wrapper (name sqlite-function)
                                `(defmacro ,name (db fields &body clauses)
                                   `(sqlite-execute #',',sqlite-function ,db
                                                    (,',(find-symbol (string name) :sxql) ,fields ,@clauses))))
           (define-sxql-wrappers (&body args)
                                 `(progn ,@(loop :for (sfun . names) :in args
                                                 :append (loop :for name :in names :collect
                                                               `(define-sxql-wrapper ,name ,sfun))))))
  (define-sxql-wrappers
    (sqlite:execute-to-list select union-queries union-all-queries)
    (sqlite:execute-non-query insert-into update delete-from create-table drop-table alter-table create-index drop-index)))
