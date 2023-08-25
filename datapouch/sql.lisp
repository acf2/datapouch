;;;; sql.lisp


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


(defun use-foreign-keys (flag)
  (sqlite:execute-non-query *db* (format nil "PRAGMA foreign_keys=~A;" (if flag :on :off))))


(defun integrity-check (&key (fast nil))
  (let* ((check (if fast "integrity_check" "quick_check")))
    (handler-case (values t (first (first (sqlite:execute-to-list *db* (format nil "PRAGMA ~A;" check)))))
      (error (e) (values nil e)))))