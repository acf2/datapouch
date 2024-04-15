;;;; sql.lisp


(in-package :datapouch.sql)


(defparameter *db* nil)


(defun open-db (path)
  (when path
    (setf *db* (sqlite:connect *database-path*))))


(defun close-db ()
  (when *db*
    (sqlite:disconnect *db*)))


(defun execute (function statement)
  (multiple-value-bind (query values) (sxql:yield statement)
    (apply function *db* query values)))


(defun query (statement)
  (execute #'sqlite:execute-to-list statement))


;;; XXX: Why have you used <fun-name>-queries for API, and yet just <fun-name> for make-op?
;;;      Insanity.
(defparameter +unions+ (list (cons :union-queries :union)
                             (cons :union-all-queries :union-all)))

(defparameter +statements+ (list :insert-into
                                 :update
                                 :delete-from
                                 :create-table
                                 :drop-table
                                 :alter-table
                                 :create-index
                                 :drop-index))


(defun build (entity &rest arguments)
  (let ((union-fun (rest (assoc entity +unions+))))
    (cond (union-fun
            (apply #'sxql:make-op
                   union-fun
                   (list-existing* arguments)))
          ((eq entity :select)
           (let ((fields (first arguments))
                 (clauses (rest arguments)))
             (apply #'sxql:make-statement
                    entity
                    (apply #'sxql:make-clause :fields (list-existing* fields))
                    (list-existing* clauses))))
          ((member entity +statements+)
             (apply #'sxql:make-statement
                    entity
                    (list-existing* arguments)))
          (:else (apply #'sxql:make-clause
                        entity
                        (list-existing* arguments))))))


(defun build-and-query (statement &rest arguments)
  (execute #'sqlite:execute-to-list (apply #'build statement arguments)))


;;; File an issue or merge request to https://github.com/fukamachi/sxql
;;; Why tuple syntax is not supported? Maybe I'm missing something? Sqlite3 seems to support it
;;; Like here https://stackoverflow.com/a/7865437
;;;
;;; Very EZ, very kludgy
(defun column-tuple (column-names)
  (list :raw (sxql:yield (apply #'sxql:make-clause :fields column-names))))


;; Union does not work for sqlite3, i dunno why
;; Just like this:
;;  Code ERROR: near "(": syntax error.
;;  SQL: (SELECT * FROM note) UNION (SELECT * FROM note)
;;
;; MR: https://github.com/fukamachi/sxql/pull/77
;; It won't be accepted for the next bajillion years
;; And for the second bajillion after that, new code won't be uploaded to quicklisp
;; So no, I woke up today and chose violence: we gotta make a kludge, babe.
(cl-package-locks:with-packages-unlocked
  (:sxql.operator)
  (macrolet ((new-yield-for-union-ops (sxql.operator::keyword)
                                      `(multiple-value-bind (sxql.operator::statements sxql.operator::others)
                                         (loop :for obj :in (sxql.operator::conjunctive-op-expressions sxql.operator::op)
                                               :if (sxql.operator::sql-statement-p obj)
                                               :collect obj :into sxql.operator::statements
                                               :else
                                               :collect obj :into sxql.operator::others
                                               :finally (return (values sxql.operator::statements sxql.operator::others)))
                                         (sxql.operator::with-yield-binds
                                           (format nil (if sxql.operator::*inside-select*
                                                         "(~{~A~^ ~})"
                                                         "~{~A~^ ~}")
                                                   (append (list (format nil ,(format nil "~~{~~A~~^ ~a ~~}" sxql.operator::keyword)
                                                                         (mapcar #'sxql.operator::yield sxql.operator::statements)))
                                                           (when sxql.operator::others
                                                             (list (format nil "~{~A~^ ~}"
                                                                           (mapcar #'sxql.operator::yield sxql.operator::others))))))))))
    (defmethod sxql.operator::yield ((sxql.operator::op sxql.operator::union-op))
      (new-yield-for-union-ops "UNION"))
    (defmethod sxql.operator::yield ((sxql.operator::op sxql.operator::union-all-op))
      (new-yield-for-union-ops "UNION ALL"))))


(macrolet ((define-sxql-wrapper (name sqlite-function)
                                `(defmacro ,name (fields &body clauses)
                                   `(execute #',',sqlite-function
                                                    (,',(find-symbol (string name) :sxql) ,fields ,@clauses))))
           (define-sxql-union-wrapper (name sqlite-function)
                                      `(defmacro ,name (&body queries)
                                         `(execute #',',sqlite-function
                                                          (,',(find-symbol (string name) :sxql)
                                                            ,@(loop :for query :in queries
                                                                    :collect `(,(find-symbol (string (first query)) :sxql) ,@(rest query)))))))
           (define-sxql-wrappers (&body args)
                                 (let ((union-sql-functions (list 'union-queries 'union-all-queries)))
                                 `(progn ,@(loop :for (sfun . names) :in args
                                                 :append (loop :for name :in names
                                                               :collect (if (member name union-sql-functions)
                                                                          `(define-sxql-union-wrapper ,name ,sfun)
                                                                          `(define-sxql-wrapper ,name ,sfun))))))))
  (define-sxql-wrappers
    (sqlite:execute-to-list select union-queries union-all-queries insert-into update delete-from)
    (sqlite:execute-non-query create-table drop-table alter-table create-index drop-index)))


(defun use-foreign-keys (flag)
  (sqlite:execute-non-query *db* (format nil "PRAGMA foreign_keys=~A;" (if flag :on :off))))


(defun check-integrity (&key (fast nil))
  (let* ((check (if fast "integrity_check" "quick_check")))
    (handler-case (values t (first (first (sqlite:execute-to-list *db* (format nil "PRAGMA ~A;" check)))))
      (error (e) (values nil e)))))
