;;;; auxiliary.lisp

(in-package :datapouch.auxiliary)


(defun list-existing (&rest rest)
  (remove nil rest))


(defmacro list-existing* (&rest rest)
  `(remove nil (list* ,@rest)))


(defmacro append-lists (list-of-lists)
  `(reduce #'append ,list-of-lists))


;;; Transpose lists
;;; https://stackoverflow.com/a/3513158
(defun rotate (list-of-lists)
  (if (or (null list-of-lists)
          (and (null (car list-of-lists))
               (null (cdr list-of-lists))))
    list-of-lists
    (apply #'map 'list #'list list-of-lists)))


(defmacro repeat-string (times str)
  `(format nil "~V@{~A~:*~}" ,times ,str))
