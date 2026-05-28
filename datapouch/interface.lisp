;;;; interface.lisp

(in-package :datapouch.interface)


(defgeneric concat-two (one another)
  (:documentation "Concatenate two entities"))


(declaim (ftype (function (t)) concat-many))
(defun concat-many (lst)
  (reduce #'concat-two (remove nil lst)))


(defmacro concat (&rest lst)
  `(concat-many (list ,@lst)))


(defgeneric combine-two (one another)
  (:documentation "Combine two entities"))


(declaim (ftype (function (t)) combine-many))
(defun combine-many (lst)
  (reduce #'combine-two (remove nil lst)))


(defmacro combine (&rest lst)
  `(combine-many (list ,@lst)))


(defgeneric put-into (container item &key))
(defgeneric get-from (container identifier))
