;;;; docform.lisp


(in-package :datapouch.command.pattern)


(defclass docform ()
  ((doc-expr :initarg :doc-expr
             :type list
             :reader doc-expr)))


(defmethod make-optional ((docform docform))
  (make-instance 'docform :doc-expr (list :optional (doc-expr docform))))


(defmethod concat-two ((one docform) (another docform))
  (flet ((take-sequence (obj) (if (and (listp obj)
                                       (eq (first obj) :sequence))
                                (rest obj)
                                (list obj))))
    (with-slots ((one-expr doc-expr)) one
      (with-slots ((another-expr doc-expr)) another
        (make-instance 'docform
                       :doc-expr (cons :sequence
                                       (append 
                                         (take-sequence one-expr)
                                         (take-sequence another-expr))))))))


(defmethod combine-two ((one docform) (another docform))
  (flet ((take-alternation (obj) (if (and (listp obj)
                                          (eq (first obj) :alternation))
                                   (rest obj)
                                   (list obj))))
    (with-slots ((one-expr doc-expr)) one
      (with-slots ((another-expr doc-expr)) another
        (make-instance 'docform
                       :doc-expr (cons :alternation
                                       (append 
                                         (take-alternation one-expr)
                                         (take-alternation another-expr))))))))


(declaim (ftype (function (string))
                make-docform))
(defun make-docform (string)
  (make-instance 'docform
                 :doc-expr string))


(defmethod make-named-group ((name string) (docform docform) &optional info)
  (declare (ignore info))
  (make-instance 'docform
                 :doc-expr (list :named-group name (doc-expr docform))))


(defun default-doc-expr-finalizer (doc-expr &optional (enum-type nil))
  (cond ((not (listp doc-expr))
         doc-expr)
        ((eq (first doc-expr)
             :named-group)
         (format nil "<~A:~A>"
                 (second doc-expr)
                 (default-doc-expr-finalizer (third doc-expr))))
        ((eq (first doc-expr)
             :optional)
         (format nil "[~A]"
                 (default-doc-expr-finalizer (second doc-expr))))
        ((eq (first doc-expr)
             :sequence)
         (format nil
                 "~:[~;(~]~{~A~}~0@*~:[~;)~]"
                 (member enum-type (list :alternation))
                 (map 'list (lambda (subexpr)
                              (default-doc-expr-finalizer subexpr :sequence))
                      (rest doc-expr))))
        ((eq (first doc-expr)
             :alternation)
         (format nil
                 "~:[~;(~]~{~#[~;~A~:;~A | ~]~}~0@*~:[~;)~]"
                 (member enum-type (list :sequence))
                 (delete-duplicates
                   (map 'list (lambda (subexpr)
                                (default-doc-expr-finalizer subexpr :alternation))
                        (rest doc-expr))
                   :test #'string=)))))


(defparameter *doc-expr-finalizer* #'default-doc-expr-finalizer)
