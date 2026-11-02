;;;; interaction/assoc-table.lisp


(in-package :datapouch.interaction)


(defgeneric assoc-row (key table)
  (:documentation "Get row from assoc-table based on key value."))


(defclass assoc-table ()
  ((table-rows :initarg :rows
               :type list)
   (printable-table :initarg :printable-table)
   (index-lookup :initform (make-hash-table :test #'equal))))


(defmethod initialize-instance :after ((table assoc-table) &key &allow-other-keys)
  (with-slots (index-lookup printable-table) table
    (if (and (compute-applicable-methods #'print-object (list printable-table *standard-output*))
             (compute-applicable-methods #'slice (list printable-table 0 1)))
      (with-slots (rows) printable-table
        (loop :for (key . rest) :in rows
              :for index :from 0
              :do (setf (gethash key index-lookup) index)))
      (error 'type-error 
             :datum printable-table
             :expected-type '"An object implementing PRINT-OBJECT and D.INTER:SLICE"))))


(defmethod print-object ((table assoc-table) stream)
  (with-slots (printable-table) table
    (print-object printable-table stream)))


(defmethod assoc-row (key (table assoc-table))
  (with-slots (table-rows index-lookup) table
    (let ((index (gethash key index-lookup)))
      (when index
        (nth index table-rows)))))


(defclass assoc-table-slice ()
  ((table :initarg :table
          :type assoc-table)
   (starting-row :initarg :start
                 :type number)
   (ending-row :initarg :end
               :type number)))


(defmethod slice ((table assoc-table) (start number) (end number))
  (with-slots (table-rows) table
    (make-instance 'assoc-table-slice
                   :table table
                   :start (max 0 start)
                   :end (min (length table-rows) end))))


(defmethod print-object ((table-slice assoc-table-slice) stream)
  (with-slots (table starting-row ending-row) table-slice
    (with-slots (printable-table) table
      (print-object (slice printable-table starting-row ending-row) stream))))


;; XXX: Technically, you *can* access invisible rows from the slice.
;;      Bug? Feature? Meh.
(defmethod assoc-row (key (table-slice assoc-table-slice))
  (with-slots (table) table-slice
    (with-slots (table-rows index-lookup) table
      (let ((index (gethash key index-lookup)))
        (when index
          (nth index table-rows))))))


;; This is simple, usable implementation of assoc-table
;; It uses simple printable table
;; NB: Make your own, if this one is limited.

(defun make-simple-assoc-table (column-names rows &optional printable-table)
  (make-instance 'assoc-table
                 :rows rows
                 :printable-table (or printable-table
                                      (make-simple-printable-table column-names
                                                                   rows))))
