;;;; auxiliary.lisp


(in-package :zac.auxiliary)


;;; Got from here https://www.cliki.net/expt-mod
(defun expt-mod (n exponent modulus)
  "As (mod (expt n exponent) modulus), but more efficient."
  ;; When you have to tackle it, uncomment
  ;(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (declare (optimize (safety 0) (space 0) (debug 0)))
  (loop :with result := 1
        :for i :of-type fixnum :from 0 :below (integer-length exponent)
        :for sqr := n :then (mod (* sqr sqr) modulus)
        :when (logbitp i exponent) :do
        (setf result (mod (* result sqr) modulus))
        :finally (return result)))


(defun make-name (&key ((:table table) nil) ((:index index) nil) ((:column column) nil))
  (intern (concatenate 'string
                       (when table (symbol-name table))
                       (when index (write-to-string index))
                       (when column (concatenate 'string "." (symbol-name column))))
          "KEYWORD"))


;;; Kinda bad interface, if you ask me
;;; starting and ending are lists of 2 things
;;; Like (table-name column-name)
(defun get-repeated-join-clause (table depth source-column destination-column
                                       &key
                                       ((:starting-table-and-column starting) nil)
                                       ((:starting-table-alias starting-alias) nil)
                                       ((:ending-table-and-column ending) nil)
                                       ((:ending-table-alias ending-alias) nil))
  (declare (type keyword table source-column destination-column)
           (type integer depth)
           (type (or null list) starting ending)
           (type (or null keyword) starting-alias ending-alias))
  (let ((starting-table (first starting))
        (starting-column (second starting))
        (ending-table (first ending))
        (ending-column (second ending)))
    (inner-join (if ending (if ending-alias
                             (sxql:make-op :as ending-table ending-alias)
                             ending-table)
                  (sxql:make-op :as table (make-name :table table
                                                     :index depth)))
                :on (:= (if starting (make-name :table (or starting-alias starting-table)
                                                :column starting-column)
                          (make-name :table table
                                     :index (1- depth)
                                     :column destination-column))
                        (if ending (make-name :table (or ending-alias ending-table)
                                              :column ending-column)
                          (make-name :table table
                                     :index depth
                                     :column source-column))))))


(defun get-chained-table-expression (chain-length starting-table starting-column
                                                  link-table link-source-column link-destination-column
                                                  ending-table ending-column
                                                  &key
                                                  ((:starting-table-alias starting-alias) nil)
                                                  ((:ending-table-alias ending-alias) nil))
  (list* (from (if starting-alias
                 (sxql:make-op :as starting-table starting-alias)
                 starting-table))
         (loop :for depth :from 0 :to chain-length
               :collect (get-repeated-join-clause link-table depth link-source-column link-destination-column
                                                  :starting-table-and-column (when (= depth 0)
                                                                               (list starting-table starting-column))
                                                  :starting-table-alias starting-alias
                                                  :ending-table-and-column (when (= depth chain-length)
                                                                             (list ending-table ending-column))
                                                  :ending-table-alias ending-alias))))


(defun get-table-power-expression (chain-length table source-column destination-column
                                                &key
                                                ((:starting-table-alias starting-alias) nil)
                                                ((:ending-table-alias ending-alias) nil))
  (get-chained-table-expression (1- chain-length)
                                table destination-column
                                table source-column destination-column
                                table source-column
                                :starting-table-alias starting-alias
                                :ending-table-alias ending-alias))


;;; Use this from macros
;;; It's assumed that table0 and tableN are going to be aliased by the user
;(defun join-chain (table length base-field target-field &key ((:join-kind join-kind) :inner))
;  (loop :for i :from 1 :to length
;        :collect (sxql:make-clause :join (sxql:expand-op (list :as table (make-name :table table
;                                                                                    :index i)))
;                                   :kind kind
;                                   :on (expand-op (list :=
;                                                        (make-name :table table
;                                                                   :index (1- i)
;                                                                   :column base-field)
;                                                        (make-name :table table
;                                                                   :index i
;                                                                   :column target-field))))))

