;;;; auxiliary.lisp


(in-package :zac.auxiliary)


(defun list-existing (&rest rest)
  (remove nil rest))


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

