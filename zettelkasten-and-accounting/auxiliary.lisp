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
