;;;; auxiliary.lisp


(in-package :zac.auxiliary)


;;; Got from here https://www.cliki.net/expt-mod
(defun expt-mod (n exponent modulus)
  "As (mod (expt n exponent) modulus), but more efficient."
  ;; When you have to tackle it, uncomment
  ;(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (declare (optimize (safety 0) (space 0) (debug 0)))
  (loop with result = 1
        for i of-type fixnum from 0 below (integer-length exponent)
        for sqr = n then (mod (* sqr sqr) modulus)
        when (logbitp i exponent) do
        (setf result (mod (* result sqr) modulus))
        finally (return result)))


;;; TODO
;;; File an issue or merge request to https://github.com/fukamachi/sxql
;;; Why tuple syntax is not supported? Maybe I'm missing something? Sqlite3 seems to support it
;;; Like here https://stackoverflow.com/a/7865437
;;;
;;; Very OUCH. Don't do that, kids, or you will code with C++ for the rest of your life.
;;; I will feel very stupid, if this is supported somehow.
;;;
;;; This wrapper works only because sxql can handle multiple where clauses in query.
;;; Pure SQL cannot, if I recall correctly.
(defmacro wrap-where-tuple-in (sql-fun sxql-name fields tuple tuple-list &rest clauses)
  (if (not (and tuple tuple-list))
    `((find-symbol (string sxql-name) :d.sql) ,fields ,@clauses)
    (let ((marker "CTHULHU FHTAGN") ; There is chance of collision still. Maybe...
          (tuple-var (gensym)) (tuple-list-var (gensym))
          (query-string (gensym)) (parameters (gensym))
          (parameter-position (gensym)) (in-query-postion (gensym))
          (counter (gensym)) (last-position (gensym)) (new-position (gensym))
          (element (gensym))
          (sxql-fun (find-symbol (string sxql-name) :sxql)))
      `(let ((,tuple-var ,tuple)
             (,tuple-list-var ,tuple-list))
         (declare (type list ,tuple-var ,tuple-list-var))
         (multiple-value-bind (,query-string ,parameters) (sxql:yield (,sxql-fun ,fields ,@clauses (where ,marker)))
           (let* ((,parameter-position (position ,marker ,parameters :test #'equal))
                  (,in-query-postion (loop :for ,counter = 0 :then (1+ ,counter)
                                           :for ,last-position = 0 :then ,new-position
                                           :for ,new-position = (position #\? ,query-string :start (1+ ,last-position))
                                           :when (= ,counter ,parameter-position) :return ,new-position)))
             (if (every (lambda (,element)
                          (= (length ,tuple-var)
                             (length ,element)))
                        ,tuple-list-var)
               (apply ,sql-fun *db*
                     (concatenate 'string
                                  (subseq ,query-string 0 ,in-query-postion)
                                  (format nil "((~{~(~#[~;~A~:;~A, ~]~)~}) IN (~{(~{~#[~;?~*~:;?~*, ~]~})~#[~:;, ~]~}))" ,tuple-var ,tuple-list-var)
                                  (subseq ,query-string (1+ ,in-query-postion)))
                     (concatenate 'list
                                  (subseq ,parameters 0 ,parameter-position)
                                  (reduce #'append ,tuple-list-var)
                                  (subseq ,parameters (1+ ,parameter-position))))
               (error "wrap-where-tuple-in: not all data tuples are equal in size to parameter tuple"))))))))
