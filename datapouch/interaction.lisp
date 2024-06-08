;;;; interaction.lisp


(in-package :datapouch.interaction)


;;; Generalized dialog with user
;;; Args:
;;;   query-fun is a function with one optional argument, that prints query message for the user
;;;     At least once this function will be called with no arguments at all
;;;     Arguments:
;;;       1) Previous erroneous user input
;;;   input-handler is a function of one argument, that handles user input
;;;     Arguments:
;;;       1) User input
;;;     Must return two values:
;;;       1) Is user input accepted, or user must be queried again? (t/nil)
;;;       2) Filtered user input to be returned from dialog
;;;   prompt-fun is a function, that is used as prompt for read-form (read read-from docs)
;;;   raw-input is a flag
;;;     Essentially, it determines, will lisp reader be used on user input, or not
;;;     Values:
;;;       nil, then read-form will be used to read the form
;;;       t, then pure readline wrapper function will be used without any kind of reader
;;;
;;; NOTE: Wrap whole SBCL in continuation, put it in query-fun and base my whole application on this dialog function? Nah, too difficult. But tempting.
(defun dialog (&key ((:query-fun query-fun) nil)
                    ((:input-handler input-handler) nil)
                    ((:prompt-fun prompt-fun) *prompt-fun*)
                    ((:raw-input raw-input) nil))
  (let ((read-fun (if raw-input #'d.cli:readline #'d.cli:read-form)))
    (when query-fun
      (funcall query-fun))
    (finish-output *standard-output*)
    (loop :for (form eof) = (multiple-value-list (funcall read-fun "" prompt-fun))
          :for (is-acceptable filtered-input) = (multiple-value-list (funcall input-handler form))
          :if eof :return nil
          :else :if is-acceptable :return filtered-input
          :else :if query-fun :do
          (funcall query-fun form)
          (finish-output *standard-output*))))


(defparameter *max-string-length* 50)
(defparameter *wrap-marker* "...")


(defun wrap-string (string)
  (let* ((part-length (min (length string) (- *max-string-length* (length *wrap-marker*))))
         (space-position (position #\space string :end part-length :from-end t))
         (newline-position (position #\newline string :end part-length)))
    (if (and (null (find #\newline string :end (min (length string) *max-string-length*)))
             (<= (length string) *max-string-length*))
      string
      (concatenate 'string
                   (subseq string 0 (or newline-position
                                        space-position
                                        part-length))
                   *wrap-marker*))))


(defun slice-string-for-printing (string &optional (width 79))
  (let ((string-len (length string)))
    (loop :for last-pos     = 0 :then (+ pos 2)
          :for search-start = (min last-pos
                                   string-len)
          :for search-end   = (min (+ last-pos width)
                                   string-len)
          :for space-pos    = (position #\space string :start search-start :end search-end :from-end t)
          :for newline-pos  = (position #\newline string :start search-start :end search-end)
          :for min-pos      = (min (or space-pos string-len)
                                   (or newline-pos string-len))
          :for pos          = (if (or (= min-pos string-len)
                                      (<= (- string-len last-pos) width))
                                string-len
                                (1- min-pos))
          :while (< last-pos string-len)
          :collect (subseq string last-pos pos))))


;;; Usage:
;;; (format nil *table-metaformat* desired-field-widths) to get format string
;;; (format t format-string table) to pretty print table
;;; TODO: Right justify number columns
(defparameter *table-metaformat* "~~:{~{~~~A,4@<~~A~~>~}~~&~~}")
(defparameter *table-pad-width* 2)
(defparameter *get-table-name-delimiter* (lambda (length)
                                           (repeat-string length #\-)))


(defun find-max-field-widths (row-list)
  (map 'list (lambda (lst)
               (apply #'max lst))
       (rotate (map 'list (lambda (lst)
                            (map 'list (lambda (elem)
                                         (length (wrap-string
                                                   (if (stringp elem)
                                                     elem
                                                     (write-to-string elem)))))
                                 lst))
                    row-list))))


(defun pretty-print-rows (row-list &key ((:max-field-widths max-field-widths) (find-max-field-widths row-list)))
  (format *standard-output* (format nil *table-metaformat* (map 'list
                                                                (lambda (x) (+ x *table-pad-width*))
                                                                max-field-widths))
          (map 'list (lambda (row) (map 'list
                                        (lambda (value)
                                          (if (stringp value) (wrap-string value) value))
                                        row))
               row-list)))


(defun pretty-print-table (column-names rows)
  (let ((max-field-widths (find-max-field-widths (cons column-names rows))))
    (pretty-print-rows (cons column-names
                             (cons (map 'list (lambda (length)
                                                (funcall *get-table-name-delimiter* (min length *max-string-length*)))
                                        max-field-widths)
                                   rows))
                       :max-field-widths max-field-widths)))


(defun find-row-dialog (column-names rows &key ((:choose-many choose-many) nil)
                                               ((:prompt-msg prompt-msg) (if choose-many "Enter row numbers:~&" "Enter row number:~&"))
                                               ((:error-msg error-msg) "Please, try again.~&")
                                               ((:id-column-name-map id-map) (lambda (column-names)
                                                                               (cons "Row number" column-names)))
                                               ((:row-mapping-function row-map) (lambda (row-number row)
                                                                                  (cons row-number row)))
                                               ((:row-transformation-function row-transform) #'identity)
                                               ((:get-index get-index) nil)
                                               ((:prompt-fun prompt-fun) *prompt-fun*)
                                               ((:pretty-print-table-function pretty-print-table) #'pretty-print-table))
  (cond ((= (length rows) 0) nil)
        ((= (length rows) 1) (if get-index 0 (first rows)))
        (:else (dialog :query-fun (lambda (&optional (error-form nil error-form-supplied?))
                                    (declare (ignore error-form))
                                    (if error-form-supplied?
                                      (format *standard-output* error-msg)
                                      (progn
                                        (funcall pretty-print-table
                                                 (funcall id-map column-names)
                                                 (funcall row-transform (loop :for row :in rows
                                                                              :for i :from 1 :to (length rows)
                                                                              :collect (funcall row-map i row))))
                                        (format *standard-output* prompt-msg))))
                       :input-handler (lambda (unfiltered-input)
                                        (labels ((filter-input (input) (if choose-many
                                                                         (map 'list #'parse-integer (ppcre:split "\\D+" input))
                                                                         input))
                                                 (is-integer-accepted? (int) (and (integerp int) (<= 1 int (length rows))))
                                                 (is-input-accepted? (input) (if choose-many
                                                                               (and input (every #'is-integer-accepted? input))
                                                                               (is-integer-accepted? input)))
                                                 (index-fun (numbers) (if choose-many
                                                                        (map 'list #'1- numbers)
                                                                        (1- numbers)))
                                                 (get-rows (numbers) (if choose-many
                                                                       (map 'list
                                                                            (lambda (i) (nth i rows))
                                                                            (index-fun numbers))
                                                                       (nth (index-fun numbers) rows))))
                                          (let* ((input (filter-input unfiltered-input))
                                                 (accepted (is-input-accepted? input)))
                                            (values accepted
                                                    (when accepted
                                                      (if get-index
                                                        (index-fun input)
                                                        (get-rows input)))))))
                       :prompt-fun prompt-fun
                       :raw-input choose-many))))
