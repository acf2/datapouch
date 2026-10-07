;;;; interaction/basic.lisp


(in-package :datapouch.interaction)


;;; NOTE: Wrap whole SBCL in continuation, put it in query-fun and base my
;;;       whole application on this dialog function?
;;;       Nah, too difficult. But tempting.
(defun dialog (&key ((:query-fun query-fun) nil)
                    ((:input-handler input-handler) nil)
                    ((:prompt-fun prompt-fun) *prompt-fun*)
                    ((:raw-input raw-input) nil))
  "Generalized dialog with user.
Arguments:
 QUERY-FUN is a function with one optional argument, that prints query message
 for the user.
   At least once this function will be called with no arguments at all
   Arguments:
     1) Previous erroneous user input.
 INPUT-HANDLER is a function of one argument, that handles user input
   Arguments:
     1) User input.
   Must return two values:
     1) Is user input accepted, or not and user must be queried again? (T/NIL),
     2) Filtered user input to be returned from dialog.
 PROMPT-FUN is a function, that is used as prompt for READ-FORM or readline.
 RAW-INPUT is a flag
   Essentially, it determines, will lisp reader be used on user input, or not
   Values:
     NIL, then READ-FORM will be used to read the form
     T, then pure readline wrapper function will be used without any kind of reader"
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


(defgeneric slice (table start end)
  (:documentation "Take a slice from the table in question"))


(defclass printable-table ()
  ((column-names :initarg :columns
                 :type list-of-strings)
   (rows :initarg :rows
         :type list-of-list-of-string)
   (fields-lengths :type integer
                   :reader fields-lengths)
   (pretty-print-fun :initarg :print-fun
                     :type function)))


(defmethod initialize-instance :after ((table printable-table) &key &allow-other-keys)
  (with-slots (column-names rows fields-lengths) table
    (setf fields-lengths (mapcar (lambda (lengths)
                                   (apply #'max lengths))
                                 (rotate (mapcar (lambda (row)
                                                   (mapcar (lambda (string-field)
                                                             (length string-field))
                                                           row))
                                                 (cons column-names
                                                       rows)))))))


;; *print-readably* has merit, but the whole system must be refit for it.
;; Right now? There is a neat condition to signal that you're not ready.
;; *print-level* has to be honored, but it will be automatic: just use write.
;; *print-pretty* may affect individual elements, so be careful.
;; All others? Irrelevant.


(defmethod print-object ((table printable-table) stream)
  (if *print-readably*
    (error 'print-not-readable
           :object table)
    (with-slots (column-names rows fields-lengths pretty-print-fun) table
      (funcall pretty-print-fun
               stream
               column-names
               rows
               fields-lengths))))


(defclass printable-table-slice ()
  ((table :initarg :table
          :type printable-table)
   (starting-row :initarg :start
                 :type number)
   (ending-row :initarg :end
               :type number)))


(defmethod slice ((table printable-table) (start number) (end number))
  (with-slots (rows) table
    (make-instance 'printable-table-slice
                   :table table
                   :start (max 0 start)
                   :end (min (length rows) end))))


(defmethod print-object ((table-slice printable-table-slice) stream)
  (if *print-readably*
    (error 'print-not-readable
           :object table-slice)
    (with-slots (table starting-row ending-row) table-slice
      (with-slots (column-names rows fields-lengths pretty-print-fun) table
        (funcall pretty-print-fun
                 stream
                 column-names
                 (subseq rows starting-row ending-row)
                 fields-lengths)))))


(defparameter *max-string-length* 50)
(defparameter *wrap-marker* "...")


(defun wrap-string (string)
  (declare (special *max-string-length* *wrap-marker*))
  "This function tries to wrap STRING around *MAX-STRING-LENGTH* characters
smartly. It uses the fact that string could have a natural newline somewhere
before character limit is reached, and therefore could be wrapped there. If
this fails, it tries to wrap the string on a space, rather than cut the word in
two. Also, it calculates length of *WRAP-MARKER*, and if string could be
fitted tightly without marker - it will do it."
  (let* ((first-newline-position (position #\newline string))
         (single-line? (and (or (null first-newline-position)
                                (= first-newline-position (1- (length string))))
                            (<= (length string) *max-string-length*))))
    (if single-line?
      (subseq string 0 first-newline-position)
      (let* ((part-length (min (length string) (- *max-string-length* (length *wrap-marker*))))
             (space-position (position #\space string :end part-length :from-end t))
             (newline-position (position #\newline string :end part-length)))
        (concatenate 'string
                     (subseq string 0 (or newline-position
                                          space-position
                                          part-length))
                     *wrap-marker*)))))


(defparameter *assumed-terminal-width* 80)


(defun slice-string-for-printing (string &optional (width (1- *assumed-terminal-width*)))
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


;;; TODO: Rewrite to use any table format whatsoever
;;; Usage:
;;; (format nil *table-metaformat* desired-field-widths) to get format string
;;; (format t format-string table) to pretty print table
;;; TODO: Right justify number columns
(defparameter *table-metaformat* "~~:{~{~~~A,4@<~~A~~>~}~~&~~}")
(defparameter *table-pad-width* 2)
(defparameter *get-table-name-delimiter* (lambda (length)
                                           (repeat-string length #\-)))


;; DEPRECATED
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


;; DEPRECATED
(defun pretty-print-rows (row-list &key ((:max-field-widths max-field-widths) (find-max-field-widths row-list)))
  (format *standard-output* (format nil *table-metaformat* (map 'list
                                                                (lambda (x) (+ x *table-pad-width*))
                                                                max-field-widths))
          (map 'list (lambda (row) (map 'list
                                        (lambda (value)
                                          (if (stringp value) (wrap-string value) value))
                                        row))
               row-list)))


;; DEPRECATED
(defun pretty-print-table (column-names rows)
  (let ((max-field-widths (find-max-field-widths (cons column-names rows))))
    (pretty-print-rows (cons column-names
                             (cons (map 'list (lambda (length)
                                                (funcall *get-table-name-delimiter* (min length *max-string-length*)))
                                        max-field-widths)
                                   rows))
                       :max-field-widths max-field-widths)))


(defun simple-pretty-print-rows (stream row-list max-field-widths)
  (format stream (format nil *table-metaformat* (mapcar (lambda (x)
                                                          (+ x *table-pad-width*))
                                                        max-field-widths))
          row-list))


(defun simple-pretty-print-table (stream string-column-names string-rows max-field-widths)
  (format stream (format nil *table-metaformat* (mapcar (lambda (x)
                                                          (+ x *table-pad-width*))
                                                        max-field-widths))
          (cons string-column-names
                (cons (mapcar (lambda (length)
                                (funcall *get-table-name-delimiter*
                                         (min length *max-string-length*)))
                              max-field-widths)
                      string-rows))))


(defun make-simple-printable-table (column-names rows)
  (flet ((print-field (x) (wrap-string
                            (if (stringp x)
                              x
                              (write-to-string x)))))
    (let ((stringified-table (mapcar (lambda (row)
                                       (mapcar #'print-field
                                               row))
                                     (cons column-names
                                           rows))))
      (make-instance 'printable-table
                     :columns (first stringified-table)
                     :rows (rest stringified-table)
                     :print-fun #'simple-pretty-print-table))))
