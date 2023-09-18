;;;; interaction.lisp


(in-package :datapouch.interaction)


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


(defun find-one-row-dialog (column-names rows &key
                            ((:prompt-msg prompt-msg) "Enter row number:~&")
                            ((:error-msg error-msg) "Please, try again.~&")
                            ((:id-column-name id-column-name) "Row number")
                            ((:get-index get-index) nil)
                            ((:prompt-fun prompt-fun) *prompt-fun*))
  (pretty-print-table (cons id-column-name column-names)
                      (loop for row in rows
                            for i from 1 to (length rows)
                            collect (cons i row)))
  (format *standard-output* prompt-msg)
  (finish-output *standard-output*)
  (let* ((number (loop for (form _ eof) = (multiple-value-list (read-form "" prompt-fun))
                       if eof return nil
                       else if (and (integerp form) (<= 1 form (length rows))) return form
                       else if error-msg do
                       (format *standard-output* error-msg)
                       (finish-output *standard-output*))))
    (when number
      (if get-index
        (1- number)
        (nth (1- number) rows)))))
