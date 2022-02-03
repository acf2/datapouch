;;;; datapouch-interaction.lisp

(in-package :datapouch.interaction)

(defparameter *max-string-length* 50)
(defparameter *wrap-marker* "...")

(defun wrap-string (string)
  (let* ((part-length (min (length string) (- *max-string-length* (length *wrap-marker*))))
         (space-position (position #\Space string :end part-length :from-end t))
         (newline-position (position #\Newline string :end part-length)))
    (if (and (null (find #\Newline string :end (min (length string) *max-string-length*)))
             (<= (length string) *max-string-length*))
      string
      (concatenate 'string
                   (subseq string 0 (or newline-position
                                        space-position
                                        part-length))
                   *wrap-marker*))))

;; Transpose lists
;; https://stackoverflow.com/a/3513158
(defun rotate (list-of-lists)
  (if (or (null list-of-lists)
          (and (null (car list-of-lists))
               (null (cdr list-of-lists))))
    list-of-lists
    (apply #'map 'list #'list list-of-lists)))

(defmacro repeat-string (times str)
  `(format nil "~V@{~A~:*~}" ,times ,str))

;; Usage:
;; (format nil *table-metaformat* desired-field-widths) to get format string
;; (format t format-string table) to pretty print table
(defparameter *table-metaformat* "~~:{~{~~~A,4@<~~A~~>~}~~&~~}")
(defparameter *table-pad-width* 2)
(defparameter *get-table-name-delimiter* (lambda (length)
                                           (repeat-string length #\-)))

(defun find-max-field-widths (row-list)
  (map 'list (lambda (lst)
            (apply #'max lst))
          (rotate (map 'list (lambda (lst)
                               (map 'list (lambda (elem)
                                            (if (stringp elem)
                                              (length elem)
                                              (length (write-to-string elem))))
                                    lst))
                       row-list))))

(defun pretty-print-rows (row-list &optional (max-field-widths nil))
  (format t (format nil *table-metaformat* (map 'list
                                                (lambda (x) (+ x *table-pad-width*))
                                                (if max-field-widths
                                                  max-field-widths
                                                  (find-max-field-widths row-list))))
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
                                   rows)))))

;; prompt, if function, should handle 2 args: column-names, rows
;; error-prompt, if function, should handler 3 args: column-names, rows, entered-form
(defun find-one-row-dialog (column-names rows &key
                            ((:prompt prompt) "Enter row number:~&")
                            ((:error-prompt error-prompt) "Please, try again.~&")
                            ((:id-column-name id-column-name) "Row number")
                            ((:input-object input-object) *input*))
  (pretty-print-table (cons id-column-name column-names)
                      (loop for row in rows
                            for i from 1 to (length rows)
                            collect (cons i row)))
  (when prompt
    (cond ((functionp prompt) (funcall prompt column-names rows))
          (:else (format t prompt)
                 (finish-output t))))
  (let ((number (loop for (form is-eof?) = (multiple-value-list (read-form input-object))
                      if is-eof? return nil
                      else if (and (integerp form) (<= 1 form (length rows))) return form
                      else if error-prompt do (cond ((functionp error-prompt) (funcall error-prompt column-names rows form))
                                                    (:else (format t error-prompt)
                                                           (finish-output t))))))
    (when number
      (nth (1- number) rows))))
