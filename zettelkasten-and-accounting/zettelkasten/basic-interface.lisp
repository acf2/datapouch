;;;; zettelkasten/basic-interface.lisp


(in-package :zac.box)


;;; Returns maximum ID in note table
(defmacro max-note-id ()
  `(caar (select :id (from :note) (order-by (:desc :id)) (limit 1))))


;;; Returns list of notes with one note (unified format for queries)
(defun get-note-by-id (id)
  (select '(:id :text) (from :note) (where (:= :id id))))


(defun show-note (note)
  (let ((answer (when note (get-note-by-id note))))
    (if answer
      (format *standard-output* "~A~&" (second (first answer)))
      (error +interrmsg-cannot-find-id+))))


;;; Wrapper for setting a note
;;; Every note change should be going through this function
;;; Unless you what to add some low-level stuff
(defun set-current-note (note &key ((:update-history update-history) t))
  (setf *current-note* note)
  (when note
    (when update-history
      nil) ; TODO
    (when *option-show-note-after-jump*
      (show-note note))))


;;; Call editor to edit one or more notes
(defun edit-notes (&rest clauses)
  (let* ((old-notes (apply #'d.sql:build-and-query
                           :select '(:id :text)
                           (from :note)
                           clauses))
         (new-notes (map 'list (lambda (note new-text) (list (first note) new-text))
                         old-notes
                         (apply #'edit-strings (map 'list #'second old-notes)))))
    (when old-notes
      (apply #'d.sql:build-and-query
             :update :note
             (set= :text `(:case :id
                                 ,@(loop :for new-note :in new-notes
                                         :collect (cons :when new-note))
                                 (:else :text)))
             clauses))))


(defun get-field-names (fields)
  (map 'list #'first fields))


(defun get-field-dialog-texts (fields)
  (remove nil (map 'list #'second fields)))


(defun get-field-mapping-for-rows (fields)
  (let ((indices (loop :for element :in (map 'list #'second fields)
                       :for i :from 0 :to (length fields)
                       :when element :collect i)))
    (lambda (row)
      (map 'list (lambda (i) (nth i row)) indices))))


(defun choose-row-from-table-dialog (table-clauses fields prompt &rest clauses)
  (declare (type list fields)
           (type function prompt))
  (let* ((found-rows (apply #'d.sql:build-and-query
                            :select (get-field-names fields)
                            (append (ensure-list table-clauses)
                                    clauses)))
         (chosen-row-index (and found-rows (find-row-dialog (get-field-dialog-texts fields)
                                                            (map 'list (get-field-mapping-for-rows fields) found-rows)
                                                            :get-index t
                                                            :prompt-fun prompt))))
    (cond ((null found-rows)
           (values nil +msg-no-notes+)) ; TODO Do something with this
          ((null chosen-row-index)
           (values nil +msg-note-is-not-chosen+)) ; TODO Do something with this [x2]
          (:else (values (nth chosen-row-index found-rows) nil)))))
