;;;; zettelkasten/basic-interface.lisp


(in-package :zettelkasten)


;;; Returns maximum ID in note table
(defmacro max-note-id ()
  `(caar (select :id (from :note) (order-by (:desc :id)) (limit 1))))


;;; Returns list of notes with one note (unified format for queries)
(defun get-note-by-id (id)
  (select '(:id :text) (from :note) (where (:= :id id))))


(defun get-notes-by-id (ids)
  (select '(:id :text)
          (from :note)
          (where (:in :id ids))
          (order-by `(:case :id ,@(loop :for index :from 1 :to (length ids)
                                        :for id :in ids
                                        :collect (list :when id index))))))


;(defun get-links-by-id-pairs (id-pairs)
;  nil)


(defun print-note (text &optional (last? t))
  (format *standard-output* "~A~&~@[~%~]" text (not last?)))


(defun show-note (note)
  (let* ((answer (when note (get-note-by-id note)))
         (text (second (first answer))))
    (if text
      (print-note text)
      (error +intermsg-cannot-find-id+))))


(defun list-notes (notes)
  (let* ((answer (when notes (get-notes-by-id notes)))
         (texts (map 'list #'rest answer)))
    (if texts
      (pretty-print-table '("Notes") texts)
      (error +intermsg-cannot-find-id+))))


(defun show-notes (notes &key ((:print-note-fun print-note-fun) #'print-note))
  (let* ((answer (when notes (get-notes-by-id notes)))
         (texts (map 'list #'rest answer)))
    (if texts
      (loop :for note-texts := texts :then (rest note-texts)
            :for note-text := (first (first note-texts))
            :while note-text
            :do (funcall print-note-fun note-text (null (rest note-texts))))
      (error +intermsg-cannot-find-id+))))


;;; Wrapper for setting a note
;;; Every note change should be going through this function
;;; Unless you what to add some low-level stuff
(defun set-current-note (note &key ((:update-history update-history) t))
  (when *current-note*
    (when update-history
      (setf *note-history* (cons *current-note* *note-history*))
      (setf *note-future* nil)))
  (setf *current-note* note)
  (when *current-note*
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


;; Should find all notes, that are unreachable from root
(defun find-lost-notes ()
  (loop :with all-notes := (reduce #'append (select '(:id) (from :note)))
        :with all-arcs := (select '(:source :destination) (from :link))
        :for queue := (list 0) :then (cdr queue)
        :for current := (car queue)
        :unless current :return all-notes
        :when (member current all-notes)
        :do (setf all-notes (delete current all-notes))
        (setf queue (append queue (map 'list #'second (remove-if-not (lambda (x) (eq current (first x))) all-arcs))))))


;;; Add new note
;;; :NUMBER will determine number of this link (is userful for sorting and
;;; tables of contents)
;;; NOTE: Exception to "interaction rule"
(defun add-note (text source-note &optional (number nil))
  (let ((new-note (caar (insert-into :note
                                     (set= :text text)
                                     (returning :id)))))
    (when source-note
      (insert-into :link (set= :source source-note
                               :destination new-note
                               :number number)))
    new-note))
