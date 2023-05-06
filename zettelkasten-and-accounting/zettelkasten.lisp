;;;; user.lisp
;;; Zettelkasten example
;;;
;;; WARNING! Every and any note interaction must interpret a note as ID number
;;; Note lists are lists of IDs (lists of integers)
;;; This is the crucial component in maintaining consistency between application and database


(in-package :zac.box)


(defparameter *current-note* nil)
(defparameter *note-history* nil) ; TBD


(defparameter *option-show-note-after-jump* t)


;;; Table creation
;;; note: id [int] PK, text [text] NN
;;; link: source (id) NN, destination (id) NN, number [int]
(defun create-zettelkasten ()
  (create-table (:note :if-not-exists t)
                ((:id :type 'integer
                      :primary-key t
                      :autoincrement t)
                 (:text :type 'text
                        :not-null t)))
  (create-table (:link :if-not-exists t)
                ((:source :type 'integer
                          :not-null t)
                 (:destination :type 'integer
                               :not-null t)
                 (:number :type 'integer))
                (foreign-key :source
                             :references '(:note :id)
                             :on-delete :cascade
                             :on-update :cascade)
                (foreign-key :destination
                             :references '(:note :id)
                             :on-delete :cascade
                             :on-update :cascade)))


;;; Returns maximum ID in note table
(defmacro max-note-id ()
  `(caar (select :id (from :note) (order-by (:desc :id)) (limit 1))))


;;; Returns list of notes with one note (unified format for queries)
(defun get-note-by-id (id)
  (select '(:id :text) (from :note) (where (:= :id id))))


;;; Returns list of all notes without parents (no links with destination == note.id)
;;; There should be only one such a note in normal zettelkasten
;(defun get-orphans ()
;  (select '(:id :text)
;    (from :note)
;    (left-join :link :on (:= :link.destination :note.id))
;    (where (:is-null :link.source))))


(defun get-prompt ()
  (when *current-note* (format nil "~36,V,'0R" (1+ (floor (log (max-note-id) 36))) *current-note*)))


(defparameter +errmsg-note-is-not-chosen+ "Note is not chosen.")


(defun note-is-not-chosen (&key ((:output-stream output-stream) *standard-output*))
  (if (null *current-note*)
    (format output-stream "~A~&" +errmsg-note-is-not-chosen+)
    (error "INTERNAL ERROR in zac.box: *current-note* is not nil, but assumed as such")))


(defun show-text (&key
                   ((:note note) *current-note*)
                   ((:output-stream output-stream) *standard-output*))
  (if note
    (let ((answer (get-note-by-id *current-note*)))
      (if answer
        (format output-stream "~A~&" (second (first answer)))
        (error "INTERNAL: No note with such ID")))
    (note-is-not-chosen :output-stream output-stream)))


;;; Find notes from db, using substring
;;; Subset of notes can be limited by choosing specific list of IDs
(defun goto-text (substring &key ((:list note-list) nil))
  (declare (type list note-list))
  (let ((where-clause (list :instr :text substring)))
    (when note-list
      (setf where-clause (list :and where-clause (list :in :id note-list))))
    (select :id
            (from :note)
            (where where-clause))))

; TODO
;(defun goto-history
;(defun goto-link


;;; As with `find-note' but more interactive. Obsolete.
;(defun note-search ()
;  (with-state 'search
;    (format t "Please enter part of note or its ID to find it:~&")
;    (loop for info = (read-form *input*)
;          if (stringp info) return (find-note info)
;          else if (integerp info) return (get-note-by-id info)
;          else do (format t "That is not string or a number. Try again:~&"))))


;;; Call editor to edit one or more notes
;;; (on call with no arguments will attempt to edit current node)
(defun edit-note (&key ((:notes notes) (list *current-note*)))
  (declare (type list notes))
  (and notes
       (let* ((old-notes (select '(:id :text)
                                 (from :note)
                                 (where (:in :id notes))))
              (new-notes (map 'list (lambda (note new-text) (list (first note) new-text))
                              old-notes
                              (apply #'edit-strings (map 'list #'second old-notes)))))
         (update :note
                 (set= :text `(:case :id
                                     ,@(loop for new-note in new-notes
                                             collect (cons :when new-note))
                                     (:else :text)))
                 (where (:in :id notes))))))


;;; Add new note
;;; Default source-note is *current-note*. In the hope that this will incentivize user not to create dangling notes.
;;; :NUMBER will determine number of this link (is userful for sorting and
;;; tables of contents)
(defun add-note (&key ((:from source-note) *current-note*)
                      ((:number num) nil))
  (let ((new-note (caar (insert-into :note
                                     (set= :text (first (edit-strings "")))
                                     (returning :id)))))
    (when source-note
      (insert-into :link (set= :source source-note
                               :destination new-note
                               :number num)))))


;;; Remove note by ID (or current note if ID is not supplied)
;;; When nil is supplied, does nothing
(defun remove-note (&key ((:note note) *current-note*))
  (when note
    (delete-from :note (where (:= :id note)))
    (when (eql note *current-note*)
      (setf *current-note* nil)))) ; TODO Remake to last history item, when history is implemented


;;; Show all links of a note
(defun show-links (&key ((:note note) *current-note*)
                        ((:output-stream output-stream) *standard-output*))
  (if note
    (pretty-print-table
      '("Number" "Note")
      (select ((:ifnull :link.number "") :note.text)
              (from :link)
              (left-join :note :on (:= :link.destination :note.id))
              (where (:= :link.source note))
              (order-by (:asc :link.number)))
      :output-stream output-stream)
    (note-is-not-chosen :output-stream output-stream)))


;;; Interactive dialog to choose a note from list
(defun choose-note-interactive (notes)
  (declare (type list notes))
  (if (= (length notes) 1)
    (first notes)
    (let* ((found-notes (select '(:id :text) (from :note) (where (:in :id notes))))
           (chosen-note-index (and found-notes (find-one-row-dialog '("Text")
                                                                    (map 'list #'cdr found-notes)
                                                                    :get-index t
                                                                    :prompt-fun (constantly "note> ")))))
      (when chosen-note-index (first (nth chosen-note-index found-notes))))))


;;; Go to link of some note and return destination note ID
(defun choose-link-interactive (&key ((:note note) *current-note*)
                                     ((:output-stream output-stream) *standard-output*))
  (if note
    (let ((linked-notes (select (:link.destination :link.number :note.text)
                                (from :link)
                                (left-join :note :on (:= :link.destination :note.id))
                                (order-by (:asc :link.number))
                                (where (:= :link.source note)))))
      (cond ((null linked-notes) (format output-stream "No links available."))
            ((= (length linked-notes) 1) (setf *current-note* (first (first linked-notes))))
            (:else
              ;(format t "~A~&" linked-notes) ; DEBUG
              ;(format t "~A~&" (map 'list #'cdr linked-notes)) ; DEBUG
              (let ((chosen-link-index (find-one-row-dialog '("Number" "Text")
                                                            (map 'list #'cdr linked-notes)
                                                            :get-index t
                                                            :output-stream output-stream)))
                (when chosen-link-index (first (nth chosen-link-index linked-notes)))))))
    (note-is-not-chosen)))


;;; HIGH LEVEL (To be called from shortcuts)
