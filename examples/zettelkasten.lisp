;;;; user.lisp
;;; Zettelkasten example

;;; user.lisp was made to be somewhat jury-rigged in feel and composition
;;; it's config for particular user, not some production-quality code
;;; so you will see several examples of *very* bad and tangled lisp down there

(defparameter *note* nil)

(defun recreate-zettelkasten ()
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

(defmacro max-note-id ()
  `(caar (select :id (from :note) (order-by (:desc :id)) (limit 1))))

(defun get-note-by-id (id)
  (select :* (from :note) (where (:= :id id))))

;; TODO: Feature: Use note id as number in base36 with zero padding
;; NOTE: You can inherit interactive-input for more drastic changes
(defmethod get-prompt :around ((input interactive-input))
  (concatenate 'string
               (when *note* (format nil "[~36,V,'0R] " (1+ (floor (log (max-note-id) 36))) *note*))
               (call-next-method input)))

(defmacro with-state (state &body body)
  `(let (result)
     (push ,state (prompt-list *input*))
     (setf result (progn ,@body))
     (pop (prompt-list *input*))
     result))

(defun find-note (substring)
  (let ((found-notes (select :* (from :note) (where (:instr :text substring)))))
    (cond ((= (length found-notes) 1) (first found-notes))
          (:else
            (with-state 'row
                        (and found-notes (nth (find-one-row-dialog '("Text")
                                                              (map 'list #'cdr found-notes)
                                                              :get-index t)
                                              found-notes)))))))

;(defun note-search ()
;  (with-state 'search
;    (format t "Please enter part of note or its ID to find it:~&")
;    (loop for info = (read-form *input*)
;          if (stringp info) return (find-note info)
;          else if (integerp info) return (get-note-by-id info)
;          else do (format t "That is not string or a number. Try again:~&"))))

(defun edit-note (&rest notes)
  (cond ((null notes) (edit-note *note*))
        (:else 
          (let* ((old-notes (select :*
                                    (from :note)
                                    (where (:in :id (loop for note in notes
                                                          if (integerp note) collect note
                                                          else if (and (listp note) (integerp (first note))) collect (first note))))))
                 (new-notes (map 'list (lambda (note new-text) (list (first note) new-text))
                                 old-notes
                                 (apply #'edit-strings (map 'list #'second old-notes)))))
            (loop for new-note in new-notes do
                  (update :note
                          (set= :text (second new-note))
                          (where (:= :id (first new-note)))))))))

(defun add-note (&key ((:from from) nil)
                      ((:number num) nil))
  (let ((source-note (cond ((null from) *note*)
                           ((listp from) (first from))
                           ((stringp from) (first (find-note from)))
                           ((integerp from) from)
                           (:else nil)))
        (first-note (null (select :* (from :note)))))
    (when (or source-note first-note)
      (let ((new-note (caar (insert-into :note
                                         (set= :text (first (edit-strings "")))
                                         (returning :id)))))
        (unless first-note
          (insert-into :link (set= :source source-note
                                   :destination new-note
                                   :number num)))))))


;;; HIGH LEVEL
                             
(defun note (&optional substring)
  (cond ((and *note* (not substring)) (format t "~A~&" (second (first (get-note-by-id *note*)))))
        (substring (setf *note* (first (find-note substring))))))

(defun clear ()
  (setf *note* nil))
