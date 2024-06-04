;;;; zettelkasten/schema.lisp


(in-package :zac.zettelkasten.schema)


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
                (unique-key '(:source :destination))
                (foreign-key :source
                             :references '(:note :id)
                             :on-delete :cascade
                             :on-update :cascade)
                (foreign-key :destination
                             :references '(:note :id)
                             :on-delete :cascade
                             :on-update :cascade))
  (unless (select :* (from :note) (where (:= :id 0)))
    (insert-into :note (set= :id 0 :text "A stub: Root node")))) ; add root node


(defparameter +table-note-fields+ (list (list :id) (list :text "Text")))
(defparameter +table-link-fields+ (list (list :source) (list :destination) (list :number "Number")))
