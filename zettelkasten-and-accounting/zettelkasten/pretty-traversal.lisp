;;;; pretty-traversal.lisp


(in-package :zac.zettelkasten.pretty-traversal)


;;; fields is a function of one argument - how long join-chain is supposed to be in this select
(defun build-select-notes-through-links (&key ((:backward backward?) nil)
                                              ((:depth exponent) 1)
                                              ((:closure closure?) nil)
                                              ((:link-name-generator link-name-generator) (lambda (&optional index)
                                                                                            (make-name :table :link
                                                                                                       :index index)))
                                              ((:starting-table-alias starting-table-alias) :source)
                                              ((:ending-table-alias ending-table-alias) :destination)
                                              ((:fields fields) (lambda (chain-length)
                                                                  (declare (ignore chain-length))
                                                                  :id))
                                              ((:clauses clauses) nil)
                                              ((:union-clauses union-clauses) nil))
  (declare (type boolean backward? closure?)
           (type integer exponent))
  (let* ((source-column (if (not backward?) :source :destination))
         (destination-column (if (not backward?) :destination :source))
         (max-fields-length (length (funcall fields exponent))))
    (labels ((get-padded-fields (chain-length) (let ((current-fields (funcall fields chain-length)))
                                                 (append current-fields
                                                         (loop :for i :from (1+ (length current-fields)) :to max-fields-length
                                                               :collect :null))))
             (build-select (chain-length) (apply #'build :select
                                                 (get-padded-fields chain-length)
                                                 (append
                                                   (zac.aux:get-chained-table-expression chain-length
                                                                                         :note :id
                                                                                         link-name-generator source-column destination-column
                                                                                         :note :id
                                                                                         :starting-table-alias starting-table-alias
                                                                                         :ending-table-alias ending-table-alias)
                                                   clauses)))
             (build-union (max-chain-length) (apply #'build :union-queries
                                                    (append
                                                      (loop :for i :from 1 :to max-chain-length
                                                            :collect (build-select i))
                                                      union-clauses))))
      (funcall (if closure? #'build-union #'build-select) exponent))))


(defun select-notes-through-links (note-id backward? exponent closure?)
  (declare (type integer note-id exponent)
           (type boolean backward? closure?))
  (let* ((target-column (if (not backward?) :destination :source))
         (show-numbers? (and (not closure?)
                             (= exponent 1))))
    (labels ((link-name-gen (&optional index column) (make-name :table :link
                                                                :index index
                                                                :column column))
             (get-fields-for-join-chain (chain-length) (loop :for index :from 0 :to (1- chain-length)
                                                             :collect (link-name-gen index target-column)))
             (fields-generator (chain-length) (list-existing* :destination.text
                                                              :destination.id
                                                              (when show-numbers? (link-name-gen 0 :number))
                                                              (when closure? (get-fields-for-join-chain chain-length)))))
      (let* ((found-rows (query (build-select-notes-through-links :backward backward?
                                                                  :depth exponent
                                                                  :link-name-generator #'link-name-gen
                                                                  :fields #'fields-generator
                                                                  :closure closure?
                                                                  :clauses (list-existing (where (:= :source.id note-id))
                                                                                          (unless closure?
                                                                                            (apply #'build :order-by (list-existing (when show-numbers?
                                                                                                                                      (link-name-gen 0 :number))
                                                                                                                                    :destination.id))))
                                                                  :union-clauses (list-existing (when closure?
                                                                                                  (apply #'build :order-by
                                                                                                         (reverse (get-fields-for-join-chain exponent))))))))
             (transformed-rows (map 'list (lambda (row)
                                            (append (list :text (first row))
                                                    (list :id (second row))
                                                    (when show-numbers? (list :number (third row)))
                                                    (when closure? (list :path (list-existing* (cddr row))))))
                                    found-rows)))
        transformed-rows))))


(defun column-names-for-notes-through-links (backward? exponent closure?)
  (declare (ignore backward?))
  (let ((show-numbers? (and (not closure?)
                            (= exponent 1))))
    (list-existing (when closure? "Path")
                   (second (assoc :text zac.box.db:+table-note-fields+))
                   (when show-numbers? (second (assoc :number zac.box.db:+table-link-fields+))))))


(defun note-path-to-string (path &optional (nil-string "N"))
  (format nil "~{~#[~;~A~:;~A->~]~}" (map 'list (lambda (x)
                                                (if (null x)
                                                  nil-string
                                                  x))
                                        path)))


(defun row-transformation-without-pathing (rows &key ((:number-stub number-stub) "") ((:continue-stub continue-stub) "->"))
  (map 'list (lambda (row)
               (let* ((index (first row))
                      (transformed-row (rest row))
                      (number (getf transformed-row :number)))
                 (list-existing index
                                (getf transformed-row :text)
                                (if number (if (= number 0)
                                             continue-stub
                                             number)
                                  number-stub))))
       rows))


(defun row-transformation-for-pathing (rows &key ((:backward backward?) nil))
  ;(format t "ROWS: ~A~&" rows)
  (let ((id-mapping (map 'list (lambda (row)
                                 (let ((index (first row))
                                       (transformed-row (rest row)))
                                   (list (getf transformed-row :id) index)))
                         rows)))
    ;(format t "ID MAP: ~A~&" id-mapping)
    (map 'list (lambda (row)
                 (let* ((index (first row))
                        (transformed-row (rest row))
                        (pretty-path (map 'list (lambda (note-id)
                                                  (second (assoc note-id id-mapping)))
                                          (getf transformed-row :path))))
                   (list index
                         (note-path-to-string (if backward? (reverse pretty-path) pretty-path))
                         (getf transformed-row :text))))
         rows)))


(defun choose-row-from-note-through-links (transformed-rows prompt backward? exponent closure?)
  (let* ((column-names (column-names-for-notes-through-links backward? exponent closure?))
         (chosen-row-index (and transformed-rows (find-row-dialog column-names
                                                                  transformed-rows
                                                                  :row-transformation-function (if closure?
                                                                                                 (lambda (rows)
                                                                                                   (row-transformation-for-pathing rows :backward backward?))
                                                                                                 #'row-transformation-without-pathing)
                                                                  :get-index t
                                                                  :prompt-fun prompt))))
    (and chosen-row-index (nth chosen-row-index transformed-rows))))


(defun pretty-print-note-through-links (transformed-rows backward? exponent closure?)
  (let* ((column-names (column-names-for-notes-through-links backward? exponent closure?))
         (row-transformation (if closure?
                               (lambda (rows) (row-transformation-for-pathing rows :backward backward?))
                               #'row-transformation-without-pathing))
         (prettified-rows (map 'list #'cdr (funcall row-transformation (loop :for row :in transformed-rows
                                                                             :for i :from 1 to (length transformed-rows)
                                                                             :collect (cons i row))))))
    (pretty-print-table column-names prettified-rows)))
