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
                                                   (get-chained-table-expression chain-length
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


(defun select-notes-through-links (note-id &key ((:direction direction)) ((:exponent exponent)) ((:closure closure?)))
  (declare (type integer note-id exponent)
           (type keyword direction)
           (type boolean closure?))
  (let* ((target-column (if (eq direction :forward) :destination :source))
         (backward? (eq direction :backward))
         (show-numbers? (and (not closure?)
                             (= exponent 1))))
    (labels ((link-name-gen (&optional index column) (make-name :table :link
                                                                :index index
                                                                :column column))
             (get-fields-for-join-chain (chain-length) (cons :source.id
                                                             (loop :for index :from 0 :to (1- chain-length)
                                                                   :collect (link-name-gen index target-column))))
             (fields-generator (chain-length) (list-existing* :destination.text
                                                              :destination.id
                                                              (when show-numbers? (link-name-gen 0 :number))
                                                              (when closure? (get-fields-for-join-chain chain-length)))))
      (let* ((found-rows (query (build-select-notes-through-links :backward backward?
                                                                  :depth exponent
                                                                  :link-name-generator #'link-name-gen
                                                                  :fields #'fields-generator
                                                                  :closure closure?
                                                                  :clauses (list-existing (where (:and (:= :source.id note-id)
                                                                                                       (:!= :destination.id note-id)))
                                                                                          (unless closure?
                                                                                            (apply #'build :group-by (fields-generator exponent)))
                                                                                          (unless closure?
                                                                                            (apply #'build :order-by (list-existing (when show-numbers?
                                                                                                                                      (link-name-gen 0 :number))
                                                                                                                                    :destination.id
                                                                                                                                    :destination.text))))
                                                                  :union-clauses (list-existing (when closure?
                                                                                                  (apply #'build :order-by
                                                                                                         (reverse (cons :destination.text
                                                                                                                        (get-fields-for-join-chain exponent)))))))))
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
    (list-existing (when closure? "Referrers")
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


(defun prettify-rows (rows &key ((:number-stub number-stub) "") ((:continue-stub continue-stub) "->"))
  (map 'list (lambda (row)
               (let ((number (getf row :number)))
                 (list-existing (getf row :text)
                                (if number
                                  (if (= number 0)
                                    continue-stub
                                    number)
                                  number-stub))))
       rows))


(defun clear-cycles-from-path (path)
  (loop :for current-list   = path :then (rest current-list)
        :for current-id     = (first current-list)
        :for previous-list  = (cons current-id previous-list)
        :for pos            = (position current-id previous-list :start 1)
        :unless current-list :return (nreverse (rest previous-list))
        :when pos
        :do (setf previous-list (subseq previous-list pos))))


;; Get referrers (second to last in paths)
;; Sort rows by path length
;; Sort referrers for every id by order of rows in sorted row list
;; Place sorted referrers in p-list by :referrers key
(defun get-referrers (rows)
  (let* ((rows-with-clear-paths (map 'list (lambda (row)
                                             (append row (list :clear-path
                                                               (clear-cycles-from-path (getf row :path)))))
                                     rows))
         (sorted-rows (sort rows-with-clear-paths #'< :key (lambda (row)
                                                             (length (getf row :clear-path)))))
         (referrers (make-hash-table :test #'eql)))
    (labels ((row-id-key (row) (getf row :id))
             ;; When len == 2, the path is current-note -> target-note,
             ;;  in this case, referrer is current-note (i.e. root of this subtree)
             ;; Second to last, because the last one is target-note id, but we need referrer id
             (get-referrer-from-path (path) (if (> (length path) 2)
                                              (first (last path 2))
                                              'root))
             ;; generate-referrer-list with sorted referrers
             ;; sorted rows already established some sort of sorting
             ;; just hard copy it, using positions of ids inside sorted-rows for comparison
             ;; subtree root is always on top
             ;; NILs are ids of notes, that were filtered due to some circumstances
             ;; they are always at the bottom
             (generate-referrer-list (row) (sort (remove-duplicates (gethash (row-id-key row) referrers))
                                                 (lambda (one another)
                                                   (if (eq another 'root)
                                                     nil
                                                     (if (eq one 'root)
                                                       t
                                                       (let ((one-pos (position one sorted-rows :key #'row-id-key))
                                                             (another-pos (position another sorted-rows :key #'row-id-key)))
                                                         (cond ((and (null one-pos) (null another-pos)) nil)
                                                               ((and (null one-pos) another-pos) nil)
                                                               ((and one-pos (null another-pos)) t)
                                                               (:else (< one-pos another-pos))))))))))
      (loop :for row :in sorted-rows
            :do (pushnew (get-referrer-from-path (getf row :clear-path))
                         (gethash (row-id-key row) referrers)))
      (map 'list (lambda (row)
                   (append row
                           (list :referrers
                                 (generate-referrer-list row))))
           (remove-duplicates sorted-rows :key #'row-id-key)))))


(defun referrers-to-string (referrer-list &key
                                          ((:source-stub source-stub) ">")
                                          ((:nonexistent-referrer-stub nonexistent-referrer-stub) "."))
  (format nil "~{~#[~;~A~:;~A ~]~}" (map 'list (lambda (referrer)
                                                 (cond ((eq referrer 'root) source-stub)
                                                       ((null referrer) nonexistent-referrer-stub)
                                                       (:else referrer)))
                                         referrer-list)))


(defun row-transformations-for-referrers (rows)
  (let ((id-mapping (map 'list (lambda (row)
                                 (let ((index (first row))
                                       (transformed-row (rest row)))
                                   (list (getf transformed-row :id) index)))
                         rows)))
    (map 'list (lambda (row)
                 (let* ((index (first row))
                        (transformed-row (rest row))
                        (pretty-referrers (map 'list (lambda (referrer)
                                                       (if (eq referrer 'root)
                                                         referrer
                                                         (second (assoc referrer id-mapping))))
                                               (getf transformed-row :referrers))))
                   (list index
                         (referrers-to-string pretty-referrers)
                         (getf transformed-row :text))))
         rows)))


(defun choose-row-from-note-through-links (transformed-rows prompt &key ((:direction direction)) ((:exponent exponent)) ((:closure closure?)) ((:choose-many choose-many) nil))
  (declare (type integer note-id exponent)
           (type keyword direction)
           (type boolean closure?))
  (let* ((backward? (eq direction :backward)))
    (cond (closure?
            (let* ((column-names (column-names-for-notes-through-links backward? exponent closure?))
                   (sorted-rows (get-referrers transformed-rows))
                   (chosen-row-index (and sorted-rows (find-row-dialog column-names
                                                                       sorted-rows
                                                                       :row-transformation-function #'row-transformations-for-referrers
                                                                       :get-index t
                                                                       :prompt-fun prompt
                                                                       :choose-many choose-many))))
              (and chosen-row-index (if choose-many
                                      (loop :for index :in chosen-row-index
                                            :collect (nth index sorted-rows))
                                      (nth chosen-row-index sorted-rows)))))
          (:else
            (let* ((column-names (column-names-for-notes-through-links backward? exponent closure?))
                   (chosen-row-index (and transformed-rows (find-row-dialog column-names
                                                                            (prettify-rows transformed-rows)
                                                                            :get-index t
                                                                            :prompt-fun prompt
                                                                            :choose-many choose-many))))
              (and chosen-row-index (if choose-many
                                      (loop :for index :in chosen-row-index
                                            :collect (nth index transformed-rows))
                                      (nth chosen-row-index transformed-rows))))))))


(defun pretty-print-note-through-links (transformed-rows backward? exponent closure?)
  (let* ((column-names (column-names-for-notes-through-links backward? exponent closure?))
         (sorted-rows (funcall (if closure? #'get-referrers #'prettify-rows) transformed-rows))
         (prettified-rows (if closure?
                            (row-transformations-for-referrers (loop :for row :in sorted-rows
                                                                                       :for i :from 1 to (length sorted-rows)
                                                                                       :collect (cons i row)))
                            sorted-rows)))
    (pretty-print-table (cons "Number" column-names) prettified-rows)))
