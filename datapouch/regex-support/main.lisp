;;;; regex-support/main.lisp
;;; This package is made for much easier usage of named groups.
;;; They are not supported normally, but could be enabled for ppcre.
;;; But their handling is shit. That's why this wrapper exists.


(in-package :datapouch.regex-support)


;;; Allow named registers and make control abstraction
(setf ppcre:*allow-named-registers* t)


(defun allow-named-registers (&optional (flag t))
  "Allow or disallow the use of named registers in PPCRE. Note: disallowing it makes
the lib useless."
  (setf ppcre:*allow-named-registers* flag))


(defclass group-id ()
  ((name :type string
         :reader name
         :initarg :name)
   (index :type integer
          :reader index
          :initarg :index))
  (:documentation "Group ID represents indexed group id. This allows for
differentiating between multiple groups with the same
name."))


(declaim (ftype (function (string integer)) make-group-id))
(defun make-group-id (group-name group-index)
  "Shortcut for GROUP-ID class instance creation."
  (make-instance 'group-id
                 :name group-name
                 :index group-index))


(declaim (ftype (function (group-id)) bake-group-id))
(defun bake-group-id (group-id)
  "Group names inside regex must be strings with restricted character set.
'Baking' just serializes GROUP-ID object in some regex-friendly manner."
  (format nil "~A.~A" (name group-id) (index group-id)))


(defun group-map-p (list)
  (and (listp list)
       (every (lambda (x)
                (and (consp x)
                     (typep (first x) 'group-id)
                     (get-properties (rest x) (list :name))
                     (typep (getf (rest x) :name) 'string)
                     (get-properties (rest x) (list :info))))
              list)))


(deftype group-map ()
  "Is NIL or is assoc with elements (GROUP-ID :name STRING :info T). First is
GROUP-ID instance, rest is property list with associated group properties.
Two are saved right now: clean group name and some contextual info, that is
passed along with group match later."
  `(satisfies group-map-p))


(defclass regex ()
  ((tree :type list
         :reader tree
         :initarg :tree)
   (group-map :type group-map
              :reader group-map
              :initarg :group-map
              :initform nil))
  (:documentation "REGEX class objects retain information about named groups in
them. GROUP-IDs help to differ between same-named groups for
matching, while contextual info contains critical user
information for match processing."))


(defun bake-regex-tree (regex-tree)
  "Prepare regex-tree, so it can be read with PPCRE lib. Indices can no longer
be changed."
  (typecase regex-tree
    (group-id (bake-group-id regex-tree))
    (list (map 'list #'bake-regex-tree regex-tree))
    (t regex-tree)))


(defun get-regex-tree-topology (regex-tree)
  "Save the topological structure of named groups inside the tree, to be
reconstructed later."
  (cond
    ;; Named registers will become (<group-id> ...)
    ((ppcre-named-register? regex-tree)
     (list (list* (second regex-tree)
                  (reduce #'append (mapcar #'get-regex-tree-topology
                                           (cddr regex-tree))))))
    ;; Lists will be stripped from everything, other that named registers
    ((listp regex-tree)
     (reduce #'append (mapcar #'get-regex-tree-topology
                              regex-tree)))
    (:else nil)))


(defun bake-group-map (group-map)
  "Prepare group-map to be used along regex-tree. Indices cannot be changed
anymore, unless this is reverted, so 'baking'."
  (map 'list (lambda (mapping)
               (cons (bake-group-id (first mapping))
                     (rest mapping)))
       group-map))


(declaim (ftype (function (group-id integer)) shift-group-id-index))
(defun shift-group-id-index (group-id offset)
  (make-group-id (name group-id)
                 (+ (index group-id) offset)))


;; TODO: make type for regex tree
(declaim (ftype (function (t integer)) shift-group-indices-in-tree))
(defun shift-group-indices-in-tree (regex-tree offset)
  (typecase regex-tree
    (group-id (shift-group-id-index regex-tree offset))
    (list (map 'list (lambda (x)
                       (shift-group-indices-in-tree x offset))
               regex-tree))
    (t regex-tree)))


(declaim (ftype (function (group-map integer)) shift-group-map-indices))
(defun shift-group-map-indices (group-map offset)
  (map 'list (lambda (group-mapping)
               (cons (shift-group-id-index (first group-mapping) offset)
                     (rest group-mapping)))
       group-map))


(declaim (ftype (function (string)) regex-from-string))
(defun regex-from-string (string)
  "Make D.REGEX:REGEX instance from a STRING, that contains a regex."
  (make-instance 'regex :tree (ppcre:parse-string string)))


(defmethod scan ((sc regex) (target-string string) &key start end &allow-other-keys)
  (ppcre:scan (bake-regex-tree (tree sc)) target-string :start (or start 0) :end (or end (length target-string))))


(defgeneric wrap-in-noncapturing-group (regex))


(defmethod wrap-in-noncapturing-group ((regex regex))
  (make-instance 'regex
                 :tree (list :group (tree regex))
                 :group-map (group-map regex)))


(defgeneric make-optional (regex))


(defmethod make-optional ((regex regex))
  (make-instance 'regex
                 :tree (list :greedy-repetition 0 1 (tree regex))
                 :group-map (group-map regex)))


(defgeneric make-named-group (name regex &optional info)
  (:documentation "Make named matching group"))


(defmethod make-named-group ((name string) (regex regex) &optional info)
  (let ((group-id (make-group-id name (length (group-map regex)))))
    (make-instance 'regex
                   :tree (list :named-register group-id (tree regex))
                   :group-map (cons (list group-id :name name :info info)
                                    (group-map regex)))))


(defmethod make-named-group ((name string) (regex string) &optional info)
  (make-named-group name (regex-from-string regex) info))


(defmethod make-named-group ((name symbol) regex &optional info)
  (make-named-group (string name) regex info))


(defmethod d.iface:concat-two ((one regex) (another regex))
  (let ((offset (length (group-map one))))
    (make-instance 'regex
                   :tree (ppcre-sequence-two
                           (tree one)
                           (shift-group-indices-in-tree (tree another) offset))
                   :group-map (append (group-map one)
                                      (shift-group-map-indices (group-map another) offset)))))


(defmethod d.iface:concat-two ((one regex) (another string))
  (concat-two one (regex-from-string another)))


(defmethod d.iface:concat-two ((one string) (another regex))
  (concat-two (regex-from-string one) another))


;; 0_o: Delet zis
(defmethod d.iface:concat-two ((one string) (another string))
  (concat-two (regex-from-string one)
              (regex-from-string another)))


(defmethod d.iface:combine-two ((one regex) (another regex))
  (let ((offset (length (group-map one))))
    (make-instance 'regex
                   :tree (ppcre-alter-two
                           (tree one)
                           (shift-group-indices-in-tree (tree another) offset))
                   :group-map (append (group-map one)
                                      (shift-group-map-indices (group-map another) offset)))))


(defmethod d.iface:combine-two ((one regex) (another string))
  (combine-two one (regex-from-string another)))


(defmethod d.iface:combine-two ((one string) (another regex))
  (combine-two (regex-from-string one) another))


;; o_0: And delet dis 2oo
(defmethod d.iface:combine-two ((one string) (another string))
  (combine-two (regex-from-string one)
               (regex-from-string another)))


(declaim (ftype (function (relaxed-regex relaxed-regex relaxed-regex)) interchange))
(defun interchange (separator one another)
  (wrap-in-noncapturing-group (combine (concat one separator another)
                                       (concat another separator one))))


(defun interchange-three (separator-regex first-regex second-regex third-regex)
  "Make regex that matches three separated regexes in any order"
  (wrap-in-noncapturing-group
    (combine (concat first-regex separator-regex (interchange separator-regex second-regex third-regex))
             (concat second-regex separator-regex (interchange separator-regex first-regex third-regex))
             (concat third-regex separator-regex (interchange separator-regex first-regex second-regex)))))


(defclass regex-scanner ()
  ((scanner :reader scanner ; TODO: scanner type?
            :initarg :scanner
            :initform nil)
   (regex-tree-topology :type list
                        :reader regex-tree-topology
                        :initarg :regex-tree-topology)
   (ordered-group-assoc :type list
                        :reader group-map
                        :initarg :group-map)
   (group-list :type list
               :reader group-list
               :initarg :group-list
               :initform nil)))


(defgeneric make-scanner (regex)
  (:documentation "Make faster scanner from regex object"))


(defmethod make-scanner ((regex regex))
  (multiple-value-bind (new-scanner register-list) (ppcre:create-scanner (bake-regex-tree (tree regex)))
    (let ((scanner-group-map (loop :with baked-group-map := (bake-group-map (group-map regex))
                                   :for register-name :in register-list
                                   :collect (assoc register-name baked-group-map :test #'string=))))
      (make-instance 'regex-scanner
                     :scanner new-scanner
                     :regex-tree-topology (bake-regex-tree (get-regex-tree-topology (tree regex)))
                     :group-map scanner-group-map
                     :group-list register-list))))


(defmethod scan ((sc regex-scanner) (target-string string) &key start end &allow-other-keys)
  (funcall (scanner sc) target-string (or start 0) (or end (length target-string))))


(defparameter +make-group-result-default+ (lambda (group-name group-info match)
                                            (list group-name group-info match)))
(defparameter *make-group-result-fun* nil)


;;; Returns assoc-list with matches and info
(defun match-to-assoc (str group-list group-map match-start match-end group-starts group-ends)
  (declare (ignore match-end))
  (when match-start
    (loop :for i :from 0 :to (1- (length group-list))
          :for group-name :in group-list
          :when (aref group-starts i)
          :collect (funcall (or *make-group-result-fun*
                                +make-group-result-default+)
                            group-name
                            (assoc group-name group-map :test #'equal)
                            (subseq str
                                    (aref group-starts i)
                                    (aref group-ends i))))))


;;; The following code is based on a simple assumption
;;; (that should really be tested somehow, in the case it changes overnight):
;;;
;;;   REGEX GROUPS THAT ARE PLACED LOWER IN THE TREE, WILL BE PLACED FARTHER IN THE MATCH INDEX ARRAY
;;;
;;; This assumption lets me interpret matches like (... #(0 0) #(42 42)) easily. First one tops.
;;; XXX, TODO: Do a proper testing, pls. It's long past due.


(defun filter-matched-groups (group-list group-starts group-ends)
  (declare (ignore group-ends))
  (loop :for group :in group-list
        :for group-start-index :across group-starts
        :when group-start-index
        :collect group))


(defun segment-string-with-match (str group-list group-map match-start match-end group-starts group-ends)
  (if (= (length group-starts) 0) ; no groups in scanner
    (list (list :piece str
                :start 0
                :end (length str)
                :groups nil))
    (let* ((separation-points (sort (remove nil (remove-duplicates (append (list match-start match-end)
                                                                           (coerce group-starts 'list)
                                                                           (coerce group-ends 'list))))
                                    #'<))
           (pieces (map 'list (lambda (start end)
                                (list :piece (subseq str start end)
                                      :start start
                                      :end end
                                      :groups nil))
                        separation-points
                        (rest separation-points)))
           (groups-ranges (mapcar (lambda (group start end)
                                    (list group start end))
                                  group-list
                                  (coerce group-starts 'list)
                                  (coerce group-ends 'list))))
      (loop :for (group-label start end) :in groups-ranges
            :do (loop :for piece :in pieces
                      :when (and (>= (getf piece :start) start)
                                 (<= (getf piece :end) end))
                      :do (push group-label (getf piece :groups))))
      pieces)))


;;; Updated algorithm is simpler in some aspects, and more complex in others.
;;; It is a variation of two-point merge for a tree (saved regex topology) and
;;; a list (labeled match results).
;;;
;;; Code will walk the regex tree, which *must* be saved, because there is no
;;; other way to preserve named groups topology. (counter-example is many empty
;;; named-groups at the same position, e.g. (... #(0 0 0..) #(0 0 0...)).
;;; No structure can be derived from that.)
;;;
;;; Simultaneously, code will iterate over the list of string segments, and
;;; compose them into a match tree.
;;;
;;; The core idea is the same as before, but I've dropped the assumption that
;;; just match indices could be enough. That will change the implementation
;;; details drastically.


(defun reconstruct-match-tree (group-map matched-groups tree-topology segment-list)
  (labels ((group-equal (one another) (string= one another))
           (path-equal (one another) (and (= (length one) (length another))
                                          (every #'group-equal one another)))
           (make-group-result (group-name tree) (let ((group-mapping (rest (assoc group-name group-map :test #'group-equal))))
                                                  (funcall (or *make-group-result-fun*
                                                               +make-group-result-default+)
                                                           (getf group-mapping :name)
                                                           (getf group-mapping :info)
                                                           tree)))
           ;; There will be groups that had a match (even an empty one) and ones that had not.
           ;; It is important to not even acknowledge the latter ones.
           (filter-nodes (branches) (remove-if-not (lambda (node)
                                                     (member (first node) matched-groups :test #'group-equal))
                                                   branches))
           (inner-function (tree-topology segment-list group-stack)
                           (loop
                             :with node-result
                             :with segments := segment-list
                             :for (group . subtree) :in (filter-nodes tree-topology)

                             ;; Try to grab the first segment for this group
                             :initially (let ((current-segment (first segments)))
                                          (when (and current-segment
                                                     (path-equal (getf current-segment :groups)
                                                                 group-stack))
                                            (push (getf current-segment :piece) node-result)
                                            (setf segments (rest segments))))

                             ;; Recurse further into the child node
                             :do (multiple-value-bind (call-result new-segments)
                                   (inner-function subtree segments (cons group group-stack))
                                   (push (make-group-result group call-result) node-result)
                                   (setf segments new-segments))

                             ;; Try to grab a splinter segment after each child
                             :do (let ((current-segment (first segments)))
                                   (when (and current-segment
                                              (path-equal (getf current-segment :groups)
                                                          group-stack))
                                     (push (getf current-segment :piece) node-result)
                                     (setf segments (rest segments))))

                             :finally (return-from inner-function (values (if node-result
                                                                            (nreverse node-result)
                                                                            (list ""))
                                                                          segments)))))
    (nth-value 0 (inner-function tree-topology segment-list nil))))


(defun match-to-group-tree (str group-list group-map regex-tree-topology match-start match-end group-starts group-ends)
  (let* ((matched-groups (filter-matched-groups group-list group-starts group-ends))
         (segments (segment-string-with-match str group-list group-map match-start match-end group-starts group-ends)))
    (reconstruct-match-tree group-map matched-groups regex-tree-topology segments)))


;; NOTE: Do not use for command parsing. If you're using it, you're doing it wrong.
;;       There are ready-made facilities for that already.
(defmacro scan-to-tree (scanner string)
  `(apply #'match-to-group-tree
          ,string
          (group-list ,scanner)
          (group-map ,scanner)
          (regex-tree-topology ,scanner)
          (multiple-value-list (d.regex:scan ,scanner
                                             ,string))))


;;; DEPRECATED

;; NOTE: Works only for groups with unique name
;;       Otherwise? Filter group list yourself.

#|
(defmacro get-group (name groups)
  `(rest (assoc (string ,name) ,groups :test #'string=)))


(defun is-group (name groups &rest options)
  (some (lambda (option)
          (string= (get-group name groups) option))
        options))


(defun list-group-names (groups)
  (map 'list #'car groups))
|#



(declaim (ftype (or (function (list-of-relaxed-regexes
                                &key
                                (:separator-regex relaxed-regex)
                                (:start-regex relaxed-regex)
                                (:end-regex relaxed-regex)
                                (:null-regex relaxed-regex)))
                    (function (list-of-relaxed-sampled-regexes
                                &key
                                (:separator-regex relaxed-sampled-regex)
                                (:start-regex relaxed-sampled-regex)
                                (:end-regex relaxed-sampled-regex)
                                (:null-regex relaxed-sampled-regex))))
                concat-separated))
(defun concat-separated (regexes &key
                                 ((:separator-regex sep-rx) nil)
                                 ((:start-regex start-rx) nil)
                                 ((:end-regex end-rx) nil)
                                 ((:null-regex null-rx) nil))
  "This function returns concatenation of regexes (either in form of
D.REGEX:REGEX, STRING or NIL) separated by SEPARATOR-REGEX. Separator can be
NIL. START-REGEX and END-REGEX are used respectively for the start and the end.
NULL-REGEX is used if all regexes are NIL."
  (let ((nonnil-regexes (remove nil regexes)))
    (if (null nonnil-regexes)
      null-rx
      (concat-many-relaxed
        (list
          start-rx
          (concat-many-relaxed
            (cons (first nonnil-regexes)
                  (loop :for regex :in (rest nonnil-regexes)
                        :collect sep-rx
                        :collect regex)))
          end-rx)))))


;; TBD: Does explicit support ab?c?|a?bc?|a?b?c
;;      It seems that it does. Does it?
;; TODO: Refactor this shit.
(declaim (ftype (or (function (list-of-relaxed-regexes
                                &key
                                (:explicit boolean)
                                (:separator-regex relaxed-regex)))
                    (function (list-of-relaxed-sampled-regexes
                                &key
                                (:explicit boolean)
                                (:separator-regex relaxed-sampled-regex))))
                optional-concat))
(defun optional-concat (regexes &key ((:explicit explicit) nil) ((:separator-regex sep-rx) nil))
  "DEPRECATED. Concat regexes as optionals, but with neat handling of separators.
:SEPARATOR-REGEX is trivially obvious.
:EXPLICIT makes it so *at least* one regex *must* match."
  (let ((nonnil-regexes (map 'list (lambda (rx)
                                     (typecase rx
                                       (regex rx)
                                       (string (regex-from-string rx))))
                             (remove nil regexes))))
    (when nonnil-regexes
      (combine-many-relaxed
        (loop :for regex-list := nonnil-regexes :then (rest regex-list)
              :while regex-list
              :collect (if (= (length regex-list) 1)
                         (if explicit
                           (first regex-list)
                           (make-optional (wrap-in-noncapturing-group (first regex-list))))
                         (concat-many-relaxed (loop :for regex :in regex-list
                                                    :for first-regex := t :then nil
                                                    :collect (if first-regex
                                                               regex
                                                               (concat-many-relaxed
                                                                 (list sep-rx (make-optional (wrap-in-noncapturing-group regex)))))))))))))
