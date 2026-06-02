;;;; auxiliary.lisp

(in-package :datapouch.auxiliary)


(defparameter *debug-output* nil)


(defun list-existing (&rest rest)
  (remove nil rest))


(defmacro list-existing* (&rest rest)
  `(remove nil (list* ,@rest)))


(defun ensure-list (entity)
  (if (listp entity)
    entity
    (list entity)))


(defmacro append-lists (list-of-lists)
  `(reduce #'append ,list-of-lists))


(defmacro map-append (func list)
  `(reduce #'append (map 'list ,func ,list)))


;;; Transpose lists
;;; https://stackoverflow.com/a/3513158
(defun rotate (list-of-lists)
  (if (or (null list-of-lists)
          (and (null (car list-of-lists))
               (null (cdr list-of-lists))))
    list-of-lists
    (apply #'map 'list #'list list-of-lists)))


;; Taken from lisp cookbook
;; https://lispcookbook.github.io/cl-cookbook/type.html#declaring-the-type-of-variables
(defun list-of-strings-p (list)
  "Return t if LIST is non nil and contains only strings."
  (and (consp list)
       (every #'stringp list)))


(deftype list-of-strings ()
  `(satisfies list-of-strings-p))


(defun list-of-list-of-strings-p (list)
  "Return t if LIST is non nil and contains only lists of strings."
  (and (consp list)
       (every #'list-of-strings-p list)))


(deftype list-of-list-of-strings ()
  `(satisfies list-of-list-of-strings-p))


(defmacro repeat-string (times str)
  `(format nil "~V@{~A~:*~}" ,times ,str))


(declaim (ftype (function (string string &key (:test function))) prefix?))
(defun prefix? (prefix str &key ((:test test) #'equal))
  (funcall test prefix (subseq str 0 (min (length str) (length prefix)))))


(defun common-prefix (list &key ((:test test) #'eql))
  (let ((prefix-length (loop :for tuple in (d.aux:rotate list)
                             :while (apply test tuple)
                             :counting t)))
    (and list (subseq (first list) 0 prefix-length))))


(declaim (ftype (function ((or null list-of-strings) &key (:test function))) common-string-prefix))
(defun common-string-prefix (list-of-strings &key ((:test test) #'char=))
  (common-prefix list-of-strings :test test))

 
(defun add-to-assoc! (assoc key elem &key ((:test test) #'equal))
  (let ((cell (assoc key assoc :test test)))
    (if cell
      (progn
        (rplacd cell (pushnew elem (cdr cell) :test test))
        assoc)
      (cons (list key elem) assoc))))


(defmacro member-of (lst &rest rest &key &allow-other-keys)
  (let ((element (gensym)))
    `(lambda (,element)
       (member ,element ,lst ,@rest))))


(defun concat-keyword (&rest symbols)
  (intern (apply #'concatenate 'string
                 symbols)
          "KEYWORD"))


(defun get-keys-from-hash-table (hash-table)
  (loop :for k :being :the :hash-key :in hash-table
        :collect k))


(defun check-directed-graph-for-cycles (&key ((:vertices vertices) nil)
                                             ((:get-adjacent get-adjacent) #'identity)
                                             ((:test test) #'eq))
  (declare (type list vertices)
           (type function get-adjacent test))
  (if (null vertices)
    nil
    (loop :for enabled-vertices := (copy-list vertices) :then (delete-if (member-of graph-component-part
                                                                                    :test test)
                                                                         enabled-vertices)
          :while enabled-vertices
          :for graph-component-part := (loop :for dfs-deque := (list (first enabled-vertices)) :then (append vertices-to-check dfs-deque)
                                             :for marked-vertices := nil :then (push current-vertex marked-vertices)
                                             :for current-vertex := (when dfs-deque (pop dfs-deque))
                                             :for vertices-to-check := (remove-if-not (member-of enabled-vertices
                                                                                                 :test test)
                                                                                      (funcall get-adjacent current-vertex))
                                             :unless current-vertex :return marked-vertices
                                             :when (some (member-of marked-vertices
                                                                    :test test)
                                                         vertices-to-check)
                                             :do (return-from check-directed-graph-for-cycles t)))))

;; ((1) (2 3) (4 5 6)) -> ((1 2 4) (1 2 5) (1 2 6)
;;                         (1 3 4) (1 3 5) (1 3 6))
(defun cartesian-product (sets &optional (transform-fun nil) (path nil))
  (if (null (rest sets))
    (loop :for e in (first sets)
          :collect (if transform-fun
                     (apply transform-fun (append path (list e)))
                     (append path (list e))))
    (loop :for e in (first sets)
          :append (cartesian-product (rest sets) transform-fun (append path (list e))))))


(defun traverse (tree predicate-fun transform-fun &optional (recurse-after-transform-fun nil))
  "Generic, but not very sophisticated traverse function to implement macros."
  (cond ((funcall predicate-fun tree)
         (multiple-value-bind (form-value new-form) (funcall transform-fun tree)
           (if (null recurse-after-transform-fun)
             (values (list form-value) new-form)
             (multiple-value-bind (new-form-values traversed-new-form)
               (traverse (funcall recurse-after-transform-fun new-form)
                         predicate-fun
                         transform-fun
                         recurse-after-transform-fun)
               (values (cons form-value new-form-values)
                       traversed-new-form)))))
        ((listp tree)
         (let ((subtree-result (rotate (map 'list (lambda (subtree)
                                                    (multiple-value-list (traverse subtree
                                                                                   predicate-fun
                                                                                   transform-fun
                                                                                   recurse-after-transform-fun)))
                                            tree))))
           (values (reduce #'append (remove-if #'null (first subtree-result)))
                   (second subtree-result))))
        (:else (values nil tree))))
