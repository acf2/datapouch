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


(defmacro repeat-string (times str)
  `(format nil "~V@{~A~:*~}" ,times ,str))


(defmacro member-of (lst &rest rest &key &allow-other-keys)
  (let ((element (gensym)))
    `(lambda (,element)
       (member ,element ,lst ,@rest))))


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
(defun cartesian-product (sets &optional (path nil))
  (if (null (rest sets))
    (loop :for e in (first sets)
          :collect (append path (list e)))
    (loop :for e in (first sets)
          :append (cartesian-product (rest sets) (append path (list e))))))
