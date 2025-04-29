;;;; shell.lisp
;;; Primitive command building system


(in-package :datapouch.shell)


(defclass expression-config ()
  ((use-nongroup-arguments :initarg :use-nongroup-arguments
                      :initform nil
                      :accessor use-nongroup-arguments)
   (allow-traversal :initarg :allow-traversal
                    :initform t
                    :accessor allow-traversal)))


(defclass expression ()
  ((name :initarg :name
         :reader name)
   (regex-group :initarg :regex-group
                :reader regex-group)
   (handler :initarg :handler
            :reader handler)
   (config :initarg :config
           :reader config)))


(defun create-expression (name regex user-handler &key (use-nongroup-arguments nil) (allow-traversal t))
  (declare (type (or string keyword) name)
           (type (or string d.regex:regex) regex)
           (type function user-handler)
           (type boolean use-nongroup-arguments allow-traversal))
  (make-instance 'expression
                 :name (string name)
                 :regex-group (d.regex:make-named-group (string name)
                                                        regex)
                 :handler user-handler
                 :config (make-instance 'expression-config
                                        :use-nongroup-arguments use-nongroup-arguments
                                        :allow-traversal allow-traversal)))


(defgeneric set-expression (lexicon name regex user-handler &key use-nongroup-arguments allow-traversal)
  (:documentation "ABRACADABRA"))


(defgeneric get-expression (lexicon name)
  (:documentation "ABRACADABRA"))


(defgeneric group-tree-traversal (lexicon group-tree)
  (:documentation "ABRACADABRA"))


(defclass lexicon ()
  ((expression-lookup :initform (make-hash-table :test #'equal))))


(defmethod set-expression ((lexicon lexicon) name regex user-handler &key (use-nongroup-arguments nil) (allow-traversal t))
  (declare (type (or string keyword) name)
           (type (or string d.regex:regex) regex)
           (type function user-handler)
           (type boolean use-nongroup-arguments allow-traversal))
  (with-slots (expression-lookup) lexicon
    (setf (gethash (string name) expression-lookup)
          (create-expression name
                             regex
                             user-handler
                             :use-nongroup-arguments use-nongroup-arguments
                             :allow-traversal allow-traversal))))



(defmethod get-expression ((lexicon lexicon) name)
  (declare (type (or string keyword) name))
  (with-slots (expression-lookup) lexicon
    (gethash (string name) expression-lookup)))


(defun processed-group? (lst)
  (and (listp lst)
       (eq (type-of (first lst)) 'keyword)
       (= (length lst) 2)))


(defun funcall-group-list-with-filtering (lexicon handler group-tree &key use-nongroup-arguments allow-traversal)
  (let* ((all-arguments (if allow-traversal
                          (map 'list (lambda (term)
                                       (group-tree-traversal lexicon term))
                               group-tree)
                          group-tree))
         (filtered-arguments (if use-nongroup-arguments 
                               all-arguments
                               (reduce #'append (remove-if-not #'processed-group? all-arguments)))))
    (apply handler filtered-arguments)))


(defmethod group-tree-traversal ((lexicon lexicon) group-tree)
  (if (not (listp group-tree))
    group-tree
    (let ((expression (when (eq (first group-tree) :group)
                        (get-expression lexicon (second group-tree)))))
      (if expression
        (funcall-group-list-with-filtering lexicon
                                           (handler expression)
                                           (cddr group-tree)
                                           :allow-traversal (allow-traversal (config expression))
                                           :use-nongroup-arguments (use-nongroup-arguments (config expression)))
        (map 'list (lambda (term)
                     (group-tree-traversal lexicon term))
             group-tree)))))


(defun set-expressions (lexicon &rest expression-definitions)
  (loop :for expression-definition :in expression-definitions
        :do (apply #'set-expression lexicon expression-definition)))


(defun make-command-handler (lexicon user-handler &key (use-nongroup-arguments nil) (allow-traversal t))
  (lambda (group-tree)
    (funcall-group-list-with-filtering lexicon
                                       user-handler
                                       group-tree
                                       :allow-traversal allow-traversal
                                       :use-nongroup-arguments use-nongroup-arguments)))


(defun make-command (lexicon regex handler &rest other &key &allow-other-keys)
  (make-instance 'd.rmacro:command
                 :regex regex
                 :handler (apply #'make-command-handler lexicon handler other)))


(defun make-commands (lexicon &rest command-definitions)
  (loop :for command-definition :in command-definitions
        :collect (apply #'make-command lexicon command-definition)))
