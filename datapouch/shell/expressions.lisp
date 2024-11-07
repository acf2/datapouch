;;;; expressions.lisp
;;; Primitive command building system


(in-package :datapouch.shell)


;; Should always be a cons with two keywords
;; Like (:name . :type)
;; :name is used to refer to this particular argument
;; :type is used to determine subexpression that should be used
;; Also, :type is used as hashtable key, or subexpression id
(defun subexpression-argument? (arg)
  (and (consp arg)
       (eq (type-of (first arg)) 'keyword)
       (eq (type-of (rest arg)) 'keyword)))


(defun acceptable-argument? (arg)
  (or
    ;; Either argument is a valid term...
    (acceptable-term? arg)
    ;; ...or named subexpression type
    (subexpression-argument? arg)))


;(defun get-argument-identifier (arg)
;  (if (keyword-argument? arg)
;    arg
;    (let* ((group (cond ((argument-with-modifiers? arg) (first arg))
;                        ((simple-argument? arg) arg))))
;      (if (and group (named-regex-group? group))
;        (first group)
;        nil))))


(defun acceptable-expression-s-form? (form)
  (every #'acceptable-argument? form))


(defun acceptable-built-expression-s-form? (form)
  (every #'acceptable-term? form))


(defgeneric define-subexpression (shell id expression)
  (:documentation "Define an subexpression and assign it id in shell expression table"))


(defgeneric define-command (shell expression)
  (:documentation "Add command (complete expression) to shell list of commands"))


(defgeneric build-expression (expression dependencies)
  (:documentation "Transform expression with the use of some dependencies"))


(defgeneric build-shell (shell)
  (:documentation "Make shell expressions usable for transformation to reader macro commands"))


;;; handler should be a function that accepts only key arguments:
;;;  - one for each regex match group, that is provided only for this command (and not subexpressions, for example)
;;;  - one for result of each subexpression (across all s-forms/lexers), and/or &allow-other-keys
(defclass shell-expression ()
  ((s-forms :initarg :s-forms
            :reader s-forms)
   (handler :initarg :handler
            :reader handler)
   (docs :initarg :docs
         :reader docs)))


(defclass shell ()
  ((subexpressions :initform (make-hash-table)
                   :reader subexpressions)
   (complete-expressions :initform nil
                         :reader complete-expressions)))


(defparameter +add-shell-expression-form-error+ "Error when adding shell expression form: ~A~&")


(defmethod add-shell-expression-form ((expression shell-expression) expression-s-form &rest other-expression-s-forms)
  (let ((all-s-forms (cons expression-s-form other-expression-s-forms)))
    (if (every #'acceptable-expression-s-form? all-s-forms)
      (with-slots (s-forms) expression
        (loop :for new-s-form :in all-s-forms
              :do (push new-s-form s-forms))
        expression)
      (error (make-instance 'simple-error
                            :format-control +add-shell-expression-form-error+
                            :format-arguments (list (format nil "one of s-form is unacceptable: ~S" all-s-forms)))))))


(defparameter +make-shell-expression-error+ "Error when making shell expression: ~A~&")


(defun make-shell-expression (expression-s-forms handler &optional docs)
  (if (every #'acceptable-expression-s-form? expression-s-forms)
    (make-instance 'shell-expression
                   :s-forms expression-s-forms
                   :handler handler
                   :docs docs)
    (error (make-instance 'simple-error
                          :format-control +make-shell-expression-error+
                          :format-arguments (list (format nil "one of s-forms is unacceptable: ~S" expression-s-forms))))))


;;; Note: wanted to use (type-id keyword), but apparently CL cannot do that
;;;       maybe will revisit later
(defmethod define-subexpression ((shell shell) (type-id symbol) (expression shell-expression))
  (with-slots (subexpressions) shell
    (setf (gethash type-id subexpressions) expression)))


(defmethod define-command ((shell shell) (expression shell-expression))
  (with-slots (complete-expressions) shell
    (pushnew expression complete-expressions))
  expression)


(defclass built-shell-expression-shard ()
  ((s-form :initarg :s-form
            :reader s-form) ; _unwrapped_ s-form, NRG-only
   (wrapped-user-handler :initarg :handler
                         :reader handler)
   (docs :initarg :docs
         :reader docs)))


(defclass built-shell ()
  ((built-subexpression-shards :initform (make-hash-table)) ; hashtable<key, list>
   (built-complete-expression-shards :initform nil)))


(defparameter +shell-build-error+ "Expression wrappers cannot be built: ~A~&")


(defparameter +scope-separator+ "--")
(defparameter +full-subexpression-group+ :whole)


(defun get-scoped-name (expression-identifier argument-identifier)
  (intern (concatenate 'string
                       (symbol-name expression-identifier)
                       +scope-separator+
                       (symbol-name argument-identifier))
          "KEYWORD"))


;; This helps to eliminate collisions of arguments/group names
(defun scope-argument-names (scope-name argument-names)
  (map 'list (lambda (argument-name)
               (get-scoped-name scope-name argument-name))
       argument-names))


;; This helps to eliminate collisions of arguments/group names
(defun scope-nrgs-in-s-form (name s-form)
  (map 'list (lambda (term)
               (cond ((and (argument-with-modifiers? term)
                           (named-regex-group? (first term)))
                      (list* (cons (get-scoped-name name (car (first term)))
                                   (cdr (first term)))
                             (rest term)))
                     ((named-regex-group? term)
                      (cons (get-scoped-name name (car term))
                            (cdr term)))
                     (:else 
                       term)))
       s-form))


(defun get-s-form-subexpressions (s-form)
  (remove-if-not #'subexpression-argument?
                 s-form))


(defun get-s-form-nrgs (s-form)
  (list-existing* (map 'list (lambda (arg)
                               (let* ((group (cond ((argument-with-modifiers? arg) (first arg))
                                                   ((simple-argument? arg) arg))))
                                 (when (and group (named-regex-group? group))
                                   group)))
                       s-form)))


;; You have:
;; :dep-name (:arg1 :arg2)
;; You get:
;; ((:dep-name.arg1 :arg1)
;;  (:dep-name.arg1 .arg2))
;(defun make-arg-assoc (scope-name argument-names)
;  (loop :for argument-name :in argument-names
;        :collect (list (get-scoped-name scope-name argument-name)
;                       argument-name)))


(defparameter +shell-expression-build-error+ "Expression cannot be built: ~A~&")


(defun build-shard (expression-s-form user-handler docs deps-info)
  (let* ((immediate-arguments (map 'list #'car (get-s-form-nrgs expression-s-form)))
         (args-info (loop :for (name . dep-info) :in deps-info
                          :collect (list :name name
                                         :wrapped-handler (getf dep-info :wrapped-handler)
                                         :arguments (map 'list #'car (get-s-form-nrgs (getf dep-info :s-form))))))
         (s-form (loop :for argument :in expression-s-form
                       :append (cond ((subexpression-argument? argument)
                                      (let* ((name (car argument))
                                             (dep-info (rest (assoc name deps-info))))
                                        (scope-nrgs-in-s-form name (getf dep-info :s-form))))
                                     (:else (list argument))))))
    ;(format t "build-shard call:~&S-FORM: ~S~&ARGS INFO: ~S~&DEPS INFO: ~S~&~%" s-form args-info deps-info)
    (flet ((get-arg-p-list (get-value arg-keys) (loop :for arg :in arg-keys
                                                      :append (list arg (funcall get-value arg)))))
      (make-instance 'built-shell-expression-shard
                     :s-form s-form
                     :docs docs
                     :handler (lambda (&rest handler-arguments &key &allow-other-keys)
                                ;(format t "command call:~&S-FORM: ~S~&IMM-ARGS: ~S~&FUN ARGS: ~S~&ARGS INFO: ~S~&DEPS INFO: ~S~&~%" s-form immediate-arguments handler-arguments args-info deps-info)
                                (let ((subexpression-values 
                                        (loop :for arg-info :in args-info
                                              :append (let ((subexpr-name (getf arg-info :name))
                                                            (subexpr-args (getf arg-info :arguments))
                                                            (subexpr-wrapper (getf arg-info :wrapped-handler)))
                                                        (list subexpr-name
                                                              (apply subexpr-wrapper
                                                                     (get-arg-p-list (lambda (subexpr-arg)
                                                                                       (getf handler-arguments
                                                                                             (get-scoped-name subexpr-name subexpr-arg)))
                                                                                     subexpr-args)))))))
                                  ;(format t "SUBEXPR VALS: ~A~&" subexpression-values)
                                  (apply user-handler
                                         (append
                                           (get-arg-p-list (lambda (nrg)
                                                             (getf handler-arguments nrg))
                                                           immediate-arguments)
                                           subexpression-values))))))))


;; This sucks ass.
;; I know. Chill.
(defmethod build-expression ((expression shell-expression) (dependencies built-shell))
  (with-slots (built-subexpression-shards) dependencies
    (loop :for expression-s-form :in (s-forms expression)
          :append (let* ((subexpression-forms (get-s-form-subexpressions expression-s-form))
                         (variable-deps-info (loop :for (name . type) :in subexpression-forms
                                                   :for built-subexpression-list := (gethash type built-subexpression-shards)
                                                   :if built-subexpression-list
                                                   :collect (loop :for built-subexpression :in built-subexpression-list
                                                                  :collect (list name
                                                                                 :wrapped-handler (handler built-subexpression)
                                                                                 :type type
                                                                                 :s-form (s-form built-subexpression)))
                                                   :else
                                                   :do (error (make-instance 'simple-error
                                                                             :format-control +shell-expression-build-error+
                                                                             :format-arguments (format nil "~A dependency is not found" type))))))
                    ;(format t "build-expression iteration~&S-FORM: ~S~&VDEPS INFO: ~S~&~%" expression-s-form variable-deps-info)
                    (if (null variable-deps-info)
                      (list (build-shard expression-s-form
                                         (handler expression)
                                         (docs expression)
                                         nil))
                      (loop :for deps-info-variant :in (d.aux:cartesian-product variable-deps-info)
                            :collect (build-shard expression-s-form
                                                  (handler expression)
                                                  (docs expression)
                                                  deps-info-variant)))))))


(defmethod build-shell ((shell shell))
  (with-slots (complete-expressions subexpressions) shell
    (flet ((get-deps-types (expr) (remove-duplicates (map 'list #'rest (loop :for s-form :in (s-forms expr)
                                                                             :append (get-s-form-subexpressions s-form)))
                                                     :from-end t)))
      (let ((subexpression-keys (d.aux:get-keys-from-hash-table subexpressions)))
        ;; Check subexpression graph for cycles
        ;; Complete expressions cannot form cycles, because no expression could depend on them
        (if (d.aux:check-directed-graph-for-cycles :vertices subexpression-keys
                                                   :get-adjacent (lambda (expression-key)
                                                                   (let ((expr (gethash expression-key subexpressions)))
                                                                     (when expr
                                                                       (get-deps-types expr)))))
          (error (make-instance 'simple-error
                                :format-control +shell-build-error+
                                :format-arguments "A cycle is found in shell-expression dependency graph"))
          (let ((built-shell (make-instance 'built-shell)))
            (with-slots (built-complete-expression-shards built-subexpression-shards) built-shell
              (labels ((prepare (expression) (let ((subexpr-types (get-deps-types expression)))
                                               (loop :for type :in subexpr-types
                                                     :for built-subexpr := (gethash type built-subexpression-shards)
                                                     :unless built-subexpr
                                                     :do (let ((subexpr (gethash type subexpressions)))
                                                           (if (null subexpr)
                                                             (error (make-instance 'simple-error
                                                                                   :format-control +shell-build-error+
                                                                                   :format-arguments (format nil "No subexpression is found with key ~S" type)))
                                                             (setf (gethash type built-subexpression-shards)
                                                                   (prepare (gethash type subexpressions))))))
                                               (build-expression expression built-shell))))
                (setf built-complete-expression-shards
                      (loop :for complete-expression :in complete-expressions
                            :append (prepare complete-expression)))))
            built-shell))))))


(defparameter +generate-commands-from-shell-error+ "Error during generation of commands from shell: ~A~&")


(defgeneric generate-commands-from-shell (shell)
  (:documentation "Unwrap and substitute all expressions and make lambdas for reader-macro commands utility"))


(defmethod generate-commands-from-shell ((built-shell built-shell))
  (flet ((groups-names-as-symbols (groups) (map 'list (lambda (symbol-name)
                                                        (intern symbol-name "KEYWORD"))
                                                (d.regex:list-group-names groups))))
    (with-slots (built-complete-expression-shards) built-shell
      ;(format t "generate-commands-from-shell~&")
      ;(loop :for shard :in built-complete-expression-shards
            ;:do (format t "shard: ~S ~S~&" (s-form shard) (docs shard)))
      ;(format t "~%")
      (loop :for shard :in built-complete-expression-shards
            :for s-form := (s-form shard)
            :for parser := (apply #'make-command-s-form-scanner s-form)
            :for handler := (handler shard)
            :for proxy-command := (let ((local-handler handler))
                                    (lambda (string match)
                                      (declare (ignore string))
                                      ;(format t "Called command:~&Args: ~S~&"
                                      ;       (loop :for group-name :in (groups-names-as-symbols match)
                                      ;             :append (list group-name
                                      ;                           (d.regex:get-group group-name match))))
                                      (apply local-handler
                                             (loop :for group-name :in (groups-names-as-symbols match)
                                                   :append (list group-name
                                                                 (d.regex:get-group group-name match))))))
            :collect (make-instance 'd.rmacro:command
                                    :regex parser
                                    :handler proxy-command)))))


(defmethod generate-commands-from-shell ((shell shell))
  (generate-commands-from-shell (build-shell shell)))


(defmacro add-shell-subexpressions (shell &rest expr-defs)
  `(progn
     ,@(loop :for (type-key s-forms user-handler docs) :in expr-defs
             :collect `(define-subexpression ,shell
                                             ,type-key
                                             (make-shell-expression ',s-forms ,user-handler ,docs)))))


(defmacro add-shell-commands (shell &rest cmd-defs)
  `(progn
     ,@(loop :for (s-forms user-handler docs) :in cmd-defs
             :collect `(define-command ,shell
                                       (make-shell-expression ',s-forms ,user-handler ,docs)))))


;(defmacro create-shell-commands (&rest cmd-defs)
;t `(list ,@(loop :for all-cmds := cmd-defs :then (cdddr all-cmds)
;                 :for cmd-rx := (first all-cmds)
;                 :for cmd-handler := (second all-cmds)
;                 :for cmd-docs := (third all-cmds)
;                 :while all-cmds
;                 :collect `(make-shell-command ,cmd-rx ,cmd-handler ,cmd-docs))))
