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


;; TODO
;; make one big command, that can incapsulate every movement command:
;;  -> if it's on its own, then move (separate complete expression with only subexpression as it's contents)
;;  -> if it's part of a bigger command - use returned value
;; then command groups? (aggregate regexes into one big variant?)


(defgeneric define-subexpression (shell id expression)
  (:documentation "Define an subexpression and assign it id in shell expression table"))


(defgeneric define-command (shell expression)
  (:documentation "Add command (complete expression) to shell list of commands"))


(defgeneric build-expression (expression dependencies)
  (:documentation "Transform expression with the use of some dependencies"))


(defgeneric build-shell (shell)
  (:documentation "Make shell expressions usable for transformation to reader macro commands"))


;;; handler should be a function that accepts:
;;;  one positional argument:
;;;    - the whole command string
;;;  key arguments:
;;;    - one for each regex match group, that is provided only for this command (and not subexpressions, for example)
;;;    - one for result of each subexpression (across all s-forms/lexers), and/or &allow-other-keys
(defclass shell-expression ()
  ((s-form :initarg :s-form
           :reader s-form)
   (handler :initarg :handler
            :reader handler)
   (docs :initarg :docs
         :reader docs)))


(defclass shell ()
  ((subexpressions :initform (make-hash-table)
                   :reader subexpressions)
   (complete-expressions :initform nil
                         :reader complete-expressions)))


;(defmethod add-shell-expression-form ((expression shell-expression) expression-s-form &rest other-expression-s-forms)
;  (let ((all-s-forms (cons expression-s-form other-expression-s-forms)))
;    (if (every #'acceptable-expression-s-form? all-s-forms)
;      (with-slots (s-forms lexers) expression
;        (loop :for new-s-form :in all-s-forms
;              :do (push new-s-form s-forms))
;        expression)
;      (error (make-instance 'simple-error
;                            :format-control +s-form-errors+
;                            :format-arguments 'add-shell-expression-form)))))


(defun make-shell-expression (expression-s-form handler &optional docs)
  (when (acceptable-expression-s-form? expression-s-form)
    (make-instance 'shell-expression
                   :s-form expression-s-form
                   :handler handler
                   :docs docs)))


;;; Note: wanted to use (type-id keyword), but apparently CL cannot do that
;;;       maybe will revisit later
(defmethod define-subexpression ((shell shell) (type-id symbol) (expression shell-expression))
  (with-slots (subexpressions) shell
    (setf (gethash type-id subexpressions) expression)))


(defmethod define-command ((shell shell) (expression shell-expression))
  (with-slots (complete-expressions) shell
    (pushnew expression complete-expressions))
  expression)


(defclass built-shell-expression ()
  ((s-form :initarg :s-form
           :reader s-form) ; _unwrapped_ s-form, NRG-only
   (wrapped-user-handler :initarg :handler
                         :reader handler)
   (docs :initarg :docs
         :reader docs)))


(defclass built-shell ()
  ((built-subexpressions :initform (make-hash-table))
   (built-complete-expressions :initform nil)))


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
               (if (named-regex-group? term)
                 (cons (get-scoped-name name (car term))
                       (cdr term))
                 term))
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


;; This sucks ass.
;; I know. Chill.
(defmethod build-expression ((expression shell-expression) (dependencies built-shell))
  (with-slots (built-subexpressions) dependencies
    (let* ((subexpression-forms (get-s-form-subexpressions (s-form expression)))
           (immediate-arguments (map 'list #'car (get-s-form-nrgs (s-form expression))))
           (deps-info (loop :for (name . type) :in subexpression-forms
                            :for built-subexpression := (gethash type built-subexpressions)
                            :if built-subexpression
                            :collect (list name
                                           :wrapped-handler (handler built-subexpression)
                                           :type type
                                           :s-form (s-form built-subexpression))
                            :else
                            :do (error (make-instance 'simple-error
                                                      :format-control +shell-expression-build-error+
                                                      :format-arguments (format nil "~A dependency is not found" type)))))
           (args-info (loop :for (name . dep-info) :in deps-info
                            :collect (list :name name
                                           :wrapped-handler (getf dep-info :wrapped-handler)
                                           :arguments (map 'list #'car (get-s-form-nrgs
                                                                         (getf dep-info :s-form))))))
           (s-form (loop :for argument :in (s-form expression)
                         :append (cond ((subexpression-argument? argument)
                                        (let* ((name (car argument))
                                               (dep-info (rest (assoc name deps-info))))
                                          (scope-nrgs-in-s-form name (getf dep-info :s-form))))
                                       (:else (list argument))))))
      (format t "build-expression call:~&S-FORM: ~S~&ARGS INFO: ~S~&DEPS INFO: ~S~&~%" s-form args-info deps-info)
      (flet ((get-arg-p-list (get-value arg-keys) (loop :for arg :in arg-keys
                                                        :append (list arg (funcall get-value arg)))))
        (make-instance 'built-shell-expression
                       :s-form s-form
                       :docs (docs expression)
                       :handler (lambda (&rest handler-arguments &key &allow-other-keys)
                                  (format t "command call:~&S-FORM: ~S~&IMM-ARGS: ~S~&FUN ARGS: ~S~&ARGS INFO: ~S~&DEPS INFO: ~S~&~%" s-form immediate-arguments handler-arguments args-info deps-info)
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
                                    (format t "SUBEXPR VALS: ~A~&" subexpression-values)
                                    (apply (handler expression)
                                           (append
                                             (get-arg-p-list (lambda (nrg)
                                                               (getf handler-arguments nrg))
                                                             immediate-arguments)
                                             subexpression-values)))))))))


(defmethod build-shell ((shell shell))
  (with-slots (complete-expressions subexpressions) shell
    (let ((subexpression-keys (d.aux:get-keys-from-hash-table subexpressions)))
      ;; Check subexpression graph for cycles
      ;; Complete expressions cannot form cycles, because no expression could depend on them
      (if (d.aux:check-directed-graph-for-cycles :vertices subexpression-keys
                                                 :get-adjacent (lambda (expression-key)
                                                                 (let ((expr (gethash expression-key subexpressions)))
                                                                   (when expr
                                                                     (map 'list #'rest (get-s-form-subexpressions (s-form expr)))))))
        (error (make-instance 'simple-error
                              :format-control +shell-build-error+
                              :format-arguments "A cycle is found in shell-expression dependency graph"))
        (let ((built-shell (make-instance 'built-shell)))
          (with-slots (built-complete-expressions built-subexpressions) built-shell
            (labels ((prepare (expression) (let ((subexpr-pairs (get-s-form-subexpressions (s-form expression))))
                                             (loop :for (name . type) :in subexpr-pairs
                                                   :for built-subexpr := (gethash type built-subexpressions)
                                                   :unless built-subexpr
                                                   :do (setf (gethash type built-subexpressions)
                                                             (prepare (gethash type subexpressions))))
                                             (build-expression expression built-shell))))
              (loop :for complete-expression :in complete-expressions
                    :do (push (prepare complete-expression)
                              built-complete-expressions))))
          built-shell)))))


(defparameter +generate-commands-from-shell-error+ "Error during generation of commands from shell: ~A~&")


(defgeneric generate-commands-from-shell (shell)
  (:documentation "Unwrap and substitute all expressions and make lambdas for reader-macro commands utility"))


(defmethod generate-commands-from-shell ((built-shell built-shell))
  (flet ((groups-names-as-symbols (groups) (map 'list (lambda (symbol-name)
                                                        (intern symbol-name "KEYWORD"))
                                                (d.regex:list-group-names groups))))
    (with-slots (built-complete-expressions) built-shell
      (format t "generate-commands-from-shell~&")
      (loop :for built-complete-expression :in built-complete-expressions
            :do (format t "BE: ~A ~A~&" (s-form built-complete-expression) (docs built-complete-expression)))
      (format t "~%")
      (loop :for built-complete-expression :in built-complete-expressions
            :for s-form := (s-form built-complete-expression)
            :for parser := (apply #'make-command-s-form-scanner s-form)
            :for handler := (handler built-complete-expression)
            :for proxy-command := (let ((local-handler handler))
                                    (lambda (string match)
                                      (declare (ignore string))
                                      (format t "Called command:~&Args: ~S~&"
                                              (loop :for group-name :in (groups-names-as-symbols match)
                                                    :append (list group-name
                                                                  (d.regex:get-group group-name match))))
                                      (apply local-handler
                                             (loop :for group-name :in (groups-names-as-symbols match)
                                                   :append (list group-name
                                                                 (d.regex:get-group group-name match))))))
            :collect (make-instance 'd.rmacro:command
                                    :regex parser
                                    :handler proxy-command)))))


(defmethod generate-commands-from-shell ((shell shell))
  (generate-commands-from-shell (build-shell shell)))


;(defmacro create-shell-commands (&rest cmd-defs)
;  `(list ,@(loop :for all-cmds := cmd-defs :then (cdddr all-cmds)
;                 :for cmd-rx := (first all-cmds)
;                 :for cmd-handler := (second all-cmds)
;                 :for cmd-docs := (third all-cmds)
;                 :while all-cmds
;                 :collect `(make-shell-command ,cmd-rx ,cmd-handler ,cmd-docs))))
