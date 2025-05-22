;;;; expressions.lisp
;;; Primitive command building system


(in-package :datapouch.expressions)


(defclass expression-config ()
  ((use-nongroup-arguments :initarg :use-nongroup-arguments
                           :type boolean
                           :initform nil
                           :accessor use-nongroup-arguments)
   (allow-traversal :initarg :allow-traversal
                    :type boolean
                    :initform t
                    :accessor allow-traversal))
  (:documentation
    "EXPRESSION-CONFIG objects contain different settings for GROUP-TREE-TRAVERSAL.
From user viewpoint, they modify expression behavior in some way.

USE-NONGROUP-ARGUMENTS, default NIL: Usually expression passes only some of
the regex match to it's handler. Passable elements are determined by
predicate PROCESSED-GROUP?, defined below. Such elements are not expected
to appear inside 'raw' regex match, but rather produced by other
expressions, that were called before current one. This setting removes this
restriction, and forces expression to pass all the content of a match to
user handler. In turn, user handler now must handle all possible arguments,
not only neat keyword ones.

Example:
  Processed group list before user handler call: (1 2 '(:num 3) 4)
  Call with (EQ USE-NONGROUP-ARGUMENTS NIL):
    (FUNCALL user-handler :num 3)
  Call with (EQ USE-NONGROUP-ARGUMENTS T):
    (FUNCALL user-handler 1 2 '(:num 3) 4)

ALLOW-TRAVERSAL, default T. Allows for GROUP-TREE-TRAVERSAL to try to
process each of the elements of regex match with other expressions, before
passing it to current expression. Setting it to NIL effectively disables
GROUP-TREE-TRAVERSAL recursive calls for this specific expression."))


(defclass expression ()
  ((expression-type :initarg :expression-type
                    :type string
                    :reader expression-type)
   (regex-group :initarg :regex
                :type d.regex:regex)
   (user-handler :initarg :user-handler
                 :type function
                 :reader user-handler)
   (config :initarg :config
           :type expression-config
           :reader config)
   (docs :initarg :docs
         :type string
         :reader docs))
  (:documentation
    "EXPRESSION objects are used to represent reusable regular expressions with
their respective handler functions.

EXPRESSION-NAME is both the id of expression, which later could be used in
hash-table to find it, and the name of named regex group, which will be
wrapped around regex passed by user.

REGEX-GROUP is the regex of the expression. The named group is not created when
set, but rather when user requires it. For each call of GET-NAMED-REGEX-GROUP
a unique info could be passed along with regex.

USER-HANDLER is user handler. It should be able to handle a regex match of
REGEX-GROUP. By default it must be a function, that accepts one positional
(regex group info) and other keyword arguments, but changing CONFIG may change
that. Return value could be anything, but checking the documentation of
PROCESSED-GROUP? is highly recommended.

CONFIG is the settings for GROUP-TREE-TRAVERSAL function.

DOCUMENTATION is self-explanatory."))


(defgeneric get-named-regex-group (expression &optional info)
  (:documentation "Get named regex group from an EXPRESSION instance."))


(declaim (ftype (function ((or string keyword)
                           (or string d.regex:regex)
                           function
                           string
                           &key
                           (:use-nongroup-arguments boolean)
                           (:allow-traversal boolean)))
                create-expression))
(defun create-expression (expression-type regex user-handler docs &key (use-nongroup-arguments nil) (allow-traversal t))
  "CREATE-EXRESSION eliminates some boilerplate for user, when creating new
expression, and checks argument types. For the meaning of arguments refer to
EXPRESSION and EXPRESSION-CONFIG documentation."
  (make-instance 'expression
                 :expression-type (string expression-type)
                 :regex regex
                 :user-handler user-handler
                 :docs docs
                 :config (make-instance 'expression-config
                                        :use-nongroup-arguments use-nongroup-arguments
                                        :allow-traversal allow-traversal)))


(defclass lexicon ()
  ((expression-lookup :initform (make-hash-table :test #'equal)))
  (:documentation
    "LEXICON objects serve as dictionaries, lookup tables for expressions.
Could be interpreted as a context for GROUP-TREE-TRAVERSAL function."))


(defgeneric set-expression (lexicon expression-type regex user-handler docs &key use-nongroup-arguments allow-traversal)
  (:documentation
    "Set expression in lexicon to new value. Expression is superseded, if
(EQUAL old-type new-type)."))


(defgeneric get-expression (lexicon expression-type)
  (:documentation "Get expression from lexicon, that is identified by EXPRESSION-TYPE."))


(defgeneric group-tree-traversal (lexicon group-tree)
  (:documentation "Generic for group-tree traversal. Refer to methods for documentation."))


(defmethod set-expression ((lexicon lexicon) expression-type regex user-handler docs &key (use-nongroup-arguments nil) (allow-traversal t))
  (declare (type (or string keyword) expression-type)
           (type (or string d.regex:regex) regex)
           (type function user-handler)
           (type boolean use-nongroup-arguments allow-traversal))
  "Method for LEXICON objects. Checks argument types."
  (with-slots (expression-lookup) lexicon
    (setf (gethash (string expression-type) expression-lookup)
          (create-expression expression-type
                             regex
                             user-handler
                             docs
                             :use-nongroup-arguments use-nongroup-arguments
                             :allow-traversal allow-traversal))))


(defmethod get-expression ((lexicon lexicon) expression-type)
  (declare (type (or string keyword) expression-type))
  "Method for LEXICON objects. Checks argument types."
  (with-slots (expression-lookup) lexicon
    (gethash (string expression-type) expression-lookup)))


(defmethod get-named-regex-group ((expression expression) &optional info)
  (with-slots (expression-type regex-group) expression
    (d.regex:make-named-group expression-type
                              regex-group
                              info)))


(declaim (ftype (function (lexicon (or string keyword) &optional t)) get-from-lexicon))
(defun get-from-lexicon (lexicon expression-type &optional info)
  "A shortcut function to get appropriate regex group from lexicon with chosen
contextual info."
  (let ((expression (get-expression lexicon expression-type)))
    (when expression
      (get-named-regex-group expression info))))


(defclass named-result ()
  ((name :initarg :name
         :accessor result-name
         :type keyword)
   (value :initarg :value
          :accessor result-value
          :type t)))


(declaim (ftype (function ((or keyword string) t)) make-result))
(defun make-result (name value)
  "This function formattes the result of an expression to be recognised by
another expression with USE-NONGROUP-ARGUMENTS set to NIL."
  (make-instance 'named-result
                 :name name
                 :value value))


(declaim (ftype (function ((or keyword string))) return-match))
(defun return-match (name)
  "RETURN-MATCH makes a function, that simply returns single term match for
expressions with USE-NONGROUP-ARGUMENTS."
  (lambda (_ arg)
    (declare (ignore _))
    (make-result name arg)))


(declaim (ftype (function (keyword t)) return-named-match))
(defun return-named-match (name arg)
  "This function returns single term match, naming it with a keyword, passed in regex info."
  (make-result name arg))


(declaim (ftype (function (lexicon function t t &key (:use-nongroup-arguments boolean) (:allow-traversal boolean)))
                funcall-group-list-with-filtering))
(defun funcall-group-list-with-filtering (lexicon user-handler group-info group-tree &key use-nongroup-arguments allow-traversal)
  "A reusable piece of code, that incapsulates expression config checks and
  corresponding transformations."
  (let* ((all-arguments (if allow-traversal
                          (map 'list (lambda (term)
                                       (group-tree-traversal lexicon term))
                               group-tree)
                          group-tree))
         (filtered-arguments (if use-nongroup-arguments 
                               all-arguments
                               (reduce #'append (map 'list
                                                     (lambda (named-result)
                                                       (list (result-name named-result)
                                                             (result-value named-result)))
                                                     (remove-if-not (lambda (term)
                                                                      (typep term 'named-result))
                                                                    all-arguments))))))
    (apply user-handler group-info filtered-arguments)))


(defmethod group-tree-traversal ((lexicon lexicon) group-tree)
  "This is the GROUP-TREE-TRAVERSAL for LEXICON class.
Processing goes like this:
1) If it's just a term, return it.
2) If it's a list, then it was a group in regex.
2.1) If corresponding expression is found in lexicon, then more:
2.1.1) If traversal is allowed, then recurse for each of the elements first.
2.1.2) Then, if only group arguments is allowed - filter all other arguments.
2.1.3) And only then make a call to user-handler with user info and these
arguments.
2.2) If corresponding expression is not found, then it's a free, 'dangling'
group. Leave it as is, but recurse further."
  (if (not (listp group-tree)) ; List means it was a (named) group
    group-tree
    (let ((expression (get-expression lexicon (first group-tree))))
      (if expression
        (funcall-group-list-with-filtering lexicon
                                           (user-handler expression)
                                           (second group-tree)
                                           (third group-tree)
                                           :allow-traversal (allow-traversal (config expression))
                                           :use-nongroup-arguments (use-nongroup-arguments (config expression)))
        ;; Sometimes it's just a group, and have no attached handler.
        ;; Leave everything as intact as it could be, but recurse further.
        (list (first group-tree)
              (second group-tree)
                (map 'list (lambda (term)
                             (group-tree-traversal lexicon term))
                     (third group-tree)))))))


(declaim (ftype (function (lexicon function &key (:use-nongroup-arguments boolean) (:allow-traversal boolean)))
                make-command-handler))
(defun make-command-handler (lexicon user-handler &key (use-nongroup-arguments nil) (allow-traversal t))
  "This function is used to create handlers for D.RMACRO:COMMAND class.
When creating your own commands, this is the question - how to connect them to
lexicons, and what to do with COMMAND janky interface. This solves both. It
kinda emulates the call that GROUP-TREE-TRAVERSAL does, but only for one
function - top user handler of a command. And returning lambda has the needed
signature to be used immediately in D.RMACRO:COMMAND creation.
Can only be used for commands in GROUP-MODE and with disabled FULL-STRING use -
this should be the default."
  (lambda (group-tree)
    (funcall-group-list-with-filtering lexicon
                                       (lambda (_ &rest rest)
                                         (declare (ignore _))
                                         (apply user-handler rest))
                                       nil
                                       group-tree
                                       :allow-traversal allow-traversal
                                       :use-nongroup-arguments use-nongroup-arguments)))


(defun make-command (lexicon regex handler docs &rest other &key &allow-other-keys)
  "This function wraps D.RMACRO:COMMAND creation with the use of MAKE-COMMAND-HANDLER in
one call."
  (make-instance 'd.rmacro:command
                 :regex (typecase regex
                          (d.regex:regex-scanner regex)
                          (d.regex:regex (d.regex:make-scanner regex))
                          (t (d.regex:make-scanner (d.regex:regex-from-string regex))))
                 :handler (apply #'make-command-handler lexicon handler other)
                 :docs docs))


(defmacro set-expressions (lexicon &rest expression-definitions)
  "This function is used to call SET-EXPRESSION in bulk."
  `(progn
     ,@(loop :for expression-definition :in expression-definitions
             :collect `(set-expression ,lexicon ,@expression-definition))))


(defmacro make-commands (lexicon &rest command-definitions)
  "Syntactic sugar to ease the use of make-command."
  `(list
     ,@(loop :for command-definition :in command-definitions
             :for regex-list := (first command-definition)
             :for handler := (second command-definition)
             :for docs := (third command-definition)
             :for options := (cdddr command-definition)
             :collect `(make-command ,lexicon
                                     (d.regex:concat-separated (list ,@(map 'list (lambda (term)
                                                                                    (if (and (listp term)
                                                                                             (typep (first term) 'keyword))
                                                                                      `(get-from-lexicon ,lexicon ,(first term) ,(rest term))
                                                                                      term))
                                                                            regex-list))
                                                               :separator-regex "\\s+"
                                                               :start-regex "^\\s*"
                                                               :end-regex "\\s*$"
                                                               :null-regex "^\\s*$")
                                     ,handler
                                     ,docs
                                     ,@options))))
