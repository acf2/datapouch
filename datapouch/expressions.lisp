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
  ((name :initarg :name
         :type (or string keyword)
         :reader name)
   (regex-group :initarg :regex
                :type d.regex:regex
                :reader regex-group)
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

NAME is both the id of expression, which later could be used in
hash-table to find it, and the name of named regex group, which will be
wrapped around regex passed by user.

REGEX-GROUP is the regex group of the expression. CREATE-EXPRESSION
function automatically wraps passed regex in an appropriately named group,
but INITILIZE-INSTANCE does not.

USER-HANDLER is user handler. It should be able to handle a regex match of
REGEX-GROUP. By default it must be a function, that accepts only keyword
arguments, but changing CONFIG may change that. Return value could be anything,
but checking the documentation of PROCESSED-GROUP? is highly recommended.

CONFIG is the settings for GROUP-TREE-TRAVERSAL function.

DOCUMENTATION is self-explanatory."))

(defun create-expression (name regex user-handler docs &key (use-nongroup-arguments nil) (allow-traversal t))
  (declare (type (or string keyword) name)
           (type (or string d.regex:regex) regex)
           (type function user-handler)
           (type boolean use-nongroup-arguments allow-traversal))
  "CREATE-EXRESSION eliminates some boilerplate for user, when creating new
expression, and checks argument types. For the meaning of arguments refer to
EXPRESSION and EXPRESSION-CONFIG documentation."
  (make-instance 'expression
                 :name (string name)
                 :regex (d.regex:make-named-group (string name)
                                                        regex)
                 :user-handler user-handler
                 :docs docs
                 :config (make-instance 'expression-config
                                        :use-nongroup-arguments use-nongroup-arguments
                                        :allow-traversal allow-traversal)))


(defgeneric set-expression (lexicon name regex user-handler docs &key use-nongroup-arguments allow-traversal)
  (:documentation
    "Set expression in lexicon to new value. Expression is superseded, if
(EQUAL old-name new-name)."))


(defgeneric get-expression (lexicon name)
  (:documentation "Get expression from lexicon, that is identified by NAME."))


(defgeneric group-tree-traversal (lexicon group-tree)
  (:documentation "Generic for group-tree traversal. Refer to methods for documentation."))


(defclass lexicon ()
  ((expression-lookup :initform (make-hash-table :test #'equal)))
  (:documentation
    "LEXICON objects serve as dictionaries, lookup tables for expressions.
Could be interpreted as a context for GROUP-TREE-TRAVERSAL function."))


(defmethod set-expression ((lexicon lexicon) name regex user-handler docs &key (use-nongroup-arguments nil) (allow-traversal t))
  (declare (type (or string keyword) name)
           (type (or string d.regex:regex) regex)
           (type function user-handler)
           (type boolean use-nongroup-arguments allow-traversal))
  "Method for LEXICON objects. Checks argument types."
  (with-slots (expression-lookup) lexicon
    (setf (gethash (string name) expression-lookup)
          (create-expression name
                             regex
                             user-handler
                             docs
                             :use-nongroup-arguments use-nongroup-arguments
                             :allow-traversal allow-traversal))))


(defmethod get-expression ((lexicon lexicon) name)
  (declare (type (or string keyword) name))
  "Method for LEXICON objects. Checks argument types."
  (with-slots (expression-lookup) lexicon
    (gethash (string name) expression-lookup)))


(defun processed-group? (lst)
  "PROCESSED-GROUP? is a predicate, that checks if a term in group-tree has a
certain structure. This structure is a list with precisely two elements,
first of which have to be a keyword. These elements will be used later in a
user-handler call, as a keyword arguments, when USE-NONGROUP-ARGUMENTS is set
to NIL.
That structure is implied to be another regex group match, processed by another
expression, hence the name. And yes, this predicate defines, how return values
of user-handlers should be structured for it to work as 'group arguments'."
  (and (listp lst)
       (typep (first lst) 'keyword)
       (= (length lst) 2)))


(defun make-result (name value)
  (declare (type (or string keyword) name))
  "This function formattes the result of an expression to be recognised by
another expression with USE-NONGROUP-ARGUMENTS set to NIL. Essentially it
mirrors the PROCESSED-GROUP? predicate: if changed, they must be changed
simultaneously."
  (list name value))


(defun return-match (name)
  (declare (type (or string keyword) name))
  "RETURN-MATCH makes a function, that simply returns the match, that is the
single term. Very useful."
  (lambda (arg)
    (make-result name arg)))


(defun funcall-group-list-with-filtering (lexicon user-handler group-tree &key use-nongroup-arguments allow-traversal)
  "A reusable piece of code, that incapsulates expression config checks and
corresponding transformations."
  (let* ((all-arguments (if allow-traversal
                          (map 'list (lambda (term)
                                       (group-tree-traversal lexicon term))
                               group-tree)
                          group-tree))
         (filtered-arguments (if use-nongroup-arguments 
                               all-arguments
                               (reduce #'append (remove-if-not #'processed-group? all-arguments)))))
    (apply user-handler filtered-arguments)))


(defmethod group-tree-traversal ((lexicon lexicon) group-tree)
  "This is the GROUP-TREE-TRAVERSAL for LEXICON class.
Processing goes like this:
1) If it's just a term, return it.
2) If it's a list, without a certain structure - recurse for each of the
elements.
3) If it's a list with regex group match structure, i.e. (:group \"name\" ...),
and the corresponding group was found in lexicon, then more:
3.1) If traversal is allowed, then recurse for each of the elements first.
3.2) Then, if only group arguments is allowed - filter all other arguments.
3.3) And only then make a call to user-handler with these arguments."
  (if (not (listp group-tree))
    group-tree
    (let ((expression (when (eq (first group-tree) :group)
                        (get-expression lexicon (second group-tree)))))
      (if expression
        (funcall-group-list-with-filtering lexicon
                                           (user-handler expression)
                                           (cddr group-tree)
                                           :allow-traversal (allow-traversal (config expression))
                                           :use-nongroup-arguments (use-nongroup-arguments (config expression)))
        (map 'list (lambda (term)
                     (group-tree-traversal lexicon term))
             group-tree)))))


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
                                       user-handler
                                       group-tree
                                       :allow-traversal allow-traversal
                                       :use-nongroup-arguments use-nongroup-arguments)))


(defun make-command (lexicon regex handler docs &rest other &key &allow-other-keys)
  "This function wraps D.RMACRO:COMMAND creation with the use of MAKE-COMMAND-HANDLER in
one call."
  (make-instance 'd.rmacro:command
                 :regex (typecase regex
                          (d.regex:regex regex)
                          (d.regex:regex-scanner regex)
                          (t (make-instance 'd.regex:regex :expr regex)))
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
                                                                                    (if (typep term 'keyword)
                                                                                      `(regex-group (get-expression ,lexicon ,term))
                                                                                      term))
                                                                            regex-list))
                                                               :separator-regex "\\s+"
                                                               :start-regex "^\\s*"
                                                               :end-regex "\\s*$"
                                                               :null-regex "^\\s*$")
                                     ,handler
                                     ,docs
                                     ,@options))))
