;;;; command/pattern.lisp


;;; A little more advanced, but still dumb command generation


(in-package :datapouch.command.pattern)


(defparameter +default-short-expander+ (lambda (info tree)
                                         (declare (ignore info))
                                         (apply #'concatenate 'string tree)))


(defclass docform-view ()
  ((space-pattern :initarg :space
                   :reader space-pattern
                   :type pattern)
   (sequence-fun :initarg :sequence
                 :reader sequence-fun
                 :type function)
   (alternation-fun :initarg :alternation
                    :reader alternation-fun
                    :type function)
   (group-fun :initarg :group
              :reader group-fun
              :type function)
   (named-regex-group-fun :initarg :named-regex
                          :reader named-regex-group-fun
                          :type function)
   (optional-fun :initarg :optional
                 :reader optional-fun
                 :type function)))


(defclass docform ()
  ((body :initarg :doc
         :type string
         :reader doc)
   (internal-combination-type :initarg :internal-combination-type
                              :reader internal-combination-type
                              :initform nil)))


(defun canon-form-p (list)
  (and (consp list)
       (every (lambda (alternatives)
                (and (listp alternatives)
                     (every (lambda (form)
                              (or (stringp form)
                                  (null form)
                                  (and (keywordp form)
                                       (eq form :not-applicable))))
                            alternatives)))
              list)))


(deftype canon-form ()
  `(satisfies canon-form-p))


(defclass pattern ()
  ((regex :initarg :regex
          :type sampled-regex
          :reader regex)
   (short-regex :initarg :short-regex
                :initform nil
                :type relaxed-sampled-regex
                :reader short-regex)
   (expander-short-regex :initarg :expander-short-regex
                         :initform nil
                         :type relaxed-sampled-regex
                         :reader expander-short-regex)
   (canon-form :initarg :canon-form
               :initform nil
               :type canon-form
               :reader canon-form)
   (docform :initarg :docform
            :type docform
            :reader docform)))


(defclass behavior ()
  ((behavior-type :initarg :behavior-type
                  :type string
                  :reader behavior-type)
   (pattern :initarg :pattern
            :type pattern
            :reader pattern)
   (utility-expression :initarg :utility-expression
                       :initform nil
                       :type (or null d.expr:expression)
                       :reader utility-expression)
   (short-utility-expression :initarg :short-expression
                             :initform nil
                             :type (or null d.expr:expression)
                             :reader short-expression)
   (expander-expression :initarg :expander-expression
                        :initform nil
                        :type (or null d.expr:expression)
                        :reader expander-expression)))


(declaim (ftype (function (string &optional (or null keyword)))
                make-docform))
(defun make-docform (body &optional internal-state)
  (make-instance 'docform
                 :doc body
                 :internal-combination-type internal-state))


(declaim (ftype (function (sampled-regex relaxed-sampled-regex canon-form string))
                make-pattern))
(defun make-pattern (regex short-regex canon-form doc)
  (make-instance
    'pattern
    :regex regex
    :short-regex short-regex
    :expander-short-regex short-regex
    :canon-form canon-form
    :docform (make-instance
               'docform
               :doc doc)))


(defparameter +default-docform-view+
  (make-instance
    'docform-view
    :space (make-pattern (sampled-regex-from-string "\\s+"
                                                    (list " " "     " (coerce (list #\Tab) 'string)))
                         (sampled-regex-from-string "\\s*"
                                                    (list "" " " "     " (coerce (list #\Tab) 'string)))
                         (list nil)
                         " ")
    :sequence (let ((grouping-types (list :alternation)))
                (lambda (&rest docforms)
                  (make-docform
                    (format nil
                            "~{~A~}"
                            (map 'list (lambda (docform)
                                         (declare (special *docform-view*))
                                         (if (member (internal-combination-type docform)
                                                     grouping-types)
                                           (funcall (group-fun *docform-view*)
                                                    (doc docform))
                                           (doc docform)))
                                 docforms))
                    :sequence)))
    :alternation (let ((grouping-types (list :sequence)))
                   (lambda (&rest docforms)
                     (make-docform
                       (format nil
                               "~{~#[~;~A~:;~A | ~]~}"
                               (map 'list (lambda (docform)
                                            (declare (special *docform-view*))
                                            (if (member (internal-combination-type docform)
                                                        grouping-types)
                                              (funcall (group-fun *docform-view*)
                                                       (doc docform))
                                              (doc docform)))
                                    docforms))
                       :alternation)))
    :group (lambda (docform)
             (make-docform (format nil "(~A)" (doc docform))))
    :named-regex (lambda (name docform)
                   (make-docform (format nil "<~A:~A>" name (doc docform))))
    :optional (lambda (docform)
                (make-docform (format nil "[~A]" (doc docform))))))


(defparameter *docform-view* +default-docform-view+)


;(defclass pattern ()
;  ((regex :initarg :regex
;          :type d.regex:sampled-regex
;          :reader regex)
;   (short-regex :initarg :short-regex
;                :initform nil
;                :type (or null d.regex:sampled-regex)
;                :reader short-regex)
;   (canon-form :initarg :canon-form
;               :initform nil
;               :type list-of-list-of-strings
;               :reader canon-form)
;   (docform :initarg :docform
;            :type string
;            :reader docform)
;   (internal-combination-type :initarg :internal-combination-type
;                              :initform nil)))

;(defclass pattern ()
;  ((pattern-type :initarg :pattern-type
;                  :type string
;                  :reader pattern-type)
;   (pattern :initarg :pattern
;            :type pattern
;            :reader pattern)
;   (handler :initarg :handler
;            :initform nil
;            :type (or null function)
;            :reader handler)
;   (short-expander :initarg :short-expander
;                   :initform +default-short-expander+
;                   :type function
;                   :reader short-expander)))


; TODO: Root pattern container, that is given to all plugins
;       To check all regexes against all samples.

(defclass behavior-container ()
  ((pattern-lookup :initform (make-hash-table :test #'equal))
   (utility-lexicon :initform (make-instance 'd.expr:lexicon))
   (expander-lexicon :initform (make-instance 'd.expr:lexicon))))


(declaim (ftype (function ((or string keyword)))
                short-expression-type))
(defun short-expression-type (behavior-type)
  (concatenate 'string "short-" (string behavior-type)))


(declaim (ftype (function ((or keyword string)
                           pattern
                           (or null function)
                           (or null function)
                           &key
                           (:use-only-named-results boolean)
                           (:allow-traversal boolean)))
                make-behavior))
(defun make-behavior (behavior-type pattern handler short-expander &key (use-only-named-results t) (allow-traversal t))
  (with-slots (regex short-regex expander-short-regex) pattern
    (make-instance 'behavior
                   :behavior-type behavior-type
                   :pattern pattern
                   :utility-expression (when handler
                                         (d.expr:make-expression
                                           behavior-type
                                           regex
                                           handler
                                           nil
                                           :use-only-named-results use-only-named-results
                                           :allow-traversal allow-traversal))
                   :short-expression (when (and short-regex handler)
                                       (d.expr:make-expression
                                         (short-expression-type behavior-type)
                                         short-regex
                                         handler
                                         nil
                                         :use-only-named-results use-only-named-results
                                         :allow-traversal allow-traversal))
                   :expander-expression (when (and short-regex short-expander)
                                          (d.expr:make-expression
                                            behavior-type ; Look! Expander lexicon uses full names!
                                            short-regex
                                            short-expander
                                            nil
                                            :use-only-named-results nil)))))


(defmethod put-into ((container behavior-container)
                     (behavior behavior)
                     &key)
  "Set pattern in behavior-container to new value. It is superseded, if
(EQUAL old-type new-type)."
  (with-slots (pattern-lookup utility-lexicon expander-lexicon) container
    (with-slots ((btype behavior-type)
                 (pattern pattern)
                 (uexpr utility-expression)
                 (suexpr short-utility-expression)
                 (eexpr expander-expression)) behavior
          (setf (gethash (string btype) pattern-lookup) pattern)
          ;; TBD: Delete from lexicons if null?
          (when uexpr
            (put-into utility-lexicon uexpr))
          (when suexpr
            (put-into utility-lexicon suexpr))
          (when eexpr
            (put-into expander-lexicon eexpr))
          btype)))


(defmethod get-from ((container behavior-container) behavior-type)
  (declare (type (or string keyword) behavior-type))
  (with-slots (utility-lexicon expander-lexicon pattern-lookup) container
    (let ((pattern (gethash (string behavior-type)
                            pattern-lookup)))
      (when pattern
        (make-instance 'behavior
                       :behavior-type behavior-type
                       :pattern pattern
                       :utility-expression (get-from utility-lexicon behavior-type)
                       :short-expression (get-from utility-lexicon
                                                   (short-expression-type behavior-type))
                       :expander-expression (get-from expander-lexicon behavior-type))))))


(declaim (ftype (function (behavior-container (or keyword string) pattern function
                            &key (:use-only-named-results boolean) (:allow-traversal boolean) (:short-expander function)))
                set-behavior))
(defun set-behavior (container behavior-type pattern handler
                      &key (use-only-named-results t) (allow-traversal t) (short-expander +default-short-expander+))
  "A shortcut function to add or reset regex group to lexicon with chosen
handler, documentation and processing parameters. Refer to EXPRESSION-CONFIG
docs for parameter meaning."
  (put-into container
            (make-behavior behavior-type
                           pattern
                           handler
                           short-expander
                           :use-only-named-results use-only-named-results
                           :allow-traversal allow-traversal)))


(declaim (ftype (function (behavior-container (or keyword string) &optional string t))
                get-pattern))
(defun get-pattern (container behavior-type &optional name info)
  (with-slots (pattern-lookup utility-lexicon expander-lexicon) container
    (let ((pattern (gethash (string behavior-type)
                            pattern-lookup))
          (urx (get-from-lexicon utility-lexicon
                                 behavior-type
                                 info))
          (surx (get-from-lexicon utility-lexicon
                                  (short-expression-type behavior-type)
                                  info))
          (erx (get-from-lexicon expander-lexicon
                                 behavior-type)))
      (when pattern
        (make-instance 'pattern
                       :regex (or urx (regex pattern))
                       :short-regex (or surx (short-regex pattern))
                       :expander-short-regex (or erx (expander-short-regex pattern))
                       :canon-form (canon-form pattern)
                       :docform (if name
                                  (funcall (named-regex-group-fun *docform-view*)
                                           name
                                           (docform pattern))
                                  (docform pattern)))))))


(declaim (ftype (function (string &key (:type (or keyword string))))
                trivial-pattern-type))
(let ((trivial-pattern-types-generated 0))
  (defun make-trivial-pattern-type (word &key ((:type behavior-type)))
    (or behavior-type
        (let ((result (format nil "trivial-pattern-~A-~A" trivial-pattern-types-generated word)))
          (setf trivial-pattern-types-generated (1+ trivial-pattern-types-generated))
          result))))


(declaim (ftype (function (behavior-container string &key (:type (or keyword string)) (:short string)))
                add-trivial-pattern))
(defun add-trivial-pattern (container word &key ((:type explicit-type)) ((:short shorthand)))
  (let ((behavior-type (make-trivial-pattern-type word :type explicit-type)))
    (put-into container
              (make-behavior behavior-type
                             (make-pattern (sampled-regex-from-string word (list word))
                                           (when shorthand
                                             (sampled-regex-from-string shorthand (list shorthand)))
                                           (list (list word))
                                           word)
                             nil
                             (lambda (&rest rest)
                               (format t "GOT: ~A~&" rest)
                               word)))
    behavior-type))


;(defparameter pc (make-instance 'd.ptrn::behavior-container))
;(d.ptrn::add-trivial-pattern pc "fart" :short "f")
;
;(defparameter farting
;  (let ((lex (with-slots ((el d.ptrn::expander-lexicon)) pc el)))
;    (d.c.aux:make-rmacro-callback
;      (d.c.aux:make-regex-parser
;        (d.regex:make-scanner
;
;          (d.regex:concat-separated
;            (list (get-from-lexicon lex (d.ptrn::trivial-pattern-type "fart")))
;            :separator-regex "\\s+"
;            :start-regex "^\\s*"
;            :end-regex "\\s*$"
;            :null-regex "^\\s*$")))
;      (wrap-with-lexicon lex (lambda (other) (format t "OTHER: ~S~&" other)) :use-only-named-results nil))))
;
;(defparameter a (multiple-value-list (funcall farting "f")))
;
;(defmacro tst () (second a))


(defun make-expander-callback (parser handler)
  (lambda (command-string)
    (multiple-value-bind (success match) (funcall parser command-string)
      (if success
        (values t (funcall handler match))
        (values nil nil)))))


(defun make-expander-with-lexicon (lexicon regex-list handler)
  (make-expander-callback
    (d.c.aux:make-regex-parser
      (d.regex:make-scanner
        (d.regex:concat-separated
          (map 'list (lambda (term)
                       (get-from-lexicon lexicon term))
               regex-list)
          :separator-regex "\\s+"
          :start-regex "^\\s*"
          :end-regex "\\s*$"
          :null-regex "^\\s*$")))
    (d.expr:wrap-with-lexicon lexicon handler :use-only-named-results nil)))

;
;(declaim (ftype (function (pattern))
;                make-optional))
;(defun make-optional (pattern)
;  (with-slots (short-regex) pattern
;    (make-instance 'pattern
;                   :regex (d.regex:make-optional (regex pattern))
;                   :short-regex (when short-regex
;                                  (d.regex:make-optional short-regex))
;                   :canon-form (cons nil (canon-form pattern))
;                   :docform (funcall (optional-fun *docform-view*)
;                                     (docform pattern)))))
;
;
;(let ((grouping-types (list :alternation)))
;  (defmethod concat-two ((one pattern) (another pattern))
;    (with-slots ((short-one short-regex)
;                 (one-comb-type internal-combination-type)) one
;      (with-slots ((short-another short-regex)
;                   (another-comb-type internal-combination-type)) another
;        (make-instance 'pattern
;                       :regex (concat-two (regex one) (regex another))
;                       :short-regex (when (and short-one short-another)
;                                      (concat-two short-one short-another))
;                       :canon-form (d.aux:cartesian-product (list (canon-form one)
;                                                                  (canon-form another))
;                                                            #'append)
;                       :docform (funcall
;                                  (sequence-fun *docform-view*)
;                                  (if (member one-comb-type
;                                              grouping-types)
;                                    (funcall (group-fun *docform-view*) (docform one))
;                                    (docform one))
;                                  (if (member another-comb-type
;                                              grouping-types)
;                                    (funcall (group-fun *docform-view*) (docform another))
;                                    (docform another)))
;                       :internal-combination-type :sequence)))))
;
;
;(let ((grouping-types (list :sequence)))
;  (defmethod combine-two ((one pattern) (another pattern))
;    (with-slots ((short-one short-regex)
;                 (one-comb-type internal-combination-type)) one
;      (with-slots ((short-another short-regex)
;                   (another-comb-type internal-combination-type)) another
;        (make-instance 'pattern
;                       :regex (combine-two (regex one) (regex another))
;                       :short-regex (when (and short-one short-another)
;                                      (combine-two short-one short-another))
;                       :canon-form (append (canon-form one)
;                                           (canon-form another))
;                       :docform (funcall (alternation-fun *docform-view*)
;                                         (if (member one-comb-type
;                                                     grouping-types)
;                                           (funcall (group-fun *docform-view*) (docform one))
;                                           (docform one))
;                                         (if (member another-comb-type
;                                                     grouping-types)
;                                           (funcall (group-fun *docform-view*) (docform another))
;                                           (docform another)))
;                       :internal-combination-type :alternation)))))


;(defun compile-pattern-expression (pattern-expression)
;  (labels
;    ((traverse (tree) (cond ((typep tree 'pattern-expression)
;                             (values (regex tree)
;                                     (short-regex tree)
;                                     (canon-form tree)
;                                     (docform tree)))
;                            ((d.regex:ppcre-sequence? tree)
;                             (let ((results (rotate (map 'list
;                                                         (lambda (subtree)
;                                                           (multiple-value-list (traverse subtree)))
;                                                         (rest tree)))))
;                               (values (concat (first results))
;                                       (concat (second results))

