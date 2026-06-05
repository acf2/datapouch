;;;; command/pattern.lisp


;;; A little more advanced, but still dumb command generation


(in-package :datapouch.command.pattern)


(defparameter +no-canon-form-marker+ :not-applicable)
(defparameter +no-canon-form+ (list (list +no-canon-form-marker+)))


(defun canon-form-p (list)
  (and (consp list)
       (every (lambda (alternatives)
                (and (listp alternatives)
                     (every (lambda (form)
                              (or (stringp form)
                                  (null form)
                                  (and (typep form (type-of +no-canon-form-marker+))
                                       (eq form +no-canon-form-marker+))))
                            alternatives)))
              list)))


(deftype canon-form ()
  `(satisfies canon-form-p))


(defun canon-form-to-autocomplete (canon-form)
  (remove nil
          (loop :for canon-subform :in canon-form
                :collect (remove nil (loop :for word :in canon-subform
                                           :until (and (typep word 'keyword)
                                                       (eq word +no-canon-form-marker+))
                                           :collect word)))))


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
            :reader docform)
   (short-docform :initarg :short-docform
                  :initform nil
                  :type (or null docform)
                  :reader short-docform)))


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


(defparameter +default-short-expander+ (lambda (info &rest tree)
                                         (declare (ignore info))
                                         (apply #'concatenate 'string tree)))


(defparameter +default-top-short-expander+ (lambda (&rest tree)
                                             (apply #'concatenate 'string tree)))


(declaim (ftype (function (sampled-regex relaxed-sampled-regex canon-form string &optional (or null string)))
                make-pattern))
(defun make-pattern (regex short-regex canon-form doc &optional short-doc)
  (make-instance
    'pattern
    :regex regex
    :short-regex short-regex
    :expander-short-regex short-regex
    :canon-form canon-form
    :docform (make-docform doc)
    :short-docform (when short-doc
                     (make-docform short-doc))))


; TODO: Root pattern container, that is given to all plugins
;       To check all regexes against all samples.

(defclass behavior-container ()
  ((pattern-lookup :initform (make-hash-table :test #'equal))
   (utility-lexicon :initform (make-instance 'd.expr:lexicon))
   (expander-lexicon :initform (make-instance 'd.expr:lexicon))))


(declaim (ftype (function ((or string keyword)))
                short-expression-type))
(defun short-expression-type (behavior-type)
  (concatenate 'string "Shorthand for " (string behavior-type)))


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
                   :expander-expression (when (and expander-short-regex short-expander)
                                          (d.expr:make-expression
                                            behavior-type ; Look! Expander lexicon uses full names!
                                            expander-short-regex
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
  "A shortcut function to add or reset pattern to behavior container with
chosen handler(s), documentation and processing parameters. Refer to
EXPRESSION-CONFIG docs for parameter meaning."
  (put-into container
            (make-behavior behavior-type
                           pattern
                           handler
                           short-expander
                           :use-only-named-results use-only-named-results
                           :allow-traversal allow-traversal)))


(declaim (ftype (function (behavior-container (or keyword string) &optional (or keyword string) t))
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
                                  (make-named-group name (docform pattern))
                                  (docform pattern))
                       :short-docform (let ((sdf (short-docform pattern)))
                                        (when sdf (if name
                                                    (make-named-group name sdf)
                                                    sdf))))))))


(declaim (ftype (function (string &optional (or null string)))
                trivial-pattern-type))
(defun trivial-pattern-type (word &optional shorthand)
  (format nil "Trivial pattern: ~:[~;~:*~A->~]~A" shorthand word))


(declaim (ftype (function (behavior-container string &optional (or null string)))
                add-trivial-pattern))
(defun add-trivial-pattern (container word &optional shorthand)
  (let ((behavior-type (trivial-pattern-type word shorthand)))
    (put-into container
              (make-behavior behavior-type
                             (make-pattern (sampled-regex-from-string word (list word))
                                           (when shorthand
                                             (sampled-regex-from-string shorthand (list shorthand)))
                                           (list (list word))
                                           word
                                           shorthand)
                             nil
                             (lambda (&rest rest)
                               (declare (ignore rest))
                               word)))
    behavior-type))


(declaim (ftype (function (string list-of-strings string &optional (or null string)))
                make-wildcard-pattern))
(defun make-wildcard-pattern (string-rx samples doc &optional short-doc)
  (let ((rx (sampled-regex-from-string string-rx samples)))
    (make-instance
      'pattern
      :regex rx
      :short-regex rx
      :expander-short-regex rx
      :canon-form +no-canon-form+
      :docform (make-docform doc)
      :short-docform (when short-doc
                       (make-docform short-doc)))))


(defmethod make-optional ((pattern pattern))
  (with-slots (regex short-regex expander-short-regex canon-form docform short-docform) pattern
    (make-instance 'pattern
                   :regex (make-optional regex)
                   :short-regex (when short-regex
                                  (make-optional short-regex))
                   :expander-short-regex (when expander-short-regex
                                           (make-optional expander-short-regex))
                   :canon-form (cons nil canon-form)
                   :docform (make-optional docform)
                   :short-docform (when short-docform
                                    (make-optional short-docform)))))


(defmethod concat-two ((one pattern) (another pattern))
  (with-slots ((short-one short-regex)
               (expand-one expander-short-regex)
               (sdf-one short-docform)) one
    (with-slots ((short-another short-regex)
                 (expand-another expander-short-regex)
                 (sdf-another short-docform)) another
      (make-instance 'pattern
                     :regex (concat-two (regex one) (regex another))
                     :short-regex (when (and short-one short-another)
                                    (concat-two short-one short-another))
                     :expander-short-regex (when (and expand-one expand-another)
                                             (concat-two expand-one expand-another))
                     ;; delete-duplicates could be used here
                     ;; cartesian-product does not reuse conses
                     :canon-form (delete-duplicates (d.aux:cartesian-product (list (canon-form one)
                                                                                   (canon-form another))
                                                                             #'append)
                                                    :test #'equal)
                     :docform (concat-two (docform one)
                                          (docform another))
                     :short-docform (when (and sdf-one sdf-another)
                                      (concat-two sdf-one
                                                  sdf-another))))))


(defmethod combine-two ((one pattern) (another pattern))
  (with-slots ((short-one short-regex)
               (expand-one expander-short-regex)
               (sdf-one short-docform)) one
    (with-slots ((short-another short-regex)
                 (expand-another expander-short-regex)
                 (sdf-another short-docform)) another
      (make-instance 'pattern
                     :regex (combine-two (regex one) (regex another))
                     :short-regex (when (and short-one short-another)
                                    (combine-two short-one short-another))
                     :expander-short-regex (when (and expand-one expand-another)
                                             (combine-two expand-one expand-another))
                     :canon-form (remove-duplicates (append (canon-form one)
                                                            (canon-form another))
                                                    :test #'equal)
                     :docform (combine-two (docform one)
                                           (docform another))
                     :short-docform (when (and sdf-one sdf-another)
                                      (combine-two sdf-one
                                                   sdf-another))))))
