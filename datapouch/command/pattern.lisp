;;;; command/pattern.lisp


;;; A little more advanced, but still dumb command generation


(in-package :datapouch.command.pattern)


(defclass pattern ()
  ((pattern-type :initarg :pattern-type
                 :type string
                 :reader pattern-type)
   (regex :initarg :regex
          :type d.regex:sampled-regex
          :reader regex)
   (short-regex :initarg :short-regex
                :initform nil
                :type (or null d.regex:sampled-regex)
                :reader short-regex)
   (handler :initarg :handler
            :initform nil
            :type (or null function)
            :reader handler)
   (short-expander :initarg :short-expander
                   :initform nil
                   :type (or null function)
                   :reader short-expander)
   (canon-form :initarg :canon-form
               :type list-of-list-of-strings
               :reader canon-form)
   (docform :initarg :docform
            :type string
            :reader docform)))


; TODO: Root pattern container, that is given to all plugins
;       To check all regexes against all samples.

(defclass pattern-container ()
  ((pattern-lookup :initform (make-hash-table :test #'equal))
   (utility-lexicon :initform (make-instance 'd.expr:lexicon))
   (expander-lexicon :initform (make-instance 'd.expr:lexicon))))


(declaim (ftype (function ((or string keyword)))
                short-expression-name))
(defun short-expression-name (pattern-type)
  (concatenate 'string "short-" pattern-type))


(defmethod put-into ((container pattern-container)
                     (pattern pattern)
                     &key
                     (use-only-named-results t)
                     (allow-traversal t))
  "Set pattern in pattern-container to new value. It is superseded, if
(EQUAL old-type new-type)."
  (with-slots (pattern-lookup utility-lexicon expander-lexicon) container
    (with-slots (pattern-type regex short-regex handler short-expander) pattern
      (setf (gethash (string pattern-type) pattern-lookup) pattern)
      (cond (handler (d.expr:set-in-lexicon utility-lexicon
                                            pattern-type
                                            regex
                                            handler
                                            nil
                                            :use-only-named-results use-only-named-results
                                            :allow-traversal allow-traversal))
            ;; TBD: Delete from lexicon both normal and short?
            ((and (null handler)
                  (get-from utility-lexicon pattern-type))
             (error 'should-not-occur))
            ((and short-regex handler)
             (d.expr:set-in-lexicon utility-lexicon
                                    (short-expression-name pattern-type)
                                    short-regex
                                    handler
                                    nil
                                    :use-only-named-results use-only-named-results
                                    :allow-traversal allow-traversal))
            ((and short-regex short-expander)
             (d.expr:set-in-lexicon expander-lexicon
                                    pattern-type
                                    short-regex
                                    short-expander
                                    nil
                                    :use-only-named-results nil))
            ;; TBD: Delete from lexicon?
            ((and (null short-expander)
                  (get-from expander-lexicon pattern-type))
             (error 'should-not-occur))))))


(defmethod get-from ((container pattern-container) pattern-type)
  (declare (type (or string keyword) pattern-type))
  (with-slots (pattern-lookup) container
    (gethash (string pattern-type) container)))


(declaim (ftype (function (string &key (:type (or keyword string))))
                trivial-pattern-type))
(defun trivial-pattern-type (word &key ((:type pattern-type)))
  (or pattern-type
      (concatenate 'string "trivial-pattern-" word)))


(declaim (ftype (function (pattern-container string &key (:type (or keyword string)) (:short string)))
                add-trivial-pattern))
(defun add-trivial-pattern (container word &key ((:type pattern-type)) ((:short shorthand)))
  (let ((name (trivial-pattern-type word :type pattern-type)))
    (put-into container
              (make-instance 'pattern
                             :pattern-type name
                             :regex (d.regex:regex-from-string word)
                             :short-regex (when shorthand (d.regex:regex-from-string shorthand))
                             :short-expander (lambda (&rest rest)
                                               (format t "GOT: ~A~&" rest)
                                               word)
                             :canon-form (list (list word))
                             :docform word)
              :use-only-named-results nil)
    name))

;(defparameter pc (make-instance 'd.ptrn::pattern-container))
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
