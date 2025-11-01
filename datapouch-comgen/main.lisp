;;;; main.lisp

;;; A little more advanced, but still dumb command generation


(in-package :datapouch.command-generation)


(defclass pattern ()
  ((regex :initarg :regex
          :reader regex
          :type (or d.regex:regex string)) ; :name -> :name ("NAME")
   (samples :initarg :samples
            :reader samples
            :type list-of-strings)))


(defgeneric make-pattern (thing)
  (:documentation "Create object of PATTERN class from something."))


;;; TBD: Scrap autocomplete. It's too complicated for now.
;;;      Autocomplete for lists of words, separated by spaces? EZ
;;;      Autocomplete for completely custom regexes? Insanity.
;;; Note: Maybe autocomplete for subset of commands? Only of a certain type?
;;;
;; This should be enough.
;; This is an analogue of pattern class, but for simple words
;; Why?
;; Because this way you can actually make autocomplete lists for them. Without
;; headache about autocompletion for custom set of strings that matches regex.
;; And why in such manner?
;; BODY defines both REGEX and SAMPLES from PATTERN class. Hence, WORD->PATTERN
;; transformation is easy.
;; But you can go further and make autocompletion from it. Really easy too.
;; Autocomplete does not break until you try to combine WORD object with
;; something else other that simple space-injected-concatenation / combination.
;(defclass word ()
;  ((body :initarg :body
;         :reader body
;         :type string)))


;; TODO: Add escaping to the string?
(defmethod make-pattern ((thing string))
  "Make a PATTERN from simple string. It assumes that string is *not* a regex,
and must be matched as-is."
  (make-instance 'pattern :regex thing :samples (list thing)))


;; Comfy pairing to use in expressions.lisp
(defclass processing-unit ()
  ((pattern :initarg :pattern
            :reader pattern
            :type (or pattern list))
   (handler :initarg :handler
            :reader handler
            :type function)))

