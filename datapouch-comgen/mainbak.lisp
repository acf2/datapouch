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
  (pattern :initarg :pattern
           :reader pattern
           :type (or pattern list))
  (handler :initarg :handler
           :reader handler
           :type function))




  (autocomplete :initarg :autocomplete
                :initform nil
                :reader autocomplete
                :type string)
  (docform :initarg :docform
           :reader docform
           :type string))


(defgeneric make-annotated-pattern (thing)
  (:documentation "Create object of ANNOTATED-PATTERN class from something."))


(defmethod make-annotated-pattern ((thing string))
  "Make a ANNOTATED-PATTERN from simple string. It assumes that string is *not* a regex,
and must be matched as-is."
  (make-instance 'annotated-pattern
                 :regex thing
                 :samples (list thing)
                 :autocomplete thing
                 :docform thing))


;; TODO: Fix docform here to something like "<regex>", or similar
(defmethod make-annotated-pattern ((thing pattern))
  "Naively extends PATTERN object to ANNOTATED-PATTERN."
  (make-instance 'annotated-pattern
                 :regex (regex pattern)
                 :samples (samples pattern)
                 :docform (regex pattern)))


;; pattern + function
;; processing unit?


;; main PU + short PU
;; main PU: pattern + handler
;; short PU: shorthand + expander
;; handler is registered for shorthand too.

(defclass pattern ()
  ((regex :initarg :regex
          :reader regex
          :type (or d.regex:regex string)) ; :name -> :name ("NAME")
   (samples :initarg :samples
            :reader samples
            :type list-of-strings) ; + autocomplete (if present) + short samples (if present)
   (autocompletion :initarg :autocompletion ; first one is the canonnical form (short form expands into this)
                   :reader autocompletion
                   :type (or boolean list-of-strings)
                   :initform nil)         ; Long expressions have prefix autocomplete: they could be autocompleted when there is a prefix with non-NIL autocomplete
   (short-regex :initarg :short-regex
                :reader short-regex
                :type (or boolean d.regex:regex string)
                :initform nil) ; :name -> "short-NAME"
  ;(short-samples :initarg :short-samples
  ;               :reader short-samples
  ;               :type (or boolean list-of-strings)
  ;               :initform nil)
   (docform :initarg :docform
            :reader docform
            :type (or boolean string)
            :initform nil))
  (:documentation "PATTERN objects represent a part of a command, or a full one altogether.
PATTERN contains various information to be processed and used in forming
regexes for commands, autocompletion lists and more. To be more frank, PATTERN
class facilities are an augmentation of datapouch command system, that does not
substitute it.

REGEX is a regex for a command or a part of it. It could be a string, but
would be transformed into d.regex:regex at the earliest opportunity.

SAMPLES is a list of samples. This is what this pattern would look like in a
command. Some patterns, like fixed words, could be sampled completely. If your
command looks like '[Hh]ome', then (\"Home\" \"home\") is a complete sample
list. But some patterns could not have complete samples. Like pattern '\\d+'.
There are some clever techniques to test it, but to ensure reproducability
SAMPLES for this pattern would be a finite list, and therefore incomplete, like
(0 1 42 31337).
SAMPLES are used later to check command regexes for any collisions. If the
pattern has a short regex, then short samples should be included in this list
by user. On the contrary, if the pattern has autocompletition forms, then they
will be included in SAMPLES automatically.

AUTOCOMPLETION are strings that represent fully formed commands, or command
parts. The first autocompletion is always the canonnical form. For example, for
\"[Hh]ome\" regex there could be an autocompletion list with \"home\" and
\"Home\" strings, where \"home\" is canon. And then the SAMPLES list would be
left empty, because all autocompletions are added there automatically. Not all
patterns could have autocompletion. \"\\d+\" could not, for example. But it
still needs samples. Canonnical form is needed for expansion of short regex: it
will be rewritten to canonnical autocompletion.
T here means 'copy REGEX into AUTOCOMPLETION'.

SHORT-REGEX is another form of a command, that should work the same way. When
PATTERN objects are transformed into other things, SHORT-REGEX will be handled
differently than REGEX. Also, SHORT-REGEX is a thing that should be rewritten
to a full form, when autocompletion is invoked.
T here means 'copy REGEX into SHORT-REGEX'.

DOCFORM is a form of a PATTERN, that is used when composing documentation.
T here means 'copy REGEX into DOCFORM'.
"))


(defclass old-pattern ()
  ((regex :initarg :regex
          :reader regex
          :type (or d.regex:regex string)) ; :name -> :name ("NAME")
   (autocompletion :initarg :autocompletion ; first one is the canonnical form (short form expands into this)
                   :reader autocompletion
                   :type (or boolean list-of-strings)
                   :initform nil)         ; Long expressions have prefix autocomplete: they could be autocompleted when there is a prefix with non-NIL autocomplete
   (samples :initarg :samples
            :reader samples
            :type list-of-strings) ; + autocomplete
   (short-regex :initarg :short-regex
                :reader short-regex
                :type (or boolean d.regex:regex string)
                :initform nil) ; :name -> "short-NAME"
   (short-samples :initarg :short-samples
                  :reader short-samples
                  :type (or boolean list-of-strings)
                  :initform nil)
   (docform :initarg :docform
            :reader docform
            :type (or boolean string)
            :initform nil)))


(defparameter +sequence-marker+ :sequence)
(defparameter +alternation-marker+ :alternation)


;; 0) Canonnical form
;;    - For autocomplete
;;    - For sampling
;; 1) Regex
;;    - For matching
;;    - For capturing arguments
;; 2) Short form


(defmethod initialize-instance :after ((pattern pattern) &key &allow-other-keys)
  (with-slots (regex short-regex autocompletion samples short-samples docform) pattern
    (cond ((and autocompletion (listp autocompletion))
           (setf autocompletion (cons +alternation-marker+ autocompletion)))
          ((eq autocompletion t)
           (setf autocompletion (list regex))))
    (setf samples (cons +alternation-marker+ (append (rest autocompletion) samples)))
    (when (eq short-regex t)
      (setf short-regex regex))
    (cond ((and short-samples (listp short-samples))
           (setf short-samples (cons +alternation-marker+ short-samples)))
          ((eq short-samples t)
           (setf short-samples samples)))
    (assert (or (not short-regex) short-samples))
    (when (eq docform t)
      (setf docform regex))))


(declaim (ftype (function (function pattern pattern)) property-wrapper-generator))
(defun property-wrapper-generator (op one another)
  (lambda (property-name)
    (let ((reader-fun (find-symbol (string property-name) :d.comgen)))
      (when (fboundp reader-fun)
        (list (intern (string property-name) 'keyword)
              (funcall op (funcall reader-fun one) (funcall reader-fun another)))))))
              


(defmethod d.iface:concat-two ((one pattern) (another pattern))
  (flet ((concat-pattern-properties (p1 p2) (cons +sequence-marker+
                                                  (loop :for prop :in (list p1 p2)
                                                        :append (if (and (listp prop)
                                                                         (eq (first prop) +sequence-marker+))
                                                                  (rest prop)
                                                                  (list prop))))))
    (let ((wrapper (property-wrapper-generator #'concat-pattern-properties one another)))
      ;(apply #'make-instance 'pattern
      (list* #'make-instance 'pattern
             (loop :for property :in '(:regex :autocompletion :samples :short-regex :short-samples :docform)
                   :append (funcall wrapper property))))))


(defmethod d.iface:combine-two ((one pattern) (another pattern))
  nil)


(defun compose-doc (doctree &optional stack)
  (let ((stack-string (format nil "~{~#[~;~A~;~A:~]~}" (reverse stack))))
    (format nil "~{~A~}" (map 'list (lambda (term)
                                      (typecase term
                                        (list (compose-doc (first term) (cons (second term) stack)))
                                        (string (format nil term stack-string))
                                        (t term)))
                              doctree))))


    ;;; 1) simultaneous short expression definition (Long + short forms, defined in one place)
    ;;; 2) Problem with optional and separation regexes
    ;;;   :optional -> \\s+term?
    ;;;   :immediate -> term
    ;;;   :optinally-immediate -> (\\s+)?term
    ;;; 3) documentation concatenation (simultaneous docs concat)
    ;;; 4*) automatic creation of autocompletion list (simultaneous autocomplete list filling)
    ;;;     S: 

    ;;; Short form -> expands to full form
    ;;; Full form autocompletes
    ;;; :dae -> ("f" "forward" :separated) ("b" "backward" :separated) ("\\d+" nil :separated) ("\\*" nil)
    ;;; :goto -> ("g" . "goto") 
;      `(:new-link-next-sign "next"
;                            ,(return-match :next)
;                            "Sign for making new note the next note of current one"
;                            :use-nongroup-arguments t
;                            :short-regex "n"
;                            :short-options '("n" "next" :separated))
;
;      `(define-zac-expr :name :new-link-next-sign
;                        :full-regex "next\\s+(\\d+)"
;                        :short-regex "n\\d+"
;                        :short-options '(("n" "next" :separated) ("\\d+" nil :separated)) ;???
;                        :handler ,(return-match :next)
;                        :docs "Sign for making new note the next note of current one"
;                        :use-nongroup-arguments t)
;      ;;; + "short-NEW-LINK-NEXT-SIGN" -> `("n" ,(return-match :next) ...)
;
;      (list :name :number-plus
;            :regex '("\\d+" "\\*")
;            :short-form t ; eq to '("\\d+" nil :separated t)
;            :doc-form "<number>"
;            :handler ...
;            :separated t
;            :use-nongroup-arguments t)
;      (list :name :next-designator
;            :regex '("next" :number)
;            :short-form '(("n" "next" :separated t) :number)
;            ;;; func?
;            ;;; n -> "next" ; first separated is ignored
;            ;;; <number> -> " <number>"
;            :doc-form t ;eq to - :doc-form '("next" :number)
;            :separated t)
;
;      (command
;        :regex '("goto" :next-designator)
;        :short-form '(("g" "goto") :next-designator)
;        :doc-form t
;        
;
;
;      ;;; 1) Full regex + short regex, both working
;      ;;; 2.1) full form is autocompletable
;      ;;; 2.2) short regex is able to autocomplete into full form
;      ;;; 2.2.1) short regex autocomplete adds spaces where needed
;
;
;      ;basic block? regex shard
;      (:rx "asdf"
;       :short "a"
;       :samples '("asdf" "a")
;       :doc "asdf")
;      (:rx "\\d+"
;       :short t
;       :samples '("13" "1234567890")
;       :doc "<number>")
;
;      ;example:
;
;      (list :name :next-designator
;            :expr (list (list :rx "[Nn]ext" ; -> :next-designator
;                              :short "n"    ; -> "short-NEXT-DESIGNATOR"
;                              :autocomplete (list "Next" "next")
;                              :samples '("n") ; + autocomplete
;                              :docform "next")
;                        :number-plus)
;            ;;; func?
;            ;;; n -> "next" ; first separated is ignored
;            ;;; <number> -> " <number>"
;            :doc "Designating next note" ;eq to - :doc-form '("next" :number)
;            :handler ...
;            :separated t)
;
;      (list :name :number
;            :expr (list (list :rx "\\d+" ; -> :number-plus
;                              :short t   ; -> "short-NUMBER-PLUS"
;                              :autocomplete nil
;                              :samples '("13" "1234567890")
;                              :docform "<number>"))
;            :doc "Number"
;            :handler ...
;            :separated t
;            :use-nongroup-arguments t)
;      (list :name :direction
;            :expr (list (list :rx "forward|backward"
;                              :short "f|b"
;                              :autocomplete '("forward" "backward")
;                              :samples '("f" "b") ; + autocomplete
;                              :docform "{forward|backward}"))
;            :doc "Direction marker"
;            :hander ...
;            :separated t
;            :use-nongroup-arguments t)
;      (list :name :closure
;            :expr (list (list :rx "\\*"
;                              :short t
;                              :autocomplete nil
;                              :samples '("*")
;                              :docform "[*]"))
;            :handler ...
;            :separated nil ;; ATTENTION!
;            :use-nongroup-arguments t)
;
;      ;;; TODO: HOW TO COMBINE THEM?
;      ;;;       What a mess...
;      (list :name :dae
;            :expr (list :direction
;                        :number
;                        :closure)
;            :doc "Direction Advanced Exponent"
;            :hander ...
;            :separated t)
;
;      (command :expr (list (list :rx "goto"
;                                 :short "g"
;                                 :autocomplete (list "goto") ;
;                                 :additional-samples (list "g") ; +autocomplete
;        :short-form '(("g" "goto") :next-designator)
;        :doc-form t
;
;
;      ;;; Idea I: Use another lexicon to return full versions of command
;
;    ;; aux.lisp
;    (defparameter *substring-nonterm* (define-nonterminal :regex ".*?"
;                                                          :docs "<text>"
;                                                          :sample (list "a" "a a" "Asdf aSdf")))
;
;    ;; main.lisp
;    (zac-expression :substring *substring-nonterm*)
;
;    (zac-command (list (define-nonterminal :regex "[Hh]ello"
;                                           :sample (list "hello" "Hello"))
;                       *substring-nonterm*))
