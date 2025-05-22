;;;; main.lisp

;;; A little more advanced, but still dumb command generation


(defclass isle ()
  ((regex :initarg :regex
          :type (or d.regex:regex string)) ; :name -> :name ("NAME")
   (autocomplete :initarg :autocomplete ; first one is the canonnical form (short form expands into this)
                 :type (or boolean list-of-strings)
                 :initform nil)         ; Long expressions have prefix autocomplete: they could be autocompleted when there is a prefix with non-NIL autocomplete
   (samples :initarg :samples
            :type list-of-strings) ; + autocomplete
   (short-regex :initarg :short
                :type (or boolean d.regex:regex string)
                :initform nil) ; :name -> "short-NAME"
   (short-samples :initarg :short-samples
                  :type (or t list-of-strings))
   (docform :initarg :docform
            :type (or boolean string)
            :initform nil)))


;; 0) Canonnical form
;;    - For autocomplete
;;    - For sampling
;; 1) Regex
;;    - For matching
;;    - For capturing arguments
;; 2) Short form


(defmethod initialize-instance :after ((isle isle) &key &allow-other-keys)
  (with-slots (regex short-regex autocomplete samples docform) isle
    (when (eq autocomplete t)
      (setf autocomplete (list regex)))
    (setf samples (append autocomplete samples))
    (when (eq short-regex t)
      (setf short-regex regex))
    (when (eq short-samples t)
      (setf short-samples (list short-regex)))
    (when (eq docform t)
      (setf docform regex))))


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
