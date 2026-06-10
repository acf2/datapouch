;;;; pattern-expressions.lisp


(in-package :datapouch.command.pattern)


(defparameter +default-space-samples+ (list (repeat-string 0 #\Space)
                                            (repeat-string 1 #\Space)
                                            (repeat-string 42 #\Space)
                                            (repeat-string 1 #\Tab)
                                            (repeat-string 30 #\Tab)))


(defparameter +default-space-pattern+
  (make-pattern (sampled-regex-from-string "\\s+"
                                           (rest +default-space-samples+))
                (sampled-regex-from-string "\\s*"
                                           +default-space-samples+)
                (list nil)
                (string #\Space)
                ""))


(defparameter +default-fixed-space-pattern+
  (make-pattern (sampled-regex-from-string "\\s+"
                                           (rest +default-space-samples+))
                (sampled-regex-from-string "\\s+"
                                           (rest +default-space-samples+))
                (list nil)
                (string #\Space)
                (string #\Space)))


(defparameter +default-begin-pattern+
  (make-pattern (sampled-regex-from-string "^\\s*"
                                           +default-space-samples+)
                (sampled-regex-from-string "^\\s*"
                                           +default-space-samples+)
                (list nil)
                ""
                ""))


(defparameter +default-end-pattern+
  (make-pattern (sampled-regex-from-string "\\s*$"
                                           +default-space-samples+)
                (sampled-regex-from-string "\\s*$"
                                           +default-space-samples+)
                (list nil)
                ""
                ""))


(declaim (ftype (function (behavior-container))
                add-space-patterns))
(defun add-space-patterns (container)
  (loop :for (btype pattern expander) :in (list (list :space +default-space-pattern+ (constantly (string #\Space)))
                                                (list :fixed-space +default-fixed-space-pattern+ (constantly (string #\Space)))
                                                (list :begin +default-begin-pattern+ (constantly ""))
                                                (list :end +default-end-pattern+ (constantly "")))
        :do (put-into container
                      (make-behavior btype
                                     pattern
                                     nil
                                     expander))))


(defun make-preset-behavior-container ()
  (let ((bc (make-instance 'behavior-container)))
    (add-space-patterns bc)
    bc))


;; :some-pattern
(defun pattern-expression-bare-keyword? (term)
  (and (atom term)
       (typep term 'keyword)))


;; (:behavior-type . <info>)
(defun pattern-expression-reference-term? (term)
  (and (listp term)
       (or (typep (first term) 'keyword))))


;; (:trivial "trivial_word" ["tw"])
(defun pattern-expression-trivial-reference-term? (term)
  (and (listp term)
       (atom (first term))
       (member (first term)
               (list :trivial))
       (stringp (second term))
       (or (null (third term))
           (stringp (third term)))))


(defun get-name-from-plist (plist)
  (and (listp plist)
       (evenp (length plist))
       (getf plist :name)))


;; Plus (:+) designates sequences
;; Questionmark (:?) designates optional
;; Asterisk (:*) designates alternatives
(defparameter *compile-pattern-optional* :?)
(defparameter *compile-pattern-sequence* :+)
(defparameter *compile-pattern-alternation* :*)


(defun pattern-tree? (form compile-pattern-sign)
  (and (listp form)
       (atom (first form))
       (string= (first form)
                compile-pattern-sign)))


(defun compile-pattern-expression-snippet (container-name tree)
  (cond ((pattern-tree? tree *compile-pattern-optional*)
         ;; optional
         (if (> (length (rest tree)) 1)
           `(make-optional (d.iface:concat ,@(map 'list (lambda (subtree)
                                                  (compile-pattern-expression-snippet container-name
                                                                                      subtree))
                                          (rest tree))))
           `(make-optional ,(compile-pattern-expression-snippet container-name (second tree)))))
        ((pattern-tree? tree *compile-pattern-sequence*)
         ;; sequence
         `(d.iface:concat ,@(map 'list (lambda (subtree)
                                 (compile-pattern-expression-snippet container-name
                                                                     subtree))
                         (rest tree))))
        ((pattern-tree? tree *compile-pattern-alternation*)
         ;; alternation
         `(d.iface:combine ,@(map 'list (lambda (subtree)
                                  (compile-pattern-expression-snippet container-name
                                                                      subtree))
                          (rest tree))))
        ((pattern-expression-bare-keyword? tree)
         ;; :some-pattern
         `(get-pattern ,container-name ,tree))
        ((pattern-expression-trivial-reference-term? tree)
         ;; (:trivial "trivial_word" "tw")
         ;; ---> translates into --->
         ;; (let* ((<word symbol> "trivial_word")
         ;;        (<shorthand symbol> "tw")
         ;;        (<type symbol> (trivial-pattern-type <word symbol> <shorthand symbol>)))
         ;;   (or (get-pattern <container> <type symbol>)
         ;;       (get-pattern <container> (add-trivial-pattern <container>
         ;;                                                     <word symbol>
         ;;                                                     <shorthand symbol>))))
         (with-gensyms
           (word shorthand behavior-type handler-fun)
           `(let* ((,word ,(second tree))
                   (,shorthand ,(third tree))
                   (,behavior-type (trivial-pattern-type ,word ,shorthand))
                   (,handler-fun ,(fourth tree)))
              (or (get-pattern ,container-name ,behavior-type)
                  (get-pattern ,container-name (add-trivial-pattern ,container-name
                                                                    ,word
                                                                    ,shorthand
                                                                    ,handler-fun))))))
        ((pattern-expression-reference-term? tree)
         ;; (:behavior-type . <info>)
         ;; Generally it goes like this:
         ;;
         ;; (:behavior-type :name (some expr) . <other info>)
         ;; ---> translates into --->
         ;; (let ((<name symbol> (some expr)))
         ;;   (get-pattern <container>
         ;;                :behavior-type
         ;;                <name symbol>
         ;;                <other info>))
         ;;
         ;; === OR ===
         ;;
         ;; (:behavior-type . <other info>)
         ;; ---> translates into --->
         ;; (let ((<name symbol> (format nil "~(~A~)" :behavior-type)))
         ;;   (get-pattern <container>
         ;;                :behavior-type
         ;;                <name symbol>
         ;;                <other info>))
         ;;
         ;; === OR ===
         ;;
         ;; (:behavior-type)
         ;; ---> translates into --->
         ;; (let ((<name symbol> nil))
         ;;   (get-pattern <container>
         ;;                :behavior-type
         ;;                <name symbol>
         ;;                nil))
         (let* ((behavior-type (first tree)) ; Can safely do with a keyword, w/o gensym
                (name-name (gensym)) 
                (other-info (rest tree))
                (name-in-plist (get-name-from-plist other-info))
                (name (when other-info
                        (or name-in-plist
                            `(format nil "~(~A~)" ,behavior-type)))))
           (when name-in-plist
             (setf (getf other-info :name) name-name))
           `(let ((,name-name ,name))
              (get-pattern ,container-name
                           ,behavior-type
                           ,name-name
                           (list ,@other-info)))))
        (:else
          ;; last default - it's a pattern of user
          tree)))


; ...*this* side of insanity. Don't delude yourself which is which.
; Yet I have never felt myself more lucid then right now.
; Does every schizo feel this way? Is this a sign of sanity slipping?


;; (:type pattern-expr handler keyword-args...)
(defmacro set-behaviors (container &body set-forms)
  (let ((container-name (gensym)))
    `(let ((,container-name ,container))
       ,@(loop :for form :in set-forms
               :collect `(set-behavior ,container-name
                                       ,(first form)
                                       ,(compile-pattern-expression-snippet container-name
                                                                            (second form))
                                       ,(third form)
                                       ,@(nthcdr 3 form))))))


(defun pattern-let-snippet (container-name let-symbol let-forms body)
  `(,let-symbol ,(loop :for let-form :in let-forms
                       :collect (list (first let-form)
                                      (compile-pattern-expression-snippet container-name
                                                                          (second let-form))))
                ,@body))


(defmacro pattern-let (container (&rest let-forms) &body body)
  (with-gensyms
    (container-name)
    `(let ((,container-name ,container))
       ,(pattern-let-snippet container-name 'cl:let let-forms body))))


(defmacro pattern-let* (container (&rest let-forms) &body body)
  (with-gensyms
    (container-name)
    `(let ((,container-name ,container))
       ,(pattern-let-snippet container-name 'cl:let* let-forms body))))


(defun make-expander-callback (parser handler)
  (lambda (command-string)
    (multiple-value-bind (success match) (funcall parser command-string)
      (if success
        (values t (funcall handler match))
        (values nil nil)))))


;; TODO: Very bad, rewrite, ples
;; (pattern-expr handler docs keyword-args...)
(defun collect-yields-snippet (container-name forms)
  (with-gensyms
    (ul-name el-name)
    (let ((yields (loop :for form :in forms
                        :collect (with-gensyms
                                   (pattern-expr-name rxname srxname erxname handler-name options-name)
                                   `(let ((,pattern-expr-name ,(compile-pattern-expression-snippet container-name
                                                                                                   (first form))))
                                      (with-slots (regex short-regex expander-short-regex docform short-docform) ,pattern-expr-name
                                        (let ((,rxname (d.regex:make-scanner regex))
                                              (,srxname (when short-regex
                                                          (d.regex:make-scanner short-regex)))
                                              (,erxname (when expander-short-regex
                                                          (d.regex:make-scanner expander-short-regex)))
                                              (,handler-name ,(second form))
                                              (,options-name (list ,@(nthcdr 3 form))))
                                          (list (list (d.c.aux:make-rmacro-callback ; rmacro-callbacks, full & short
                                                        (d.c.aux:make-regex-parser ,rxname)
                                                        (apply #'d.expr:wrap-with-lexicon
                                                               ,ul-name
                                                               ,handler-name
                                                               ,options-name))
                                                      (when ,srxname
                                                        (d.c.aux:make-rmacro-callback
                                                          (d.c.aux:make-regex-parser ,srxname)
                                                          (apply #'d.expr:wrap-with-lexicon
                                                                 ,ul-name
                                                                 ,handler-name
                                                                 ,options-name))))
                                                (list (when ,erxname ; expander-callbacks
                                                        (make-expander-callback
                                                          (d.c.aux:make-regex-parser ,erxname)
                                                          (funcall #'d.expr:wrap-with-lexicon
                                                                   ,el-name
                                                                   +default-top-short-expander+
                                                                   :use-only-named-results nil))))
                                                (canon-form-to-autocomplete (canon-form ,pattern-expr-name)) ; canon-form
                                                (list (list (funcall *doc-expr-finalizer* (doc-expr docform)) ; documentation
                                                            (when short-docform
                                                              (funcall *doc-expr-finalizer* (doc-expr short-docform)))
                                                            ,(third form)))
                                                (list regex short-regex))))))))) ; sampled regexes to check collisions/incompatible regexes
      `(with-slots ((,ul-name utility-lexicon)
                    (,el-name expander-lexicon)) ,container-name
         (map 'list (lambda (yield-list)
                      (remove nil (reduce #'append yield-list)))
              (d.aux:rotate (list ,@yields)))))))


(defmacro collect-yields (container &body forms)
  (with-gensyms
    (container-name)
    `(let ((,container-name ,container))
       ,(collect-yields-snippet container-name forms))))


(define-condition incompatiple-regexes (error)
  ((incompatible-pairs :initarg :pairs :reader incompatible-pairs)))


(defun yields-into-application (rmacro-callbacks expander-callbacks canon-forms docs sampled-scanners &rest other &key &allow-other-keys)
  (let ((pairs (d.regex:find-incompatible-sampled-regexes sampled-scanners)))
    (if (null pairs)
      (apply #'d.app:push-new-application
             :rmacro-callbacks rmacro-callbacks
             :expander-callbacks (map 'list (lambda (cb)
                                              (d.cli:wrap-expander-callback-with-command-character cb #\/))
                                      expander-callbacks)
             :autocomplete-tree (d.cli:add-command-character-to-autocomplete-tree
                                  (d.cli:make-autocomplete-tree-from-lists
                                    canon-forms)
                                  #\/)
             :docs docs
             other)
      (error 'incompatiple-regexes :pairs pairs))))


(defmacro compile-into-application (container forms &rest other &key &allow-other-keys)
  (with-gensyms
    (container-name)
    `(let ((,container-name ,container))
       (apply #'yields-into-application
              (append ,(collect-yields-snippet container-name forms)
                      (list ,@other))))))


(defmacro return-application (container forms &rest other &key &allow-other-keys)
  (with-gensyms
    (container-name)
    `(let ((,container-name ,container))
       `(apply #'yields-into-application
               (append ',,(collect-yields-snippet container-name forms)
                       ',(list ,@other))))))


(declaim (ftype (function (keyword))
                return-keyword))
(defun return-keyword (kw)
  "The dumb version, when you want to return a keyword as a result of a match."
  (d.expr:return-match (lambda (info arg)
                         (declare (ignore arg))
                         (or (getf info :name) kw))
                       (lambda (info arg)
                         (declare (ignore info arg))
                         kw)))
