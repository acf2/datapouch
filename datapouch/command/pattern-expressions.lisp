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
  (put-into container
            (make-behavior :space
                           +default-space-pattern+
                           nil
                           (constantly (string #\Space))))
  (put-into container
            (make-behavior :begin
                           +default-begin-pattern+
                           nil
                           (constantly "")))
  (put-into container
            (make-behavior :end
                           +default-end-pattern+
                           nil
                           (constantly ""))))


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


;; :some-pattern
(defun pattern-expression-bare-keyword? (term)
  (and (atom term)
       (typep term 'keyword)))


;; (:behavior-type . <info>)
(defun pattern-expression-reference-term? (term)
  (and (listp term)
       (or (typep (first term) 'keyword))))


;; (:trivial "trivial_word" "tw")
(defun pattern-expression-trivial-reference-term? (term)
  (and (listp term)
       (atom (first term))
       (member (first term)
               (list :trivial))
       (stringp (second term))
       (stringp (third term))))


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
           `(make-optional (concat ,@(map 'list (lambda (subtree)
                                                  (compile-pattern-expression-snippet container-name
                                                                                      subtree))
                                          (rest tree))))
           `(make-optional ,(compile-pattern-expression-snippet container-name (second tree)))))
        ((pattern-tree? tree *compile-pattern-sequence*)
         ;; sequence
         `(concat ,@(map 'list (lambda (subtree)
                                 (compile-pattern-expression-snippet container-name
                                                                     subtree))
                         (rest tree))))
        ((pattern-tree? tree *compile-pattern-alternation*)
         ;; alternation
         `(combine ,@(map 'list (lambda (subtree)
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
         (let ((word-name (gensym))
               (shorthand-name (gensym))
               (behavior-type-name (gensym)))
           `(let* ((,word-name ,(second tree))
                   (,shorthand-name ,(third tree))
                   (,behavior-type-name (trivial-pattern-type ,word-name ,shorthand-name)))
              (or (get-pattern ,container-name ,behavior-type-name)
                  (get-pattern ,container-name (add-trivial-pattern ,container-name
                                                                    ,word-name
                                                                    ,shorthand-name))))))
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
                (name (when (rest tree)
                        (or (get-name-from-plist (rest tree))
                            `(format nil "~(~A~)" ,behavior-type))))
                (other-info (rest tree)))
           (when (getf other-info :name)
             (setf (getf other-info :name) name-name))
           `(let ((,name-name ,name))
              (get-pattern ,container-name
                           ,behavior-type
                           ,name-name
                           ,other-info))))
        (:else
          ;; last default - it's a pattern of user
          tree)))


; ...*this* side of insanity. Don't delude yourself which is which.
; Yet I have never felt myself more lucid then right now.
; Does every schizo feel this way? Is this a sign of sanity slipping?


(defun set-behaviors () nil)
(defun collect-yields () nil)
(defun compile-into-application () nil)


;(defun compile-behavior-expression (container pattern-expression handler docs &rest options &key &allow-other-keys)
;  (declare (ignore docs))
;  "fart fart poooooooh... shite"
;  (list ,@(map 'list (lambda (term)
;                       (cond ((pattern-expression-bare-keyword? term)
;                              (let* ((behavior-type term))
;                                (get-pattern container
;                                             behavior-type)))
;                             ((pattern-expression-reference-term? term)
;                              (let* ((behavior-type (first term))
;                                     (name (or (get-name-from-plist (rest term))
;                                               (and (rest term)
;                                                    (format nil "~(~A~)" behavior-type))))
;                                     (get-pattern container
;                                                  behavior-type
;                                                  name
;                                                  (rest term))))
;                              ((pattern-expression-trivial-reference-term? term)
;                               (let* ((behavior-type (trivial-pattern-type (second (first term))))
;                                      (name (or (get-name-from-plist (rest term))
;                                                (and (rest term)
;                                                     (format nil "~(~A~)" (second (first term)))))))
;                                 (get-pattern container
;                                              behavior-type
;                                              name
;                                              (rest term))))
;                              (:else
;                                term))))
;               pattern-expression))
;
;  `(d.c.aux:make-rmacro-callback
;     (d.c.aux:make-regex-parser
;       (d.regex:make-scanner
;         (d.regex:concat-separated
;           (list ,@(map 'list (lambda (term)
;                                (if (and (li:stp term)
;                                         (typep (first term) 'keyword))
;                                  `(get-from-lexicon ,lexicon ,(first term) ,(rest term))
;                                  term))
;                        regex-list))
;           :separator-regex "\\s+"
;           :start-regex "^\\s*"
;           :end-regex "\\s*$"
;           :null-regex "^\\s*$")))
;     (wrap-with-lexicon ,lexicon ,handler ,@options)))
;
;
;(defparameter +with-patterns-macro-name+ 'with-patterns)
;(defparameter +with-patterns-final-name+ '#:)
;(defparameter +with-patterns-sub-name+ '#:behaving-pattern)
;
;
;(defun with-patterns-predicate (form)
;  (and (listp form)
;       (> (length form) 1)
;       (atom (first form))
;       (or (eq (first form) +with-lexicon-macro-name+)
;           (member (first form)
;                   (list +with-lexicon-command-name+
;                         +with-lexicon-expression-name+)
;                   :test #'string=))))
;
;
;(defun with-lexicon-transform (form current-lexicon)
;  (cond ((string= (first form) +with-lexicon-command-name+)
;         (values nil (apply #'make-command-with-lexicon-snippet current-lexicon (rest form))))
;        ((string= (first form) +with-lexicon-expression-name+)
;         (values nil (apply #'set-expression-with-lexicon-snippet current-lexicon (rest form))))
;        ((eq (first form) +with-lexicon-macro-name+)
;         (multiple-value-bind (_ new-tree) (d.aux:traverse (cddr form)
;                                                           #'with-lexicon-predicate
;                                                           (lambda (subtree)
;                                                             (with-lexicon-transform subtree (second form)))
;                                                           #'identity)
;           (values _ `(progn ,@new-tree))))
;        (:else (error 'should-not-be)))) ; XXX: Make pretty
;
;
;(defun with-lexicon-fun (lexicon forms)
;  (multiple-value-bind (_ new-tree) (d.aux:traverse (list* +with-lexicon-macro-name+
;                                                           lexicon
;                                                           forms)
;                                                    #'with-lexicon-predicate
;                                                    (lambda (subtree)
;                                                      (with-lexicon-transform subtree #'identity))
;                                                    #'identity)
;    (declare (ignore _))
;    new-tree))
;
;
;(defmacro with-lexicon (lexicon-expr &rest forms)
;  (let ((lexicon-name (gensym)))
;    `(let ((,lexicon-name ,lexicon-expr))
;       ,(with-lexicon-fun lexicon-name forms))))
;
;
;(defmacro with-new-lexicon (lexicon-name &rest forms)
;  `(let ((,lexicon-name (make-instance 'lexicon)))
;     ,(with-lexicon-fun lexicon-name forms)))
;
;
;(defmacro with-anonymous-lexicon (&rest forms)
;  (let ((lexicon-name (gensym)))
;    `(let ((,lexicon-name (make-instance 'lexicon)))
;       ,(with-lexicon-fun lexicon-name forms))))
