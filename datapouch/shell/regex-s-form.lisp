;;;; regex-s-form.lisp


(in-package :datapouch.shell)


(defun acceptable-regex? (arg)
  (and (not (listp arg))
       (or (stringp arg)
           (member (type-of arg) (list 'd.regex:regex 'd.regex:regex-scanner)))))


;; Simple form, like "\\w+"
(defun simple-regex-group? (regex-group) 
  (acceptable-regex? regex-group))


;; Named group, like (:name . "\\w+")
;; Aka NRG
(defun named-regex-group? (regex-group)
      (and (consp regex-group)        
           (or (stringp (car regex-group)) (symbolp (car regex-group)))
           (acceptable-regex? (cdr regex-group))))


(defun acceptable-regex-group? (regex-group)
  (or (simple-regex-group? regex-group)
      (named-regex-group? regex-group)))


;; Just an argument, like "\\w+" or (:name . "\\w+"), or even already an object of regex class
(defun simple-argument? (term)
  (acceptable-regex-group? term))


(defun acceptable-modifier-for-argument? (modifier)
  (eq (type-of modifier) 'keyword))


;; ...or argument with modifiers, like ("\\w+" :optional) or ((:name . "\\w+") :optional)
(defun argument-with-modifiers? (term)
  (and (consp term)
       (acceptable-regex-group? (first term))
       (listp (rest term))
       (every #'acceptable-modifier-for-argument?
              (rest term))))


(defun acceptable-term? (term)
  (or (simple-argument? term)
      (argument-with-modifiers? term)))


;;; === EBNF ===
;;; s-form            ::= dispatch-regex {argument-regex}
;;; dispatch-regex    ::= regex
;;; argument-regex    = regex | (regex modifier {modifier})
;;; nonspace-modifier = :optional
;;; modifier          = <nonspace-modifier> | :immediate | :optionally-immediate
;;;
;;; NOTES:
;;;  :optional                 makes this argument optional
;;;  :immediate                won't separate this argument from previous one with spaces
;;;  :optionally-immediate     *may not* separate this argument with spaces


(defparameter +forced-separator+ "\\s+")
(defparameter +optional-separator+ "\\s*")

(defparameter +complete-regex-beginning+ "^\\s*")
(defparameter +complete-regex-ending+ "\\s*$")


(defun make-regex-s-form-scanner (argument-regexes &key ((:complete complete) t))
  (flet ((make-optional (regex) (d.regex:concat (d.regex:wrap-in-noncapturing-group regex) "?")))
    (d.regex:make-scanner
      (apply #'d.regex:concat
             (reduce #'append
                     (list
                       (when complete (list +complete-regex-beginning+))
                       (loop :for argument-regex :in argument-regexes
                             :for first-argument := t :then nil
                             :collect (cond ((listp argument-regex)
                                             (let* ((result (first argument-regex))
                                                    (modifiers (rest argument-regex))
                                                    (separator (if (member :optionally-immediate modifiers)
                                                                 +optional-separator+
                                                                 +forced-separator+)))
                                               (unless (or (member :immediate modifiers)
                                                           first-argument)
                                                 (setf result (d.regex:concat-two separator result)))
                                               (when (member :optional modifiers)
                                                 (setf result (make-optional result)))
                                               result))
                                            (first-argument argument-regex)
                                            (:else (d.regex:concat-two +forced-separator+ argument-regex))))
                       (when complete (list +complete-regex-ending+))))))))


(defparameter +s-form-errors+ "~A: Each argument should be either keyword (for references), string, regex, cons of symbol and string or regex, or the same but as a first element in a modifier list.~&")


;;; Converts s-form regex (without subexpressions!) to regex-scanner
(defun make-command-s-form-scanner (&rest terms)
  (format t "make-command-s-form-scanner call:~&")
  (flet ((process-term (term) (cond ((and (argument-with-modifiers? term)
                                          (named-regex-group? (first term)))
                                     (format t "NRG with opts: ~A~&" term)
                                     (cons (d.regex:make-named-group (car (first term))
                                                                     (cdr (first term))) (rest term)))
                                    ((named-regex-group? term)
                                     (format t "NRG: ~A~&" term)
                                     (d.regex:make-named-group (car term)
                                                               (cdr term)))
                                    (:else
                                     (format t "Simple term: ~A~&" term)
                                      term))))
    (if (every #'acceptable-term? terms)
        (let ((a (funcall #'make-regex-s-form-scanner
                          (map 'list #'process-term terms))))
          (format t "~&~%")
          a)
      (error (make-instance 'simple-error
                            :format-control +s-form-errors+
                            :format-arguments 'make-command-s-form-scanner)))))
