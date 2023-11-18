;;;; command-wrapper.lisp


(in-package :zac.command-wrapper)


(defun acceptable-regex? (arg)
  (and (not (list element))
       (or (stringp element)
           (member (type-of element) (list 'd.regex:regex 'd.regex:regex-scanner)))))


(defun acceptable-simple-command-term? (term)
  (or (acceptable-regex? term) ; Simple form, like "\\w+"
      (and (consp term)        ; Named group, like (:name . "\\w+")
           (or (stringp (car term)) (symbol (car term)))
           (acceptable-regex? (cdr term)))))


(defparameter +simple-command-fun-errors+ "~A: Each argument should be string, regex, cons of symbol and string or regex, or the same but as a first element in a modifier list.~&")


(defun make-simple-command-scanner (command &rest terms)
  (flet ((argument-with-modifiers? (arg) (and (consp arg)
                                              (listp (cdr arg)))))
    (if (and (acceptable-regex? command)
             (every (lambda (element)
                      (or (acceptable-simple-command-term? element) ; Just an argument, like "\\w+" or (:name . "\\w+")
                          (and (argument-with-modifiers? element)   ; ...or argument with modifiers, like ("\\w+" :optional) or ((:name . "\\w+") :optional)
                               (acceptable-simple-command-term? (first element)))))
                    terms))
      (apply #'d.regex:make-command-regex-scanner
             (mapcar (lambda (element)
                       (flet ((wrap-term (term) (if (argument-with-modifiers? element)
                                                  (cons term (rest element))
                                                  term)))
                         (let ((term (if (argument-with-modifiers? element)
                                       (first element)
                                       element)))
                           (wrap-term
                             (if (consp term)
                               (d.regex:make-named-group (car term) (cdr term))
                               term)))))
                     terms))
      (error (make-instance 'simple-error
                            :format-control +simple-command-fun-errors+
                            :format-arguments 'make-simple-command-scanner)))))


(defclass command-wrapper ()
  ((name :initarg :name
         :reader name)
   (s-forms :initarg :s-forms
              :reader s-forms)
   (parsers :initarg :parsers
            :reader parsers)
   (handler :initarg :handler
            :reader handler)
   (docs :initarg :docs
         :reader :docs)))


(defun add-all-commands ()
  (zac.box:add-zettelkasten-commands)
  (pushnew (make-instance 'command
                          :regex (make-command-regex-scanner "init")
                          :handler (lambda (str match)
                                     (declare (ignore str match))
                                     (init-everything)))
           *commands*)
  (pushnew (make-instance 'command
                          :regex (make-command-regex-scanner "hello"
                                                             (make-named-group :name "\\w+"))
                          :handler (lambda (str groups)
                                     (declare (ignore str))
                                     (format t "Hello, ~:(~A~)!~&"
                                             (d.regex:get-group :name groups))))
           *commands*))
