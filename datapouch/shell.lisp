;;;; shell.lisp


(in-package :datapouch.shell)


(defun acceptable-regex? (arg)
  (and (not (listp arg))
       (or (stringp arg)
           (member (type-of arg) (list 'd.regex:regex 'd.regex:regex-scanner)))))


(defun acceptable-simple-command-term? (term)
  (or (acceptable-regex? term) ; Simple form, like "\\w+"
      (and (consp term)        ; Named group, like (:name . "\\w+")
           (or (stringp (car term)) (symbolp (car term)))
           (acceptable-regex? (cdr term)))))


(defparameter +simple-command-fun-errors+ "~A: Each argument should be string, regex, cons of symbol and string or regex, or the same but as a first element in a modifier list.~&")


(defun make-simple-command-scanner (command &rest terms)
  (flet ((argument-with-modifiers? (arg) (and (consp arg)
                                              (listp (cdr arg)))))
    (if (and (acceptable-regex? command)
             (every (lambda (element)
                      (or (acceptable-simple-command-term? element) ; Just an argument, like "\\w+" or (:name . "\\w+"), or even already an object of regex class
                          (and (argument-with-modifiers? element)   ; ...or argument with modifiers, like ("\\w+" :optional) or ((:name . "\\w+") :optional)
                               (acceptable-simple-command-term? (first element)))))
                    terms))
      (apply #'d.regex:make-command-regex-scanner
             command
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


(defclass shell-command ()
  ((s-forms :initarg :s-forms
            :reader s-forms)
   (parsers :initarg :parsers
            :reader parsers)
   (handler :initarg :handler
            :reader handler)
   (docs :initarg :docs
         :reader :docs)))


(defgeneric add-shell-command-form (shell-command command-s-form &rest other-command-forms)
  (:documentation "Add another s-form for this shell-command handler to handle"))


(defmethod add-shell-command-form ((command shell-command) command-s-form &rest other-command-s-forms)
  (with-slots (s-forms parsers) command
    (loop :for new-s-form :in (cons command-s-form other-command-s-forms) :do
          (push new-s-form s-forms)
          (push (apply #'make-simple-command-scanner new-s-form) parsers))
    command))


(defun make-shell-command (command-s-form handler &optional docs)
  (let ((multiple-command-forms? (listp (first command-s-form))))
    (make-instance 'shell-command
                   :s-forms (if multiple-command-forms?
                              command-s-form
                              (list command-s-form))
                   :parsers (if multiple-command-forms?
                              (mapcar (lambda (s-form)
                                        (apply #'make-simple-command-scanner s-form))
                                      command-s-form)
                              (list (apply #'make-simple-command-scanner command-s-form)))
                   :handler handler
                   :docs docs)))


(defun generate-commands (shell-commands)
  (loop :for shell-command :in shell-commands
        :append (loop :for s-forms = (s-forms shell-command) :then (rest s-forms)
                      :for parsers = (parsers shell-command) :then (rest parsers)
                      :for current-s-form = (first s-forms)
                      :for current-parser = (first parsers)
                      :while s-forms
                      :while parsers
                      :collect (make-instance 'command
                                              :regex current-parser
                                              :handler (handler shell-command)))))


(defmacro create-shell-commands (&rest cmd-defs)
  `(list ,@(loop :for all-cmds := cmd-defs :then (cdddr all-cmds)
                 :for cmd-rx := (first all-cmds)
                 :for cmd-handler := (second all-cmds)
                 :for cmd-docs := (third all-cmds)
                 :while all-cmds
                 :collect `(make-shell-command ,cmd-rx ,cmd-handler ,cmd-docs))))
