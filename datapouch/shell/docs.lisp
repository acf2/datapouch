;;;; docs.lisp
;;; Building documentation from expressions


(in-package :datapouch.shell)


(defmethod print-expression-brief-help ((expression shell-expression) &optional (stream *standard-output*))
  (let ((doc (docs expression)))
    (when doc
      (if (listp doc)
        (format stream "~{~#[~;~A~:;~A | ~]~}:~%~4,0T~A~&" (rest doc) (first doc))
        (format stream "~A~&" doc)))))


; TODO: Check for existance of subexpression
;       or... is it necessary?
(defmethod print-expression-help ((expression shell-expression) (shell shell) &optional (stream *standard-output*))
  (let ((*print-pretty* nil))
    (labels ((invocation-help
               (expr &optional (tabs 0) (passed-id nil) (top-form t))
               (remove-duplicates
                 (loop :for s-form :in (s-forms expr)
                       :for subexpression-types := (remove-duplicates (map 'list #'rest (get-s-form-subexpressions s-form)))
                       :do (format stream "~V,0T~@[~S -> ~]~S~&" (* tabs 4) passed-id s-form)
                       :append (append subexpression-types
                                       (loop :for subexpression-type :in subexpression-types
                                             :append (invocation-help (gethash subexpression-type
                                                                               (subexpressions shell))
                                                                      (1+ tabs)
                                                                      subexpression-type
                                                                      nil)))
                       :do (when top-form
                                  (format stream "~%"))))))
      (print-expression-brief-help expression)
      (format stream "~%Invocation help:~&")
      (let ((all-subexpression-types (sort (invocation-help expression) #'string<)))
        (loop :for subexpression-type :in all-subexpression-types
              :do (format stream "~S - ~A~&" subexpression-type (docs (gethash subexpression-type
                                                                               (subexpressions shell)))))))))


(defun print-expressions-docs (expression-list)
  (let* ((with-doc-forms (remove-if-not (lambda (expr)
                                          (and (listp (docs expr))
                                               (rest (docs expr))))
                                        expression-list))
         (with-just-docs (remove-if (lambda (expr)
                                      (or (member expr with-doc-forms)
                                          (null (docs expr))))
                                    expression-list))
         (without-docs-count (count-if-not #'docs expression-list)))
    (loop :for command :in (sort with-doc-forms
                                 #'string<
                                 :key (lambda (command)
                                        (string-upcase (first (docs command)))))
          :do (print-expression-brief-help command))
    (when with-just-docs
      (format *standard-output* "~%Commands without designated names:~&")
      (loop :for command :in with-just-docs
            :do (print-expression-brief-help command)))
    (when (> without-docs-count 0)
      (format *standard-output* "~%...and ~A undocumented commands.~&" without-docs-count)))) ; but 1 command_ tho


(defun add-help-to-shell (shell &optional (command-string "help") (unnamed-command-string "help-unnamed"))
  (declare (ignore unnamed-command-string))
  (with-slots (complete-expressions) shell
    (add-shell-commands
      shell
      (`((,command-string ((:command-form . ".+?") :optional)))
        (lambda (&key ((:command-form command-form) nil))
          (if command-form
            (let ((expr (gethash command-form (doc-table shell))))
              (if expr
                (print-expression-help expr shell)
                (format *standard-output* "Command form not found: ~A~&" command-form)))
            (print-expressions-docs complete-expressions)))
        (list "help" "Show list of all available commands, and then interactively show detailed docs about one command"))
      ;TODO help-unnamed
      )))
