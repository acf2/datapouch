;;;; command/auxiliary.lisp


;;; Basic convenience for all command packages


(in-package :datapouch.command.auxiliary)


(defun make-regex-parser (regex &key ((:group-mode group-mode) t))
  "Make parser function from regex-like object."
  (let ((match-fun (if group-mode #'match-to-group-tree #'match-to-assoc)))
    (lambda (command-string)
      (multiple-value-bind (match-start match-end group-starts group-ends) (scan regex command-string)
        (if match-start
          (values t (funcall match-fun
                             command-string
                             (group-list regex)
                             (group-map regex)
                             match-start
                             match-end
                             group-starts
                             group-ends))
          (values nil nil))))))


(defun make-rmacro-callback (parser handler &key ((:full-string-is-needed full-string) nil))
  "Fuse parser and handler functions into one callback. Parser is expected to
handle one argument - command-string, and return two values: was match
successful, and the match itself. Handler must be able to handle one argument -
match, and return one value too: resulting form to be evaluated. In the case of
full-string-is-needed, handler must handle two arguments: full command string
and match."
  (lambda (command-string)
    (multiple-value-bind (success match) (funcall parser command-string)
      (if success
        (if full-string
          (values t `(funcall ,handler ,command-string ',match))
          (values t `(funcall ,handler ',match)))
        (values nil nil)))))

