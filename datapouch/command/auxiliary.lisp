;;;; command/auxiliary.lisp


;;; Basic convenience for all command packages


(in-package :datapouch.command.auxiliary)


(defun make-regex-parser (regex &key ((:group-mode group-mode) t))
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
  (lambda (command-string)
    (multiple-value-bind (success match) (funcall parser command-string)
      (if success
        (if full-string
          (values t `(funcall ,handler ,command-string ',match))
          (values t `(funcall ,handler ',match)))
        (values nil nil)))))

