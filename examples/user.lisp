;;;; user.lisp
;;; These files were made to be somewhat jury-rigged in feel and composition
;;; it's config for particular user, not some production-quality code
;;; so you will see several examples of *very* bad and tangled lisp down there

(defparameter *config-directory* (pathname-directory (truename *load-pathname*)))

(defparameter *zettelkasten-path* (make-pathname :directory *config-directory*
                                                 :name "zettelkasten.lisp"))
(defparameter *bookkeeping-path* (make-pathname :directory *config-directory*
                                                 :name "bookkeeping.lisp"))

(load *zettelkasten-path*)
(load *bookkeeping-path*)

(defmethod get-prompt :around ((input interactive-input))
  (concatenate 'string
               (get-zettelkasten-prompt)
               (call-next-method input)))

;;; Add note number to prompt
;;; NOTE: You can inherit interactive-input for more drastic changes
(when (and (boundp '*can-modify-prompt*) *can-modify-prompt*)
  (defmethod get-prompt :around ((input interactive-input))
    (concatenate 'string
                 (when *current-note* (format nil "[~36,V,'0R] " (1+ (floor (log (max-note-id) 36))) *current-note*))
                 (call-next-method input))))
