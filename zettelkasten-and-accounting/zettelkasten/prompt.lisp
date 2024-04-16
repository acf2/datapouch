;;;; zettelkasten/prompt.lisp


(in-package :zac.box)


;;; Four alphanumeric characters is enough for 36 ^ 4 = 1679616 IDs. Which is quite large number.
(defparameter *zettelkasten-prompt-character-count* 4)
(defparameter *zettelkasten-prompt-ceiling* (expt 36 *zettelkasten-prompt-character-count*))
(defparameter *zettelkasten-prompt-prime* 1679609) ; First prime less than ceiling
(defparameter *zettelkasten-prompt-primitive-root* 839888) ; "Pretty" primitive root, handpicked


;;; Try to make some permament ID for notes, but do not show their true number
;;; (for these perfectionists, who cannot stand automatic sequential ID numbering, like me)
(defun get-prompt ()
  ;(when *current-note* (format nil "~36,V,'0R" (1+ (floor (log (max-note-id) 36))) *current-note*)))
  (when *current-note* (format nil
                               "~36,V,'0R"
                               *zettelkasten-prompt-character-count*
                               (zac.aux:expt-mod *zettelkasten-prompt-primitive-root*
                                                 *current-note*
                                                 *zettelkasten-prompt-prime*))))
