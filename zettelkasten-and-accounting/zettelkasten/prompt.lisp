;;;; zettelkasten/prompt.lisp


(in-package :zac.box)


;;; Four alphanumeric characters is enough for 36 ^ 4 = 1679616 IDs. Which is quite large number.
(defparameter *zettelkasten-prompt-character-count* 4)
(defparameter *zettelkasten-prompt-ceiling* (expt 36 *zettelkasten-prompt-character-count*))
(defparameter *zettelkasten-prompt-prime* 1679609) ; First prime less than ceiling
(defparameter *zettelkasten-prompt-primitive-root* 839888) ; "Pretty" primitive root, handpicked


;; "~36,V,'0R" meaning "I want 4 character length 36-based number, padded with zeros
;; "~:[none~;other]" output none if argument is nil, otherwise other
;; "~@[smth]" output something, only if argument is not nil
;; ~1* - jump one argument forward
;; ~1:* - jump one argument backward
;; "~36,V,'0R" - format of N-symbol 36-based integer
;; XXX: Maybe, recursive formatting is actually better. This is starting to get complicated...
(defparameter +prompt-format+ "~:[~1*~;~1:*~36,V,'0R~]~1*~@[~1:*:~36,V,'0R~]")

(defun obfuscate-note-id (note-id)
  (zac.aux:expt-mod *zettelkasten-prompt-primitive-root*
                    note-id
                    *zettelkasten-prompt-prime*))


;;; Try to make some permament ID for notes, but do not show their true number
;;; (for these perfectionists, who cannot stand automatic sequential ID numbering, like me)
(defun get-prompt ()
  ;(when *current-note* (format nil "~36,V,'0R" (1+ (floor (log (max-note-id) 36))) *current-note*)))
  (format nil
          +prompt-format+
          *zettelkasten-prompt-character-count*
          (when *current-note* (obfuscate-note-id *current-note*))
          *zettelkasten-prompt-character-count*
          31337 ;(when *memorized-note* (obfuscate-note-id *memorized-note*))
          ))


;  (when *current-note* (format nil
;                               *zettelkasten-prompt-character-count*
;                               (zac.aux:expt-mod *zettelkasten-prompt-primitive-root*
;                                                 *current-note*
;                                                 *zettelkasten-prompt-prime*))))
