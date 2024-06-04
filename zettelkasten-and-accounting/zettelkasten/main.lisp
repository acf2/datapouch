;;;; zettelkasten.lisp
;;; Zettelkasten example
;;;
;;; WARNING! Every and any note interaction must interpret a note as ID number
;;; Note lists are lists of IDs (lists of integers)
;;; This is the crucial component in maintaining consistency between application and database
;;;
;;; All links should be represented by pairs (source . destination)
;;;
;;; When there is a fields argument in any function, it's an assoc list
;;; ((:field_symbol "Column name for output") (:field_symbol2) ...)
;;; Fields without column name for output will not be printed


(in-package :zac.box)


;;; Returns list of all notes without parents (no links with destination == note.id)
;;; There should be only one such a note in normal zettelkasten
;(defun get-orphans ()
;  (select '(:id :text)
;    (from :note)
;    (left-join :link :on (:= :link.destination :note.id))
;    (where (:is-null :link.source))))


;;; Find notes from db, using substring
;;; Subset of notes can be limited by choosing specific list of IDs
;;; deprecated
(defun filter-notes-by-substring (substring &key ((:in note-list) nil))
  (declare (type list note-list))
  (let ((where-clause (list :instr :text substring)))
    (when note-list
      (setf where-clause (list :and where-clause (list :in :id note-list))))
    (select :id
            (from :note)
            (where where-clause))))


; TODO
;(defun goto-history
;(defun goto-link


;;; As with `find-note' but more interactive. Obsolete.
;(defun note-search ()
;  (with-state 'search
;    (format t "Please enter part of note or its ID to find it:~&")
;    (loop :for info = (read-form *input*)
;          :if (stringp info) :return (find-note info)
;          :else :if (integerp info) :return (get-note-by-id info)
;          :else :do (format t "That is not string or a number. Try again:~&"))))


;;; Add new note
;;; :NUMBER will determine number of this link (is userful for sorting and
;;; tables of contents)
;;; NOTE: Exception to "interaction rule"
(defun add-note (text source-note &optional (number nil))
  (let ((new-note (caar (insert-into :note
                                     (set= :text text)
                                     (returning :id)))))
    (when source-note
      (insert-into :link (set= :source source-note
                               :destination new-note
                               :number number)))))


;;; Remove note by ID (or current note if ID is not supplied)
;;; When nil is supplied, does nothing
(defun remove-note (note)
  (when note
    (delete-from :note (where (:= :id note)))
    (when (eql note *current-note*)
      (set-current-note nil)))) ; TODO Remake to last history item, when history is implemented


;;; Show all links of a note
(defun show-links (note)
  (if note
    (pretty-print-table
      '("Number" "Note")
      (select ((:ifnull :link.number "") :note.text)
              (from :link)
              (left-join :note :on (:= :link.destination :note.id))
              (where (:= :link.source note))
              (order-by (:asc :link.number))))
    (format *standard-output* +msg-note-is-not-chosen+)))

;; TODO
;(defun add-link
;  backlink?
;(defun remove-link


;;; HIGH LEVEL (Includes some measure of user interaction)


;;; Interactive dialog to choose a note
;;; Returns note id
(defun choose-note-dialog (fields &rest clauses)
  (first (choose-row-from-table-dialog (from :note)
                                       (list* '(:id) fields)
                                       *choose-note-prompt*
                                       clauses)))


;;; legacy
(defun choose-note-interactive (notes)
  (choose-note-dialog '((:text "Text")) (where (:in :id notes))))


;;; Interactive dialog to choose a link
;;; Returns list (link.source link.destination)
(defun choose-link-dialog (fields &rest clauses)
  (subseq (choose-row-from-table-dialog (list (from :link)
                                              (inner-join :note :on (:= :link.destination :note.id)))
                                        (list* '(:link.source) '(:link.destination) fields)
                                        *choose-link-prompt*
                                        clauses)
          0 2))


;;; Go to link of some note and return destination note ID
;;; I wanted order-by-text to be an argument. Yes, I know about dynamic binding
;;; legacy
(defun choose-link-interactive (links &key ((:show-number show-number) t) ((:order-by-text order-by-text) *order-by-text*))
  (declare (type list links))
  (let* ((sorting-clause (when (or show-number order-by-text)
                           (d.sql:build :order-by
                                        (when show-number (list :asc :link.number))
                                        (when order-by-text (list :asc :note.text)))))
         (fields (zac.aux:list-existing (when show-number
                                          (list :link.number "Number"))
                                        :note.text)))
    (choose-link-dialog fields
                        (where (:in (d.sql:column-tuple (list :link.source :link.destination)) links))
                        sorting-clause)))


;"SELECT * FROM link WHERE (a IN (?, ?))"
;(3 4)


;(defun jump-link (


;;; TBD
;;; [ ] Core functions
;;;   [ ] Pretty print links/notes from current one -> goto, links
;;;     [ ] show numbering?
;;;   [x] Root node with fixed ID?
;;;   [ ] add note
;;;     [x] just add
;;;     [ ] add and go
;;;         A la "continue"; like it's just another part of a whole document/sequence?
;;;         Use number 0 for it?
;;;         Then:
;;;       [ ] sequential read (like in normal zettelkasten?)
;;;   [ ] remove note
;;;     [ ] Using links as a means of direction? Search?
;;;       [ ] backlinks?
;;;   [x] show link/backlinks
;;;   [ ] add links
;;;     [ ] backlinks?
;;;     [ ] Type of additions?
;;;       [ ] Through search
;;;       [ ] Through history
;;;       [ ] Freeform?
;;;     [ ] Link two other notes?
;;;       [ ] Through links? 
;;;   [ ] remove links
;;;     [ ] from backlinks only?
;;;   [x] goto / search
;;;     [x] Backlinks?
;;;     [x] Closure (kleene star) on links/backlinks?
;;;   [ ] History
;;;     [ ] History support
;;;     [ ] History traversal
;;; [ ] Tags ???
;;;   [ ] Tags tables + add / remove / list
;;;   [ ] Tagging notes
;;;   [ ] Goto / Search with tags
;;;   [ ] Tag workspaces
;;;       Implicit tags mechanic (alt - workspace defined by tags)
;;;       Or should I? Maybe it's not that bright of an idea
;;; [ ] Goto random note?

;;; COMMAND HELP / TODO
;;; /note (/n) - open text editor for current note +
;;; goto != jump
;;;   /goto forward/back/any N* [+tags] [-tags] <substring> (/gfN* +-, /gbN* +-, /gaN* +-) - interactive link choice from current note: possibly filtered by substring
;;;   /jump <N1> ... <Nm> (/j) - special command for non-interative link choice, can be chained to form "paths" of links
;;;                              Only forward. Somewhat service value for big Zettelkastens.
;;;                              Number - number of a link, if exists (or text inside note)
;;;                              Text - text inside note
;;;   /search [+tags] [-tags] <substring> (/s) - global goto substring (slow)
;;; /<linkcommand> N* - show links from this note in forward/backward/all directions
;;;                     number after the command shows how much times zt should travel
;;;                     asterisk shows if closure is needed (all levels deep notes from 1 to N will be shown)
;;;   /links (/l)
;;;   /links back (/lb)
;;;   /connections (/la) - kinda too complex for now, if accounting for closure option
;;;   /goto + /<linkcommand>?
;;;
;;; /add (/a), /remove (/r) + note (n), link (l)
;;;   /add note (/an) - adds note, with link from current to new, opens editor for new note body +
;;;   /add link (/al) [/add backlink (/abl)] - adds link (backlink) from (to) this note.
;;;
;;;   /remove note (/rn) - removes current node, jumps back in history
;;;
;;;   Interactive choice:
;;;     /remove notes? [by substring] (/rn <substr>)
;;;     /remove notes? by search (/rns)
;;;     /remove notes? by jump (/rnj)
;;;
;;;   Interactive choice:
;;;     /remove links? (/rl)
;;;     /remove backlinks? (/rbl)
;;;     /remove connections? (/ral)
;;;
;;; /history (/h) - show history in full (maybe /history short (/hs)?)
;;; /<history command> <N> - History traverse
;;;   history traverse does not change history, obviously
;;;   /back (/b)
;;;   /forward (/f)

;;; COMMANDS


(defun command-add-note (string match)
  (declare (ignore string match))
  (let ((new-note (first (edit-strings ""))))
    (if (string= new-note "")
      (format *standard-output* +msg-abort-note-creation+)
      (add-note new-note *current-note*))))


(defun command-note (string match)
  (declare (ignore string match))
  (when *current-note*
    (edit-notes (where (:= :id *current-note*)))))


(defun command-home (string match)
  (declare (ignore string match))
  (when (select :* (from :note) (where (:= :id 0)))
    (set-current-note 0)))


(defun parse-link-parameters (match)
  (let* ((backward? (and (get-group :type match)
                         (or (string= (get-group :type match) "back")
                             (string= (get-group :type match) "b"))))
         (exponent (if (get-group :exponent match)
                     (parse-integer (get-group :exponent match))
                     1))
         (closure? (not (null (get-group :closure match)))))
    (values backward? exponent closure?)))


(defun from-note-through-links-choosing-command (command string match)
  (declare (ignore string))
  (let* ((parameters (multiple-value-list (parse-link-parameters match)))
         (transformed-rows (apply #'zac.box.travel:select-notes-through-links *current-note* parameters))
         (chosen-row (apply #'zac.box.travel:choose-row-from-note-through-links transformed-rows *choose-note-prompt* parameters)))
    (cond ((null transformed-rows)
           (format *standard-output* +msg-no-notes+))
          ((null chosen-row)
           (format *standard-output* +msg-note-is-not-chosen+))
          (:else
            (funcall command chosen-row)))))


(defun command-goto (string match)
  (from-note-through-links-choosing-command (lambda (row)
                                              (set-current-note (getf row :id)))
                                            string
                                            match))


(defun command-links (string match)
  (declare (ignore string))
  (let* ((parameters (multiple-value-list (parse-link-parameters match)))
         (transformed-rows (apply #'zac.box.travel:select-notes-through-links *current-note* parameters)))
    (apply #'zac.box.travel:pretty-print-note-through-links transformed-rows parameters)))


;;; Alternative definition of goto commands
;;; For reference
(defun get-goto-commands ()
  ;; Goto
  ;; /goto (forward|back)?(:\\d+)?(\\*)?
  ;; /g(f|b)?(\\d+)?(\\*)?
  (list (make-instance 'command
                       :regex (concat "^\\s*"
                                      "goto"
                                      (wrap-in-noncapturing-group (concat "\\s+" (make-named-group :type "forward|back"))) "?"
                                      (wrap-in-noncapturing-group (concat ":" (make-named-group :exponent "\\d+"))) "?"
                                      (make-named-group :closure "\\*") "?"
                                      "\\s*$")
                       :handler #'command-goto)
        (make-instance 'command
                       :regex (concat "^\\s*"
                                      "g"
                                      (make-named-group :type "f|b") "?"
                                      (make-named-group :exponent "\\d+") "?"
                                      (make-named-group :closure "\\*") "?"
                                      "\\s*$")
                       :handler #'command-goto)))


;  (list (zac.cmd:make-command-wrapper '(("add" "note")
;                                        ("an"))
;                                      (lambda (str match)
;                                        (declare (ignore str match))
;                                        (init-everything)))
;  (list (zac.cmd:make-command-wrapper '(("remove" "note" ((:rest . ".*") :optional))
;                                        ("rn" ((:rest . ".*") :optional)))
;                                      (lambda (str match)
;                                        (declare (ignore str match))
;                                        (init-everything)))
;        (zac.cmd:make-command-wrapper '(("h(?:ello)?" ("dear" :optional) (:name . "\\w+"))
;                                        ("greet" (:name . "\\w+")))
;                                      (lambda (str groups)
;                                        (declare (ignore str))
;                                        (format t "Hello, ~:(~A~)!~&"
;                                                (d.regex:get-group :name groups))))))))

(defun get-zettelkasten-commands ()
  (let* ((substring-rx ".*")
         (substring-arguments `(((:substring . ,substring-rx) :optional)))
         (tags-rx "(?:\\w+,)*\\w+")
         (tag-arguments `((,(concat "\\+" (make-named-group :ptags tags-rx)) :optional)
                          (,(concat "-" (make-named-group :ntags tags-rx)) :optional)))
         (link-arguments `(((:type . "forward|back") :optional)
                           ((:exponent . "[1-9]\\d*") :optional)
                           ((:closure . "\\*") :optional :immediate)
                           ,@tag-arguments
                           ,@substring-arguments))
         (short-link-arguments `(((:type . "f|b") :optional :immediate)
                                 ((:exponent . "[1-9]\\d*") :optional :immediate)
                                 ((:closure . "\\*") :optional :immediate)
                                 ,@tag-arguments
                                 ,@substring-arguments))
         (goto-rxs `(("goto" ,@link-arguments)       ; /goto [forward][:<N>][*] [<substring>] | /goto back[:<N>][*] [<substring>]
                     ("g" ,@short-link-arguments)))  ; /g[f][<N>][*] [<substring>] | /gb[<N>][*] [<substring>]
         (search-rxs `("s(?:earch)?"                                                             ; /s[earch] [+tag,tag,...] [-tag,tag,...] <substring>
                       ,@tag-arguments
                       ,@substring-arguments))
         (links-rxs `(("links" ,@link-arguments)
                      ("l" ,@short-link-arguments))))
    ;; Simple commands
    (generate-commands
      (create-shell-commands '(("add" "note") ("an")) #'command-add-note "Adding note"
                             '(("note") ("n")) #'command-note "Edit current note"
                             '("home") #'command-home "Go to root note"
                             search-rxs #'command-search-note "Search by substring across all zettelkasten"
                             goto-rxs #'command-goto "Go to specific note using links, starting from current one"
                             links-rxs #'command-links "Show links from this note with specified parameters"))))



;;; SERVICE


(defun zettelkasten-init-hook ()
  (when (select :* (from :note) (where (:= :id 0)))
    (set-current-note 0)))
