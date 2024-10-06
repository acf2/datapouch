;;; zettelkasten.lisp
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
                               :number number)))
    new-note))



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
    (format *standard-output* "~A~&" +msg-note-is-not-chosen+)))

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
;;;   [x] Pretty print links/notes from current one -> goto, links
;;;     [x] show numbering?
;;;   [x] Root node with fixed ID?
;;;   [x] add note
;;;     [x] just add
;;;     [h] add and go
;;;         A la "continue"; like it's just another part of a whole document/sequence?
;;;         Use number 0 for it?
;;;         Then:
;;;       [h] sequential read (like in normal zettelkasten?)
;;;   [ ] remove note
;;;     [ ] Using links as a means of direction? Search?
;;;       [ ] backlinks?
;;;   [x] show link/backlinks
;;;   [h] add links
;;;     [x] backlinks?
;;;     [h] Type of additions?
;;;       [\] Through search
;;;       [\] Through history
;;;       [x] Freeform?
;;;     [\] Link two other notes?
;;;       [\] Through links?
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
;;; [ ] Sequential read for /next
;;; [ ] Sequential read for content notes ("books"?)

;;; COMMAND HELP / TODO
;;; /jump <N1> ... <Nm> (/j) - special command for non-interative link choice, can be chained to form "paths" of links
;;;                            Only forward. Somewhat service value for big Zettelkastens.
;;;                            Number - number of a link, if exists (or text inside note)
;;;                            Text - text inside note
;;;                            goto != jump
;;; - /goto
;;;     +tags -tags
;;;     substring
;;;     any direction ??
;;; - /search
;;;     +tags -tags
;;; - /links all | /connections (/la) - kinda too complex for now, if accounting for closure option
;;; - command composition
;;;     /goto + /<linkcommand>?
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

;;; +++++  COMMAND HELP / DONE  +++++
;;;
;;;   ===  Command types  ===
;;;
;;; [E] Exponent commands:
;;;   Commands, that support repeated application
;;;
;;;   Invokation: <command> [number]
;;;   Short invocation: <cmd>[number]
;;;
;;; [D] Directed commands:
;;;   Support change of direction.
;;;
;;;   Invokation: <command> [forward|back]
;;;   Short invokation: <cmd>[f|b]
;;;
;;; [DAE] Directed Advanced Exponent commands:
;;;   These support repeated application, as well as change of direction and
;;;   possible closure "Closure" is the Kleene star: union of all repeated
;;;   applications in 1..N at once.
;;;
;;;   Invocation: <command> [forward|back] [number][*]
;;;   Short invocation: <cmd>[f|b][number][*]
;;;
;;; [S] Substring constrained command:
;;;   These commands' results can (or must) be filtered with some substring.
;;;
;;;   Invocation: <command> [<substring>]
;;;   Short invocation ???
;;;
;;;
;;;   ===  Display commands  ===
;;;
;;; /links [DAE]
;;;   Command displays all links fitting set criteria from current note.
;;;
;;;
;;;   ===  Movement commands  ===
;;;
;;; /next (/n) - goto next note in sequence ("natural order" of reading).
;;;              (internally this is achieved by number 0 for one particular link)
;;; /goto forward/back/any N* [+tags] [-tags] <substring> (/gfN* +-, /gbN* +-, /gaN* +-) - interactive link choice from current note: possibly filtered by substring
;;;   /search [+tags] [-tags] <substring> (/s) - global goto substring (slow)
;;;
;;;
;;;   ===  Editing commands  ===
;;;
;;; /edit (/e) - open text editor for current note

;;; COMMANDS


(defun parse-directed-args (match)
  (let* ((argsymbol :direction)
         (backward? (and (get-group argsymbol match)
                         (or (string= (get-group argsymbol match) "back")
                             (string= (get-group argsymbol match) "b")))))
    backward?))



(defun parse-link-parameters (match)
  (let* ((backward? (and (get-group :type match)
                         (or (string= (get-group :type match) "back")
                             (string= (get-group :type match) "b"))))
         (exponent (if (get-group :exponent match)
                     (parse-integer (get-group :exponent match))
                     1))
         (closure? (not (null (get-group :closure match)))))
    (values backward? exponent closure?)))


(defun parse-new-link-parameters (match)
  (let* ((next-in-sequence? (or (string= (get-group :number match) "next")
                                (string= (get-group :number match) "n")))
         (link-number (if next-in-sequence?
                        0
                        (and (get-group :number match)
                             (parse-integer (get-group :number match)))))
         (notes-with-number (and link-number
                                 (car (select '(:source :destination)
                                              (from :link)
                                              (where (:and (:= :source *current-note*)
                                                           (:= :number link-number))))))))
    (values next-in-sequence? link-number notes-with-number)))


;(defun wrap-catch-sqlite-errors (fun)
;  (lambda (string match)
;    (handler-case (funcall fun string match)
;      (sqlite:sqlite-error (err) (format *standard-output* +errfmt-generic-sqlite-error+ err)))))


(defun command-edit (string match)
  (declare (ignore string match))
  (when *current-note*
    (edit-notes (where (:= :id *current-note*)))))


(defun command-home-old (string match)
  (declare (ignore string match))
  (when (select :* (from :note) (where (:= :id 0)))
    (set-current-note 0)))


(defun command-home (command-string)
  (declare (ignore command-string))
  (when (select :* (from :note) (where (:= :id 0)))
    (set-current-note 0)))



(defun command-next (string match)
  (declare (ignore string match))
  (let ((next-note-id (caar (select :destination
                                    (from :link)
                                    (where (:and (:= :source *current-note*)
                                                 (:= :number 0)))))))
    (if next-note-id
      (set-current-note next-note-id)
      (format *standard-output* "~A~&" +msg-no-notes+))))


;(defun command-memorize (string match)
;  (declare (ignore string match))
;  (setf *memorized-notes* *current-note*))


;; command-list-memorized


;; command-choose-memory


;; clear memory [N1] [N2] ...
;(defun command-clear-memory (string match)
;  (declare (ignore string match))
;  (setf *memorized-notes* nil))
;
;
;(defun command-goto-memory (string match)
;  (declare (ignore string match))
;  (if *memorized-notes*
;    (set-current-note *memorized-notes*)
;    (format *standard-output* "~A~&" +msg-note-is-not-chosen+)))
;

(defun command-back (string match)
  (declare (ignore string match))
  (let ((old-note *current-note*)
        (new-note (first *note-history*)))
    (cond (new-note
             (set-current-note new-note :update-history nil)
             (setf *note-history* (rest *note-history*))
             (setf *note-future* (cons old-note *note-future*)))
           (:else
             (format *standard-output* "~A~&" +msg-no-notes+)))))


(defun command-forward (string match)
  (declare (ignore string match))
  (let ((old-note *current-note*)
        (new-note (first *note-future*)))
    (cond (new-note
             (set-current-note new-note :update-history nil)
             (setf *note-history* (cons old-note *note-history*))
             (setf *note-future* (rest *note-future*)))
           (:else
             (format *standard-output* "~A~&" +msg-no-notes+)))))


(defun from-note-through-links-choosing-command (command string match)
  (declare (ignore string))
  (let* ((parameters (multiple-value-list (parse-link-parameters match)))
         (transformed-rows (apply #'zac.box.travel:select-notes-through-links *current-note* parameters))
         (chosen-row (apply #'zac.box.travel:choose-row-from-note-through-links transformed-rows *choose-note-prompt* parameters)))
    (cond ((null transformed-rows)
           (format *standard-output* "~A~&" +msg-no-notes+))
          ((null chosen-row)
           (format *standard-output* "~%~A~&" +msg-note-is-not-chosen+))
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


(defun command-add-note (string match)
  (declare (ignore string))
  (multiple-value-bind (next-in-sequence? link-number notes-with-number) (parse-new-link-parameters match)
    (let* ((override-note-number? (and notes-with-number (yes-or-no-p +question-note-with-number-exists+)))
           (new-note (first (edit-strings ""))))
      (cond ((string= new-note "")
             (format *standard-output* "~A~&" +msg-abort-note-creation+))
            (:else
              (when override-note-number?
                (update :link
                        (set= :number nil)
                        (where (:and (:= :source (first notes-with-number))
                                     (:= :destination (second notes-with-number))))))
              (let ((new-note (add-note new-note
                                        *current-note*
                                        (if (and notes-with-number
                                                 (not override-note-number?))
                                          nil
                                          link-number))))
                (when next-in-sequence?
                  (set-current-note new-note))))))))


;(defun command-remove-note (string match)
;  (declare (ignore string))
;  nil)


;(defun command-add-link (string match)
;  (declare (ignore string))
;  (multiple-value-bind (next-in-sequence? link-number notes-with-number) (parse-new-link-parameters match)
;    (declare (ignore next-in-sequence?))
;    (let* ((direction (if (or (string= (get-group :direction match) "to")
;                              (string= (get-group :direction match) "t"))
;                        :forward
;                        :backward))
;           (source-column (if (eq direction :forward)
;                            :source
;                            :destination))
;           (destination-column (if (eq direction :forward)
;                                 :destination
;                                 :source))
;           (override-note-number? (and notes-with-number (yes-or-no-p +question-note-with-number-exists+))))
;      (cond ((select :*
;                     (from :link)
;                     (where (:and (:= source-column *current-note*)
;                                  (:= destination-column *memorized-notes*))))
;             (format *standard-output* +msg-link-exists+))
;            (:else
;              (when override-note-number? ; TODO: Wrong condition placement - won't work
;                (update :link
;                        (set= :number nil)
;                        (where (:and (:= :source (first notes-with-number))
;                                     (:= :destination (second notes-with-number))))))
;              (when (or (not notes-with-number)
;                        override-note-number?)
;                (insert-into :link
;                             (set= source-column *current-note*
;                                   destination-column *memorized-notes*
;                                   :number link-number))))))))


;(defun command-remove-link (string match)
;  (declare (ignore string))
;  nil)


;(defun command-modify-link (string match)
;  (declare (ignore string))
;  nil)


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



;;;;;; DEBUG
;;;;;; DEBUG
;;;;;; DEBUG
;;;;;; DEBUG

;(defclass shell-expression ()
;  ((s-forms :initarg :s-forms
;            :reader s-forms)
;   (handler :initarg :handler
;            :reader handler)
;   (docs :initarg :docs
;         :reader :docs)))
;
;
;(defmethod add-shell-expression-form ((expression shell-expression) expression-s-form &rest other-expression-forms)
;  (if (every #'acceptable-expression-s-form? (cons expression-s-form other-expression-forms))
;    (with-slots (s-forms lexers) expression
;      (loop :for new-s-form :in (cons expression-s-form other-expression-forms) :do
;            (push new-s-form s-forms))
;      expression)
;    (error (make-instance 'simple-error
;                          :format-control +s-form-errors+
;                          :format-arguments 'add-shell-expression-form))))
;
;
;(defun make-shell-expression (expression-s-form handler &optional docs)
;  (let ((multiple-expression-forms? (listp (first expression-s-form))))
;    (when (or (and (not multiple-expression-forms?)
;                   (acceptable-expression-s-form? expression-s-form))
;              (and multiple-expression-forms?
;                   (every #'acceptable-expression-s-form? expression-s-form)))
;      (make-instance 'shell-expression
;                     :s-forms (if multiple-expression-forms?
;                                expression-s-form
;                                (list expression-s-form))
;                     :handler handler
;                     :docs docs))))
;
;
;(defun generate-commands (shell-commands)
;  (loop :for shell-command :in shell-commands
;        :append (loop :for s-forms := (s-forms shell-command) :then (rest s-forms)
;                      :for parsers := (parsers shell-command) :then (rest parsers)
;                      :for current-s-form := (first s-forms)
;                      :for current-parser := (first parsers)
;                      :while s-forms
;                      :while parsers
;                      :collect (make-instance 'command
;                                              :regex current-parser
;                                              :handler (handler shell-command)))))
;
;
;(defmacro create-commands (&rest cmd-defs)
;  `(list ,@(loop :for all-cmds := cmd-defs :then (cdddr all-cmds)
;                 :for cmd-rx := (first all-cmds)
;                 :for cmd-handler := (second all-cmds)
;                 :for cmd-docs := (third all-cmds)
;                 :while all-cmds
;                 :collect `(make-shell-command ,cmd-rx ,cmd-handler ,cmd-docs))))


;(defclass shell-command ()
;  ((s-forms :initarg :s-forms
;            :reader s-forms)
;   (parsers :initarg :parsers
;            :reader parsers)
;   (handler :initarg :handler
;            :reader handler)
;   (docs :initarg :docs
;         :reader :docs)))
;
;
;(defgeneric add-shell-command-form (shell-command command-s-form &rest other-command-forms)
;  (:documentation "Add another s-form for this shell-command handler to handle"))
;
;
;(defmethod add-shell-command-form ((command shell-command) command-s-form &rest other-command-s-forms)
;  (with-slots (s-forms parsers) command
;    (loop :for new-s-form :in (cons command-s-form other-command-s-forms) :do
;          (push new-s-form s-forms)
;          (push (apply #'d.shell::make-command-s-form-scanner new-s-form) parsers))
;    command))
;
;
;(defun make-shell-command (command-s-form handler &optional docs)
;  (let ((multiple-command-forms? (listp (first command-s-form))))
;    (make-instance 'shell-command
;                   :s-forms (if multiple-command-forms?
;                              command-s-form
;                              (list command-s-form))
;                   :parsers (if multiple-command-forms?
;                              (mapcar (lambda (s-form)
;                                        (apply #'d.shell::make-command-s-form-scanner s-form))
;                                      command-s-form)
;                              (list (apply #'d.shell::make-command-s-form-scanner command-s-form)))
;                   :handler handler
;                   :docs docs)))
;
;
;(defun generate-commands (shell-commands)
;  (loop :for shell-command :in shell-commands
;        :append (loop :for s-forms = (s-forms shell-command) :then (rest s-forms)
;                      :for parsers = (parsers shell-command) :then (rest parsers)
;                      :for current-s-form = (first s-forms)
;                      :for current-parser = (first parsers)
;                      :while s-forms
;                      :while parsers
;                      :collect (make-instance 'command
;                                              :regex current-parser
;                                              :handler (handler shell-command)))))
;
;
;(defmacro create-shell-commands (&rest cmd-defs)
;  `(list ,@(loop :for all-cmds := cmd-defs :then (cdddr all-cmds)
;                 :for cmd-rx := (first all-cmds)
;                 :for cmd-handler := (second all-cmds)
;                 :for cmd-docs := (third all-cmds)
;                 :while all-cmds
;                 :collect `(make-shell-command ,cmd-rx ,cmd-handler ,cmd-docs))))


;;;;;; DEBUG
;;;;;; DEBUG
;;;;;; DEBUG
;;;;;; DEBUG

(defun get-zettelkasten-commands ()
  (let* ((shell (make-instance 'd.shell:shell))
         ;(substring-rx ".*")
         ;(substring-arguments `(((:substring . ,substring-rx) :optional)))
;         (tags-rx "(?:\\w+,)*\\w+")
;         (tag-arguments `((,(concat "\\+" (make-named-group :ptags tags-rx)) :optional)
;                          (,(concat "-" (make-named-group :ntags tags-rx)) :optional)))
;         (exponent-arguments `(((:exponent . "[1-9]\\d*") :optional)))
;         (short-exponent-arguments `(((:exponent . "[1-9]\\d*") :optional :optionally-immediate)))
;         (link-type-arguments `(((:type . "forward|back") :optional)))
;         (short-link-type-arguments `(((:type . "f|b") :optional :immediate)))
;         (link-arguments `(,@link-type-arguments
;                           ,@exponent-arguments
;                           ((:closure . "\\*") :optional :immediate)
;                           ,@tag-arguments
;                           ,@substring-arguments))
;         (short-link-arguments `(,@short-link-type-arguments
;                                 ,@short-exponent-arguments
;                                 ((:closure . "\\*") :optional :immediate)
;                                 ,@tag-arguments
;                                 ,@substring-arguments))
;         (new-link-number `(((:number . "\\d+|next") :optional)))
;         (new-link-number-short `(((:number . "\\d+|n") :optional :immediate)))
;         (goto-rxs `(("goto" ,@link-arguments)       ; /goto [forward][:<N>][*] [<substring>] | /goto back[:<N>][*] [<substring>]
;                     ("g" ,@short-link-arguments)))  ; /g[f][<N>][*] [<substring>] | /gb[<N>][*] [<substring>]
;         (search-rxs `("s(?:earch)?"                                                             ; /s[earch] [+tag,tag,...] [-tag,tag,...] <substring>
;                       ,@tag-arguments
;                       ,@substring-arguments))
;         (back-rxs `(("back" ,@exponent-arguments)
;                     ("b" ,@short-exponent-arguments)))
;         (forward-rxs `(("forward" ,@exponent-arguments)
;                        ("f" ,@short-exponent-arguments)))
;         (links-rxs `(("links" ,@link-arguments)
;                      ("l" ,@short-link-arguments)))
;         (add-note-rxs `(("add" "note" ,@new-link-number)
;                         ("an" ,@new-link-number-short)))
;         (add-link-rxs `(("add" "link" (:direction . "to|from") ,@new-link-number)
;                         ("al" ((:direction . "t|f") :immediate)
;                          ,@new-link-number-short)))
         )
;    (generate-commands
;      (create-shell-commands
;        '("home") #'command-home "Go to root note"
;        '(("edit") ("e")) #'command-edit "Edit current note"
;        '(("next") ("n")) #'command-next "Goto next note in sequence"
;        back-rxs #'command-back "Go back in history"
;        forward-rxs #'command-forward "Go forward in history"
;        search-rxs #'command-search-note "Search by substring across all zettelkasten"
;        goto-rxs #'command-goto "Go to specific note using links, starting from current one"
;        links-rxs #'command-links "Show links from this note with specified parameters"
;        add-note-rxs #'command-add-note "Adding note"
;        ))
        ;(format t "Acceptable? ~A~&" (every #'acceptable-term? '("add" "some" (:r . "\\w+"))))

        (define-subexpression shell :num (make-shell-expression '((:body . "\\d+"))
                                                                (lambda (&key ((:body body)))
                                                                  (parse-integer body))
                                                                "Integer number"))
        (define-command shell (make-shell-expression '("add" "some" (:item . "\\w+") (:count . :num))
                                                     (lambda (&key ((:item item)) ((:count count)))
                                                       (format t "Add some ~A, ~A items~&" item count))
                                                     "Adding-Some command"))
        (define-command shell (make-shell-expression '("home") #'command-home "Go to root note"))
        (generate-commands-from-shell shell)))

        ;                             '(("memorize") ("memo") ("m")) #'command-memorize "Memorize current note for future use"
        ;                             '(("clear" "memo(?:ry)?") ("cm")) #'command-clear-memory "Forget memorized note"
        ;                             '(("goto" "memo(?:ry)?") ("gm")) #'command-goto-memory "Goto memorized note"
        ;add-link-rxs #'command-add-link "Adding link to and from other note"



;;; SERVICE


(defun zettelkasten-init-hook ()
  (when (select :* (from :note) (where (:= :id 0)))
    (set-current-note 0)))
