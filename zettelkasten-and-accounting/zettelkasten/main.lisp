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


;;; TBD v2
;;; Everything should be based around operator composition
;;; [x] /home
;;; [x] /next (note in sequence)
;;;    maybe next designation?
;;; [~] Select notes through expression
;;;   [x] One note
;;;     [x] From this one
;;;     [ ] Global search
;;;   [ ] Many notes
;;;     [ ] From this one
;;;     [ ] Global search
;;; [~] /goto
;;;   [x] basic
;;;   [ ] substring
;;;   [ ] tags
;;; [ ] Memorization
;;;   [ ] Command for memorizing
;;;   [ ] Expression to use memorized notes (target, memorized)


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


;;; SUBEXPRESSION HANDLERS


(defun handle-optional-number (&key ((:number number)))
  (if number
    (parse-integer number)
    1))


(defun handle-optional-direction (&key ((:type type)))
  (let* ((backward? (and type
                         (some (lambda (var)
                                 (string= type var))
                               (list "b" "back" "backward")))))
    (if backward? :backward :forward)))


(defun handle-dae (&key ((:raw-direction raw-direction)) ((:raw-number raw-number)) ((:closure closure)))
  (list :direction (handle-optional-direction :type raw-direction)
        :exponent (handle-optional-number :number raw-number)
        :closure closure
        :empty-expression? (and (null raw-direction)
                                (null raw-number)
                                (null closure))))


(defun handle-link-number (&key ((:number number)))
  (if (eq number :next)
    0
    (and number (parse-integer number))))


(defun select-constrained-notes (&key ((:parameters parameters)) ((:give-up-on-empty give-up-on-empty) nil))
  (list :parameters parameters
        :rows (when (or (not give-up-on-empty)
                        (not (getf parameters :empty-expression?)))
                (apply #'zac.box.travel:select-notes-through-links *current-note* parameters))))


(defun pick-next-note (&key &allow-other-keys)
  (let ((next-note-id (caar (select :destination
                                    (from :link)
                                    (where (:and (:= :source *current-note*)
                                                 (:= :number 0)))))))
    (if next-note-id
      next-note-id
      (format *standard-output* "~A~&" +msg-no-notes+))))


(defun pick-notes-from-dae (&key ((:selection selection))
                                 ((:choose-many choose-many) nil)
                                 ((:allow-peek allow-peek) nil)
                                 ((:default-on-empty default-on-empty) nil))
  (if (and default-on-empty
           (getf (getf selection :parameters) :empty-expression?)
           (null (getf selection :rows)))
    *current-note*
    (let* ((selected-rows (getf selection :rows))
           (chosen-rows (apply #'zac.box.travel:choose-row-from-note-through-links
                               selected-rows
                               *choose-note-prompt*
                               :choose-many choose-many
                               :allow-peek allow-peek
                               (getf selection :parameters))))
      (cond ((null selected-rows)
             (format *standard-output* "~A~&" +msg-no-notes+))
            ((null chosen-rows)
             (format *standard-output* "~%~A~&" +msg-note-is-not-chosen+))
            (:else
              (if choose-many
                (map 'list (lambda (row)
                             (getf row :id))
                     chosen-rows)
                (getf chosen-rows :id)))))))


(defun command-search-note (string match)
  (declare (ignore string))
  (let* ((substring (get-group :substring match))
         (found-notes (select '(:id :text)
                              (from :note)
                              (where (:instr :text substring))))
         (chosen-note (zac.box.travel:choose-row-from-note-with-peeking found-notes *choose-note-prompt*)))
    (cond ((null found-notes)
           (format *standard-output* "~A~&" +msg-no-notes+))
          ((null chosen-note)
           (format *standard-output* "~%~A~&" +msg-note-is-not-chosen+))
          (:else
            (set-current-note (first chosen-note))))))


;;; COMMAND HANDLERS

(defun command-home ()
  (when (select :* (from :note) (where (:= :id 0)))
    (set-current-note 0)))


(defun command-show-note (&key ((:note note-id)))
  (if note-id
    (show-note note-id)
    (format *standard-output* "~A~&" +msg-no-notes+)))


(defun command-goto (&key ((:note note-id)))
  (when note-id
    (set-current-note note-id)))


(defun command-links (&key ((:selection selection)))
  (if (getf selection :rows)
    (apply #'zac.box.travel:pretty-print-note-through-links
           (getf selection :rows)
           (getf selection :parameters))
    (format *standard-output* "~A~&" +msg-no-notes+)))


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


(defun command-add-note (&key ((:link-number link-number)) ((:continue continue)))
  (let* ((notes-with-number (and link-number
                                 (car (select '(:source :destination)
                                              (from :link)
                                              (where (:and (:= :source *current-note*)
                                                           (:= :number link-number))))))))
    (let* ((override-note-number? (and notes-with-number (yes-or-no-p +question-note-with-number-exists+)))
           (new-note-body (first (edit-strings ""))))
      (cond ((string= new-note-body "")
             (format *standard-output* "~A~&" +msg-abort-note-creation+))
            (:else
              (when override-note-number?
                (update :link
                        (set= :number nil)
                        (where (:and (:= :source (first notes-with-number))
                                     (:= :destination (second notes-with-number))))))
              (let ((new-note (add-note new-note-body
                                        *current-note*
                                        (if (and notes-with-number
                                                 (not override-note-number?))
                                          nil
                                          link-number))))
                (when continue
                  (set-current-note new-note))))))))




;;; OLD (FOR REWORK)

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


;;; +++++  COMMANDS  +++++
;;;   ===  Syntax  ===
;;;     Just /command
;;;     Each command has unique syntax, but they can be combined.
;;;
;;;   ===  Basics  ===
;;;     New command system works through combination of different commands.
;;;     For clarity, commands that select some  notes are called "designators",
;;;              and commands that act on these notes are called "operators".
;;;
;;;     They are all the same: some operators can use results of other operators.
;;;     But this distinction makes understanding easier for me.
;;;
;;;   ===  Designators  ===
;;;     cur[rent]
;;;       Selects currently chosen note.
;;;       Usually operators select current note by default, but it can be selected explicitly.
;;;
;;;     n[ext]
;;;       Select next note in stated sequence.
;;;       (Next note in sequence is explicitly labeled as "the next one" or with sign "->",
;;;        internally it is represented by link with number 0.)
;;;
;;;     DAE single-note
;;;
;;;   ===  Operations  ===
;;;
;;; TODO: Memory ops
;;; /set target (choose target from memorized list or current note; swaps, not forgets)
;;; /memorize (push new note onto memorized-notes, result: last target - new head of memo list, selected note becomes target)
;;; /rotate (first -> to end, second -> first, ...)


(defparameter +dae-help+
  "[DAE] Directed Advanced Exponent commands:
  These support repeated application, as well as change of direction and
  possible closure \"Closure\" is the Kleene star: union of all repeated
  applications in 1..N at once.")


(defun add-zettelkasten-commands (shell)
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

        (add-shell-subexpressions
          shell
          ;          ;;; Raw regexes - used to circumvent flaws of subexpressions
          (:raw-optional-direction ((((:type . "forward|back(?:ward)?") :optional))
                                    (((:type . "f|b") :optional :optionally-immediate)))
                                   (lambda (&key ((:type type)))
                                     type)
                                   "Raw value of optional direction regex.")
          (:raw-optional-number ((((:raw-number . "[1-9]\\d*") :optional :optionally-immediate)))
                                  (lambda (&key ((:raw-number raw-number)))
                                    raw-number)
                                  "Raw value of optional exponent regex.")
          ;;; Expressions - for parsing command arguments
          (:optional-direction (((:type . :raw-optional-direction)))
                               #'handle-optional-direction
                               "Optional parameter for operation direction.")
          (:optional-number (((:number . :raw-optional-number)))
                              #'handle-optional-number
                              "Optional parameter for number")
          (:optional-closure ((((:closure . "\\*") :optional :optionally-immediate)))
                             (lambda (&key ((:closure closure)))
                               (not (null closure)))
                             "Optional parameter to enable concatenation of results of repeated calls to operation (Kleene star subset).")
          (:next-note ((((:next . "n") :optionally-immediate))
                       ((:next . "next")))
                      (lambda (&key &allow-other-keys)
                        :next)
                      "Modifier for next note in sequence")
          (:dae (((:raw-direction . :raw-optional-direction)
                  (:raw-number . :raw-optional-number)
                  (:closure . :optional-closure)))
                #'handle-dae
                +dae-help+)
          (:optional-link-number (((:number . :raw-optional-number))
                                  ((:number . :next-note)))
                                 #'handle-link-number
                                 "Number mark for link")
          (:optional-continue ((((:continue . "continue") :optional))
                               (((:continue . "c") :optional :optionally-immediate)))
                              (lambda (&key ((:continue continue)))
                                (when continue
                                  :continue))
                              "Continuing modifier")
          ;;; Selectors - literally performing some form of select statement
          ;;;             return full tables, without filtering
          (:constrained-note-selector (((:parameters . :dae)))
                                      #'select-constrained-notes
                                      "For note selection, constrained by current note.")
          (:explicit-constrained-note-selector (((:parameters . :dae)))
                                               (lambda (&key ((:parameters parameters)))
                                                 (select-constrained-notes :parameters parameters
                                                                           :give-up-on-empty t))
                                               "For note selection, constrained by current note. Must be explicitly stated.")
          ;;; Designators - allow to designate singular notes and note subsets,
          ;;;               direct various note operators to notes on which they should be performed
          (:current-note-designator ((((:current . "cur(?:rent)?") :optionally-immediate)))
                                    (lambda (&key &allow-other-keys)
                                      *current-note*)
                                    "Currently chosen note.")
          (:next-note-designator (((:next . :next-note)))
                                 #'pick-next-note
                                 "Next note in sequence.")
          (:single-constrained-note-designator (((:selection . :constrained-note-selector)))
                                               #'pick-notes-from-dae
                                               "Pick one note over a constrained selection.")
          (:single-constraint-note-designator-with-default (((:selection . :constrained-note-selector)))
                                                           (lambda (&key ((:selection selection)))
                                                             (pick-notes-from-dae :selection selection
                                                                                  :default-on-empty t))
                                                           "Pick one note over a constrained selection. Defaults to current note.")
          (:multiple-constrained-note-designator-with-peeking (((:selection . :constrained-note-selector)))
                                                              (lambda (&key ((:selection selection)))
                                                                (pick-notes-from-dae :selection selection
                                                                                     :allow-peek t
                                                                                     :choose-many t))
                                                              "Pick one or more notes over a constrained selection using a safer method.")
          (:single-note-designator (((:note . :current-note-designator))
                                    ((:note . :next-note-designator))
                                    ((:note . :single-constrained-note-designator)))
                                   (lambda (&key ((:note note)))
                                     note)
                                   "Aggregate single note designator")
          )

        (add-shell-commands
          shell
          ((("home")) #'command-home "Go to root note")
          ((((:note . :single-note-designator)))
           #'command-show-note "Show contents of the note.")
          ((("ttpeek" (:notes . :multiple-constrained-note-designator-with-peeking)))
           (lambda (&key ((:notes notes)))
             (show-notes notes))
           "Test peeking")
          ((("g(?:oto)?" (:note . :single-note-designator)))
           #'command-goto "Go to some note from this one")
          ((("l(?:inks)?" (:selection . :constrained-note-selector)))
           #'command-links "Show links from this note with specified parameters")
          ((("add" "note" (:link-number . :optional-link-number) (:continue . :optional-continue))
            ("an" (:link-number . :optional-link-number) (:continue . :optional-continue)))
           #'command-add-note "Add new note")
          )
        )


;        (define-subexpression shell :exponent (make-shell-expression '(((:exponent . "[1-9]\\d*") :optional))
;                                                                     (lambda (&key ((:exponent exponent)))
;                                                                       (if (get-group :exponent match)
;                                                                         (parse-integer (get-group :exponent match))
;                                                                         1))
;                                                                     "Number of times some operation would be applied"))
;        (define-subexpression shell :direction (make-shell-expression '(((:type . "forward|back") :optional))
;                                                                      (lambda (&key ((:type type)))
;                                                                        (let* ((backward? (and type
;                                                                                               (or (string= type "back")))))
;                                                                          (if backward? :back :forward)))
;                                                                      "Direction for operations supporting it"))
;        (define-subexpression shell :closure (make-shell-expression '(((:closure . "\\*") :optional :immediate))
;                                                                    (lambda (&key ((:closure closure)))
;                                                                      (not (null closure)))
;                                                                    "Should repeated call to operation be concatenated? (Kleene star)"))

;         (exponent-arguments )
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

        ;                             '(("memorize") ("memo") ("m")) #'command-memorize "Memorize current note for future use"
        ;                             '(("clear" "memo(?:ry)?") ("cm")) #'command-clear-memory "Forget memorized note"
        ;                             '(("goto" "memo(?:ry)?") ("gm")) #'command-goto-memory "Goto memorized note"
        ;add-link-rxs #'command-add-link "Adding link to and from other note"



;;; SERVICE


(defun zettelkasten-init-hook ()
  (when (select :* (from :note) (where (:= :id 0)))
    (set-current-note 0)))
