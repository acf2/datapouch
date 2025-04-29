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


;; Should find all notes, that are unreachable from root
(defun find-lost-notes ()
  (loop :with all-notes := (reduce #'append (select '(:id) (from :note)))
        :with all-arcs := (select '(:source :destination) (from :link))
        :for queue := (list 0) :then (cdr queue)
        :for current := (car queue)
        :unless current :return all-notes
        :when (member current all-notes)
        :do (setf all-notes (delete current all-notes))
        (setf queue (append queue (map 'list #'second (remove-if-not (lambda (x) (eq current (first x))) all-arcs))))))


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
(defun remove-notes (notes)
  (when notes
    (delete-from :note (where (:in :id notes)))
    (setf *note-history* (remove-if (member-of notes) *note-history*))
    (when (member *current-note* notes :test #'eql)
      (set-current-note (first *note-history*)
                        :update-history nil))))


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


(defun handle-optional-number (&key ((:number number)) ((:default default)))
  (if number
    (parse-integer number)
    default))


(defun handle-optional-direction (&key ((:type type)))
  (let* ((backward? (member type
                            (list "b" "back" "backward")
                            :test #'string-equal)))
    (if backward? :backward :forward)))


(defun handle-dae (&key ((:direction direction)) ((:number number)) ((:closure closure)))
  (list :direction direction
        :exponent (or number 1)
        :closure closure
        :empty-expression? (and (null direction)
                                (null number)
                                (null closure))))


(defun handle-link-number (&key ((:number number)))
  (if (eq number :next)
    0
    (and number (parse-integer number))))


(defun handle-link-direction (&key ((:direction direction)))
  (let* ((backward? (member direction
                            (list "f" "from") ; opposite is "t" or "to"
                            :test #'string=)))
    (if backward? :backward :forward)))


(defun select-constrained-notes (&key ((:parameters parameters)) ((:give-up-on-empty give-up-on-empty) nil))
  (list :parameters parameters
        :rows (when (or (not give-up-on-empty)
                        (not (getf parameters :empty-expression?)))
                (apply #'zac.box.travel:select-notes-through-links *current-note* parameters))))


(defun select-global-notes (&key ((:substring substring)))
  (list :parameters (list :substring substring)
        :rows (select (get-field-names zac.box.db:+table-note-fields+)
                      (from :note)
                      (where (:instr (:upper :text) (:upper substring))))))


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
    (if choose-many
      (list *current-note*)
      *current-note*)
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


(defun pick-notes-from-global (&key ((:selection selection))
                                    ((:choose-many choose-many) nil))
  (let* ((selected-rows (getf selection :rows))
         (chosen-rows (find-row-with-peeking-dialog (get-field-dialog-texts zac.box.db:+table-note-fields+)
                                                    (map 'list (get-field-mapping-for-rows zac.box.db:+table-note-fields+) selected-rows)
                                                    :get-index t
                                                    :choose-many choose-many
                                                    :prompt-fun *choose-note-prompt*
                                                    :peek-row-function (lambda (row last?)
                                                                         (format *standard-output* "~A~&~@[~%~]" (first row) (not last?))))))
    (cond ((null selected-rows)
           (format *standard-output* "~A~&" +msg-no-notes+))
          ((null chosen-rows)
           (format *standard-output* "~%~A~&" +msg-note-is-not-chosen+))
          (:else
            (if choose-many
              (map 'list (lambda (index)
                           (first (nth index selected-rows)))
                   chosen-rows)
              (first (nth chosen-rows selected-rows)))))))


(defun pick-single-memorized-note (&key ((:number number)))
  (cond (number (nth (1- number) *memorized-notes*))
        (*memorized-notes*
          (let* ((found-rows (get-notes-by-id *memorized-notes*))
                 (chosen-row-index (and found-rows (find-row-dialog (get-field-dialog-texts zac.box.db:+table-note-fields+)
                                                                    (map 'list (get-field-mapping-for-rows zac.box.db:+table-note-fields+) found-rows)
                                                                    :get-index t
                                                                    :prompt-fun *choose-memorized-note-prompt*))))
            (nth chosen-row-index *memorized-notes*)))))


(defun pick-memorized-notes (&key ((:numbers numbers)) ((:all all)))
  (cond (all *memorized-notes*)
        (numbers (loop :for memorized-note :in *memorized-notes*
                       :for i :from 1 :to (length *memorized-notes*)
                       :when (member i numbers)
                       :collect memorized-note))
        (*memorized-notes*
          (let* ((found-rows (get-notes-by-id *memorized-notes*))
                 (chosen-row-indices (and found-rows (find-row-dialog (get-field-dialog-texts zac.box.db:+table-note-fields+)
                                                                      (map 'list (get-field-mapping-for-rows zac.box.db:+table-note-fields+) found-rows)
                                                                      :get-index t
                                                                      :choose-many t
                                                                      :prompt-fun *choose-memorized-note-prompt*))))
            (loop :for i :in chosen-row-indices
                  :collect (nth i *memorized-notes*))))))


;;; COMMAND HANDLERS

(defun command-home ()
  (when (select :* (from :note) (where (:= :id 0)))
    (set-current-note 0)))


(defun command-show-notes (&key ((:note note-id)) ((:notes notes-id)))
  (cond (note-id (show-note note-id))
        ((and notes-id (= (length notes-id) 1))
         (show-note (first notes-id)))
        (notes-id (show-notes notes-id))
        (:else
          (format *standard-output* "~A~&" +msg-no-notes+))))


(defun command-goto (&key ((:note note-id)))
  (when note-id
    (set-current-note note-id)))


(defun command-links (&key ((:selection selection)))
  (if (getf selection :rows)
    (apply #'zac.box.travel:pretty-print-note-through-links
           (getf selection :rows)
           (getf selection :parameters))
    (format *standard-output* "~A~&" +msg-no-notes+)))


(defun command-memorize (&key ((:notes notes)))
  (when notes
    (setf *memorized-notes* (remove-duplicates (append *memorized-notes* notes)
                                               :from-end t))))

(defun command-clear-memory (&key ((:memorized-notes notes-to-remove)))
  (setf *memorized-notes* (delete-if (member-of notes-to-remove) *memorized-notes*)))


(defun command-add-note (&key ((:link-number link-number)) ((:continue continue)))
  (let* ((notes-with-number (and link-number
                                 (car (select '(:source :destination)
                                              (from :link)
                                              (where (:and (:= :source *current-note*)
                                                           (:= :number link-number))))))))
    (let* ((override-note-number? (and notes-with-number (yes-or-no-dialog :prompt-msg +question-note-with-number-exists+)))
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



(defun command-remove-notes (&key ((:notes notes)))
  (if (null notes)
    (format *standard-output* "~A~&" +msg-abort-note-deletion+)
    (remove-notes notes)))


(defun command-collect-lost ()
  (let* ((lost-notes (find-lost-notes))
         (new-links (map 'list
                         (lambda (note)
                           (list *current-note* note))
                         lost-notes)))
    (insert-into :link
                 (:source :destination)
                 new-links)))


(defun command-add-link (&key ((:link-direction link-direction))
                              ((:link-number link-number))
                              ((:note note))
                              ((:notes notes)))
  (cond (note ; only one note is given, number could be provided too
          ;(format t "D: ~S, NUMBER: ~A, NOTE: ~S~&" link-direction link-number note)
          (let* ((source-note (if (eq link-direction :forward)
                                *current-note*
                                note))
                 (destination-note (if (eq link-direction :forward)
                                     note
                                     *current-note*))
                 (existing-link (first (select '(:source :destination :number)
                                               (from :link)
                                               (where (:and (:= :source source-note)
                                                            (:= :destination destination-note))))))
                 (link-with-this-number (and link-number (first (select '(:source :destination :number)
                                                                        (from :link)
                                                                        (where (:= :number link-number)))))))
            (cond (existing-link (format *standard-output* +msg-link-exists+ (third existing-link)))
                  (link-with-this-number (format *standard-output* +msg-link-with-number-exists+))
                  (:else
                    (insert-into :link
                                 '(:source :destination :number)
                                 (list source-note destination-note (or link-number :null)))))))
        (notes ; many notes 
          ;(format t "D: ~S, NOTES: ~S~&" link-direction notes)
          (let* ((source-notes (if (eq link-direction :forward)
                                 (list *current-note*)
                                 notes))
                 (destination-notes (if (eq link-direction :forward)
                                      notes
                                      (list *current-note*))))
            (insert-into :link (:source :destination)
                         (loop :for source :in source-notes
                               :append (loop :for destination :in destination-notes
                                             :collect (list source destination)))
                         (on-conflict-do-nothing '(:source :destination)))))
        (:else ; error, should not be possible
          (format *standard-output* +intermsg-no-notes-given+))))


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


;(defun old-command-add-link (string match)
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


(defun get-zettelkasten-commands ()
  (let ((zk-lex (make-instance 'lexicon)))
    (set-expressions
      zk-lex
      `(:name "\\w+" ,(lambda (name) (list :name name)) :use-nongroup-arguments t))
    (make-commands
      zk-lex
      `(,(concat "^\\s*" "[Hh]ello" "\\s+" (regex-group (get-expression zk-lex :name)) "\\s*$")
         ,(lambda (&key name)
            (format t "Greetings, ~:(~A~)~&" name))))))
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

;        (let ((nonzero-number-rx "[1-9]\\d*"))

;          (add-shell-subexpressions
;            shell
;            ;;; Raw regexes - used to circumvent flaws of subexpressions
;            (:raw-optional-number '((((:raw-number . "[1-9]\\d*") :optional :optionally-immediate)))
;                                  (lambda (&key ((:raw-number raw-number)))
;                                    raw-number)
;                                  "Raw value of optional exponent regex.")
;            (:substring '(((:string . ".*?")))
;                        (lambda (&key ((:string str)))
;                          (string-trim (list #\Newline #\Space #\Tab) str))
;                        "Substring parameters")
;
;            ;;; Expressions - for parsing command arguments
;            (:optional-direction '((((:type . "forward|back(?:ward)?") :optional))
;                                   (((:type . "f|b") :optional :optionally-immediate)))
;                                 #'handle-optional-direction
;                                 "Optional operation direction parameter.")
;            (:number `((((:number . ,nonzero-number-rx) :optionally-immediate)))
;                     #'handle-optional-number
;                     "Required numerical parameter.")
;            (:optional-number `((((:number . ,nonzero-number-rx) :optional :optionally-immediate)))
;                              #'handle-optional-number
;                              "Optional numerical parameter.")
;            (:optional-closure '((((:closure . "\\*") :optional :optionally-immediate)))
;                               (lambda (&key ((:closure closure)))
;                                 (not (null closure)))
;                               "Optional closure parameter. Enables result concatenation of repeated calls to operation (Kleene star subset).")
;            (:number-list '((((:numbers . "[1-9]\\d*(?:\\D+[1-9]\\d*)*") :optionally-immediate)))
;                          (lambda (&key ((:numbers numbers)))
;                            (map 'list #'parse-integer (ppcre:split "\\D+" numbers)))
;                          "Number list, with at least one number.")
;            (:next-note '((((:next . "n") :optionally-immediate))
;                          ((:next . "next")))
;                        (lambda (&key &allow-other-keys)
;                          :next)
;                        "Modifier for next note in sequence")
;            (:dae '(((:direction . :optional-direction)
;                     (:number . :optional-number)
;                     (:closure . :optional-closure)))
;                  #'handle-dae
;                  +dae-help+)
;            (:optional-link-direction '((((:direction . "from|to") :optional))
;                                        (((:direction . "f|t") :optional :optionally-immediate)))
;                                      #'handle-link-direction
;                                      "Direction for link")
;            (:optional-continue '((((:continue . "continue") :optional))
;                                  (((:continue . "c") :optional :optionally-immediate)))
;                                (lambda (&key ((:continue continue)))
;                                  (when continue
;                                    :continue))
;                                "Continuing modifier")
;
;            ;;; Selectors - literally performing some form of select statement
;            ;;;             return full tables, without filtering
;            (:constrained-note-selector '(((:parameters . :dae)))
;                                        #'select-constrained-notes
;                                        "For note selection, constrained by current note.")
;            (:explicit-constrained-note-selector '(((:parameters . :dae)))
;                                                 (lambda (&key ((:parameters parameters)))
;                                                   (select-constrained-notes :parameters parameters
;                                                                             :give-up-on-empty t))
;                                                 "For note selection, constrained by current note. Must be explicitly stated.")
;            (:global-note-selector '(("global" (:substring . :substring))
;                                     (("g" :optionally-immediate) (:substring . :substring)))
;                                   #'select-global-notes
;                                   "For global note selection. Case insensitive.")
;
;            ;;; Designators - allow to designate singular notes and note subsets,
;            ;;;               direct various note operators to notes on which they should be performed
;            (:current-note-designator '((((:current . "cur(?:rent)?") :optionally-immediate)))
;                                      (lambda (&key &allow-other-keys)
;                                        *current-note*)
;                                      "Currently chosen note.")
;            (:next-note-designator '(((:next . :next-note)))
;                                   #'pick-next-note
;                                   "Next note in sequence.")
;            (:single-constrained-note-designator '(((:selection . :constrained-note-selector)))
;                                                 #'pick-notes-from-dae
;                                                 "Pick one note over a constrained selection.")
;            (:single-constrained-note-designator-with-default '(((:selection . :explicit-constrained-note-selector)))
;                                                              (lambda (&key ((:selection selection)))
;                                                                (pick-notes-from-dae :selection selection
;                                                                                     :default-on-empty t))
;                                                              "Pick one note over a constrained selection. Defaults to current note.")
;            (:explicit-multiple-constrained-note-designator '(((:selection . :explicit-constrained-note-selector)))
;                                                            (lambda (&key ((:selection selection)))
;                                                              (pick-notes-from-dae :selection selection
;                                                                                   :choose-many t))
;                                                            "Pick one or more notes over a constrained selection.")
;            (:multiple-constrained-note-designator-with-default '(((:selection . :explicit-constrained-note-selector)))
;                                                                (lambda (&key ((:selection selection)))
;                                                                  (pick-notes-from-dae :selection selection
;                                                                                       :default-on-empty t
;                                                                                       :choose-many t))
;                                                                "Pick one or more notes over a constrained selection. Defaults to current note only.")
;            (:single-constrained-note-designator-with-peeking '(((:selection . :constrained-note-selector)))
;                                                              (lambda (&key ((:selection selection)))
;                                                                (pick-notes-from-dae :selection selection
;                                                                                     :allow-peek t))
;                                                              "Pick one note over a constrained selection using a safer method.")
;            (:multiple-constrained-note-designator-with-peeking '(((:selection . :constrained-note-selector)))
;                                                                (lambda (&key ((:selection selection)))
;                                                                  (pick-notes-from-dae :selection selection
;                                                                                       :allow-peek t
;                                                                                       :choose-many t))
;                                                                "Pick one or more notes over a constrained selection using a safer method.")
;            (:single-global-note-designator-with-peeking '(((:selection . :global-note-selector)))
;                                                         #'pick-notes-from-global
;                                                         "Pick exactly one note, globally filtering by substring.")
;            (:multiple-global-note-designator-with-peeking '(((:selection . :global-note-selector)))
;                                                           (lambda (&key ((:selection selection)))
;                                                             (pick-notes-from-global :selection selection
;                                                                                     :choose-many t))
;                                                           "Pick one or more notes globally filtering by substring.")
;            (:single-memory-designator '((("m" :optionally-immediate) (:number . :optional-number))
;                                         ("memo(?:ry)?" (:number . :optional-number)))
;                                       #'pick-single-memorized-note
;                                       "Memorized notes designator for single note.")
;            (:multiple-memory-designator '((("m" :optionally-immediate))
;                                           (("m" :optionally-immediate) (:numbers . :number-list))
;                                           (("m" :optionally-immediate) ((:all . "a(?:ll)?") :optionally-immediate))
;                                           ("memo(?:ry)?")
;                                           ("memo(?:ry)?" (:numbers . :number-list))
;                                           ("memo(?:ry)?" ((:all . "a(?:ll)?") :optionally-immediate)))
;                                         #'pick-memorized-notes
;                                         "Memorized notes designator.")
;            (:single-note-designator '(((:note . :current-note-designator))
;                                       ((:note . :next-note-designator))
;                                       ((:note . :single-constrained-note-designator))
;                                       ((:note . :single-global-note-designator-with-peeking))
;                                       ((:note . :single-memory-designator)))
;                                     (lambda (&key ((:note note)))
;                                       note)
;                                     "Aggregate single note designator")
;            (:multiple-note-designator '(((:next-note . :next-note-designator))
;                                         ((:notes . :explicit-multiple-constrained-note-designator))
;                                         ((:notes . :multiple-global-note-designator-with-peeking))
;                                         ((:notes . :multiple-memory-designator)))
;                                       (lambda (&key ((:notes notes)) ((:next-note next-note)))
;                                         (if next-note
;                                           (list next-note)
;                                           notes))
;                                       "Aggregate multiple note designator for broad operations.")
;            (:multiple-note-designator-with-default '(((:next-note . :next-note-designator))
;                                                      ((:notes . :multiple-constrained-note-designator-with-default))
;                                                      ((:notes . :multiple-global-note-designator-with-peeking))
;                                                      ((:notes . :multiple-memory-designator)))
;                                                    (lambda (&key ((:notes notes)) ((:next-note next-note)))
;                                                      (if next-note
;                                                        (list next-note)
;                                                        notes))
;                                                    "Aggregate multiple note designator for broad operations. Defaults to current note.")
;            )
;
;          ;;; NOTE: current:target+memorized...
;
;          (add-shell-commands
;            shell
;            ('(("home"))
;             #'command-home
;             '("Go to root note" "home"))
;
;            ('(((:note . :current-note-designator))
;               ((:note . :next-note-designator))
;               ((:notes . :multiple-memory-designator)))
;             #'command-show-notes
;             "<note>: Implicitly show contents of the note.")
;
;            ('(("s(?:how)?" (:note . :current-note-designator))
;               ("s(?:how)?" (:note . :next-note-designator))
;               ("s(?:how)?" (:note . :single-memory-designator)))
;             #'command-show-notes
;             '("Show contents of the note." "show" "s"))
;
;            ('(("memorize" (:notes . :multiple-note-designator-with-default))
;               ("mz" (:notes . :multiple-note-designator-with-default)))
;             #'command-memorize
;             '("Memorize notes for cumbersome operations." "memorize" "mz"))
;
;            ('(("c(?:lear)?" (:memorized-notes . :multiple-memory-designator)))
;             #'command-clear-memory
;             '("Clear memorized notes." "clear" "c"))
;
;            ('(("g(?:oto)?" (:note . :single-note-designator)))
;             #'command-goto
;             '("Go to some note from this one." "goto" "g"))
;
;            ('(("l(?:inks)?" (:selection . :constrained-note-selector)))
;             #'command-links
;             '("Show links from this note with specified parameters." "links" "l"))
;
;            ('(("add" "note" (:link-number . :optional-number) (:continue . :optional-continue))
;               ("an" (:link-number . :optional-number) (:continue . :optional-continue)))
;             #'command-add-note
;             '("Add new note." "add note" "an"))
;
;            ('(("remove" "notes" (:notes . :multiple-constrained-note-designator-with-peeking)))
;             #'command-remove-notes
;             '("Remove notes (cannot be reverted)." "remove notes"))
;
;            ('(("collect" "lost"))
;             #'command-collect-lost
;             '("Add links from this note to all lost (unreachable) notes." "collect lost"))
;
;            ('(("add" "link" (:link-number . :number) (:link-direction . :optional-link-direction) (:note . :single-note-designator))
;               ("add" "links?" (:link-direction . :optional-link-direction) (:notes . :multiple-note-designator))
;               ("al" (:link-number . :number) (:link-direction . :optional-link-direction) (:note . :single-note-designator))
;               ("al" (:link-direction . :optional-link-direction) (:notes . :multiple-note-designator)))
;             #'command-add-link
;             '("Some test run" "add link" "add links" "al"))
;            ))

;             (lambda (&key ((:link-direction link-direction)) ((:link-number link-number)) ((:note note)) ((:notes notes)))
;               (cond (note
;                       (format t "D: ~S, NUMBER: ~A, NOTE: ~S~&" link-direction link-number note)
;                       (list-notes (list note)))
;                     (:else
;                       (format t "D: ~S, NOTES: ~S~&" link-direction notes)
;                       (list-notes notes))))


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
