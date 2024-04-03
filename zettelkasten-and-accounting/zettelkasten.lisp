;;;; zettelkasten.lisp
;;; Zettelkasten example
;;;
;;; WARNING! Every and any note interaction must interpret a note as ID number
;;; Note lists are lists of IDs (lists of integers)
;;; This is the crucial component in maintaining consistency between application and database
;;;
;;; All links should be represented by pairs (source . destination)



(in-package :zac.box)


(defparameter *current-note* nil)

;; History
(defparameter *note-history* nil) ; TBD
(defparameter *note-future* nil) ; TBD


;;; TBD
;;; [x] Root node with fixed ID?
;;; [ ] Backlinks?
;;; [ ] Closure (kleene star) on links/backlinks?
;;; [ ] history
;;; [ ] sequential read (like in normal zettelkasten?)
;;; [ ] fetch random note
;;; [ ] Make implicit tags mechanic (alt - workspace defined by tags)
;;;     Or should I? Maybe it's not that bright of an idea

(defparameter *option-show-note-after-jump* t)

(defparameter *order-by-text* nil)


;;; Table creation
;;; note: id [int] PK, text [text] NN
;;; link: source (id) NN, destination (id) NN, number [int]
(defun create-zettelkasten ()
  (create-table (:note :if-not-exists t)
                ((:id :type 'integer
                      :primary-key t
                      :autoincrement t)
                 (:text :type 'text
                        :not-null t)))
  (create-table (:link :if-not-exists t)
                ((:source :type 'integer
                          :not-null t)
                 (:destination :type 'integer
                               :not-null t)
                 (:number :type 'integer))
                (unique-key '(:source :destination))
                (foreign-key :source
                             :references '(:note :id)
                             :on-delete :cascade
                             :on-update :cascade)
                (foreign-key :destination
                             :references '(:note :id)
                             :on-delete :cascade
                             :on-update :cascade))
  (unless (select :* (from :note) (where (:= :id 0)))
    (insert-into :note (set= :id 0 :text "A stub: Root node")))) ; add root node


(defparameter +msg-note-is-not-chosen+ "Note is not chosen.")


(defparameter +errmsg-cannot-find-id+ "INTERNAL: No note with such ID")


(defun show-note (note)
  (let ((answer (when note (get-note-by-id note))))
    (if answer
      (format *standard-output* "~A~&" (second (first answer)))
      (error +errmsg-cannot-find-id+))))


;;; Wrapper for setting a note
;;; Every note change should be going through this function
;;; Unless you what to add some low-level stuff
(defun set-current-note (note &key ((:update-history update-history) t))
  (setf *current-note* note)
  (when note
    (when update-history
      nil) ; TODO
    (when *option-show-note-after-jump*
      (show-note note))))


;;; Returns maximum ID in note table
(defmacro max-note-id ()
  `(caar (select :id (from :note) (order-by (:desc :id)) (limit 1))))


;;; Returns list of notes with one note (unified format for queries)
(defun get-note-by-id (id)
  (select '(:id :text) (from :note) (where (:= :id id))))


;;; Returns list of all notes without parents (no links with destination == note.id)
;;; There should be only one such a note in normal zettelkasten
;(defun get-orphans ()
;  (select '(:id :text)
;    (from :note)
;    (left-join :link :on (:= :link.destination :note.id))
;    (where (:is-null :link.source))))


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


;;; Find notes from db, using substring
;;; Subset of notes can be limited by choosing specific list of IDs
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


;;; Call editor to edit one or more notes
(defun edit-notes (notes)
  (declare (type list notes))
  (and notes
       (let* ((old-notes (select '(:id :text)
                                 (from :note)
                                 (where (:in :id notes))))
              (new-notes (map 'list (lambda (note new-text) (list (first note) new-text))
                              old-notes
                              (apply #'edit-strings (map 'list #'second old-notes)))))
         (update :note
                 (set= :text `(:case :id
                                     ,@(loop :for new-note :in new-notes
                                             :collect (cons :when new-note))
                                     (:else :text)))
                 (where (:in :id notes))))))


(defparameter +errmsg-no-note-found+ "No such note found.")


(defun search-note (substring)
  (let ((new-note (choose-note-interactive (reduce #'append (filter-notes-by-substring substring)))))
    (if new-note
      (set-current-note new-note)
      (format *standard-output* +errmsg-no-note-found+))))


(defparameter *choose-note-prompt* (constantly "note> "))
(defparameter *choose-link-prompt* (constantly "link> "))


;;; Interactive dialog to choose a note from list
(defun choose-note-interactive (notes)
  (declare (type list notes))
  (if (= (length notes) 1)
    (first notes)
    (let* ((found-notes (select '(:id :text) (from :note) (where (:in :id notes))))
           (chosen-note-index (and found-notes (find-row-dialog '("Text")
                                                                    (map 'list #'cdr found-notes)
                                                                    :get-index t
                                                                    :prompt-fun *choose-note-prompt*))))
      (when chosen-note-index (first (nth chosen-note-index found-notes))))))


;;; Go to link of some note and return destination note ID
;;; I wanted order-by-text to be an argument. Yes, I know about dynamic binding
(defun choose-link-interactive (links &key ((:show-number show-number) t) ((:order-by-text order-by-text) *order-by-text*))
  (declare (type list links))
  (if (= (length links) 1)
    (first links)
    (let* ((sorting-clause (when (or show-number order-by-text)
                             (d.sql:build :order-by
                                          (when show-number (list :asc :link.number))
                                          (when order-by-text (list :asc :note.text)))))
           (chosen-links (d.sql:build-and-query
                           :select
                           (list :link.source
                                 :link.destination
                                 (when show-number :link.number)
                                 :note.text)
                           (from :link)
                           (inner-join :note :on (:= :link.destination :note.id))
                           (where (:in (d.sql:column-tuple (list :link.source :link.destination)) links))
                           sorting-clause)))
      (cond ((null chosen-links) nil)
            ((= (length chosen-links) 1) (first chosen-links))
            (:else
              ;(format t "~A~&" linked-notes) ; DEBUG
              ;(format t "~A~&" (map 'list #'cdr linked-notes)) ; DEBUG
              (let ((chosen-link-index (find-row-dialog (zac.aux:list-existing (when show-number "Number")
                                                                               "Text")
                                                        (mapcar #'cddr chosen-links)
                                                        :get-index t
                                                        :prompt-fun *choose-link-prompt*)))
                (when chosen-link-index (subseq (nth chosen-link-index chosen-links) 0 2))))))))


;"SELECT * FROM link WHERE (a IN (?, ?))"
;(3 4)


;(defun jump-link (


;;; /note (/n) - open text editor for current note +
;;; goto != jump
;;;   /goto forward/back/any:N* [+tags] [-tags] <substring> (/gfN* +-, /gbN* +-, /gaN* +-) - interactive link choice from current note: possibly filtered by substring
;;;   /jump <N1> ... <Nm> (/j) - special command for non-interative link choice, can be chained to form "paths" of links
;;;                              Only forward. Somewhat service value for big Zettelkastens.
;;;                              Number - number of a link, if exists (or text inside note)
;;;                              Text - text inside note
;;;   /search [+tags] [-tags] <substring> (/s) - global goto substring (slow)
;;; /<linkcommand>[*] <N> - show links from this note in forward/backward/all directions
;;;                         number after the command shows how much times zt should travel
;;;                         asterisk shows if closure is needed (all levels deep notes from 1 to N will be shown)
;;;   /links (/l)
;;;   /backlinks (/bl)
;;;   /connections (/al)
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


(defparameter +msg-abort-note-creation+ "Aborted.~&")


(defun command-add-note (string match)
  (declare (ignore string match))
  (let ((new-note (first (edit-strings ""))))
    (if (string= new-note "")
      (format *standard-output* +msg-abort-note-creation+)
      (add-note new-note *current-note*))))


(defun command-note (string match)
  (declare (ignore string match))
  (when *current-note*
    (edit-notes (list *current-note*))))


(defun command-home (string match)
  (declare (ignore string match))
  (when (select :* (from :note) (where (:= :id 0)))
    (set-current-note 0)))


(defun command-goto (string match)
  (let* ((goto-type (if (and (get-group :type match)
                             (or (string= (get-group :type match) "back")
                                 (string= (get-group :type match) "b")))
                      'back
                      'forward))
         (exponent (if (get-group :exponent match)
                     (parse-integer (get-group :exponent match))
                     1))
         (closure (not (null (get-group :closure match))))
         (source (if (eq goto-type 'forward) :source :destination))
         (destination (if (eq goto-type 'forward) :destination :source)))
    ;; TODO
    ;; NOTE: exponent >= 1 (exponent is natural)
    (cond ((= exponent 1) (select '(:destination-note.id :destination-note.text)
                                  (from (:as :note :source-note))
                                  (inner-join :link :on (:= :source-note.id (zac.aux:make-name :table :link
                                                                                               :column source)))
                                  (inner-join (:as :note :destination-note) :on (zac.aux:make-name :table :link
                                                                                                   :column destination))
                                  (where (:= :source-note.id *current-note*))))
          ((not closure) nil)
          (:else nil))
    (format t "STRING: ~A~&MATCH: ~S~&TYPE: ~A~&EXPONENT: ~A~&CLOSURE: ~A~&" string match goto-type exponent closure)))


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
  (let ((goto-rxs `(("goto"                                                                    ; /goto (forward|back)?(:\\d+)?(\\*)?
                     ((:type . "forward|back") :optional)
                     (,(concat ":" (make-named-group :exponent "[1-9]\\d*")) :optional :immediate)
                     ((:closure . "\\*") :optional :immediate)
                     ((:substring . ".*") :optional))
                    ("g"
                     ((:type . "f|b") :optional :immediate)                                    ; /g(f|b)?(\\d+)?(\\*)?
                     ((:exponent . "[1-9]\\d*") :optional :immediate)
                     ((:closure . "\\*") :optional :immediate)
                     ((:substring . ".*") :optional)))))
    ;; Simple commands
    (zac.cmd:make-commands-from-wrappers
      (zac.cmd:generate-wrappers '(("add" "note") ("an")) #'command-add-note
                                 '(("note") ("n")) #'command-note
                                 '("home") #'command-home
                                 goto-rxs #'command-goto))))


;;; SERVICE


(defun zettelkasten-init-hook ()
  (when (select :* (from :note) (where (:= :id 0)))
    (set-current-note 0)))
