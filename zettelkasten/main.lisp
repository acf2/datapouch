;;; main.lisp
;;; Zettelkasten example
;;;
;;; Don't forget, you *demented old man*! Every and any note interaction must
;;; interpret a note as ID number.
;;; Note lists are lists of IDs (lists of integers)
;;; This is the crucial component in maintaining consistency between
;;; application and database.
;;;
;;; All links should be represented by pairs (source . destination)
;;;
;;; When there is a fields argument in any function, it's an assoc list
;;; ((:field_symbol "Column name for output") (:field_symbol2) ...)
;;; Fields without column name for output will not be printed


(in-package :zettelkasten)


;;; === Snippets ===

(defun next-note-snippet (info &key &allow-other-keys)
  (make-result (getf info :name)
               `(caar (select :destination
                              (from :link)
                              (where (:and (:= :source *current-note*)
                                           (:= :number 0)))))))


;;; === Commands ===

(defun command-home ()
  `(when (select :* (from :note) (where (:= :id 0)))
     (set-current-note 0)))


;; edit for other types of notes? memorized? Just any note (with designation)?
(defun command-edit ()
  `(when *current-note*
     (edit-notes (where (:= :id *current-note*)))))


(defun command-show-notes (&key ((:note single-note-snippet)) ((:notes multiple-notes-snippet)))
  (cond (single-note-snippet
          (once-only ((note single-note-snippet))
                     `(if ,note
                        (show-note ,note)
                        (format *standard-output* "~A~&" +msg-no-notes+))))
        (multiple-notes-snippet
          (once-only ((notes multiple-notes-snippet))
                     `(let ((length (length ,notes)))
                        (cond ((= length 0)
                               (format *standard-output* "~A~&" +msg-no-notes+))
                              ((= length 1)
                               (show-note (first ,notes)))
                              (:else
                                (show-notes ,notes))))))))


(defun command-add-note (&key ((:link-number link-number))
                              ((:continue continue))
                              ((:override-note override-note) nil override-note?))
  `(let* ((conflicting-notes (and (not ,override-note?)
                                  ,link-number
                                  (car (select '(:source :destination)
                                               (from :link)
                                               (where (:and (:= :source *current-note*)
                                                            (:= :number ,link-number))))))))
     (if conflicting-notes
       (yes-or-no-dialog (lambda (result)
                           (command-add-note :link-number ,link-number
                                             :continue ,continue
                                             :override-note (and result
                                                                 conflicting-notes)))
                         :prompt-msg +question-note-with-number-exists+)
       (let* ((new-note-body (first (edit-strings ""))))
         (cond ((string= new-note-body "")
                (format *standard-output* "~A~&" +msg-abort-note-creation+))
               (:else
                 ,(when override-note
                    `(update :link
                             (set= :number nil)
                             (where (:and (:= :source (first ',override-note))
                                          (:= :destination (second ',override-note))))))
                 (let ((new-note (add-note new-note-body
                                           *current-note*
                                           (and (or (not ,override-note?)
                                                    ',override-note)
                                                ,link-number))))
                   (when ,continue
                     (set-current-note new-note))
                   new-note)))))))


(defun get-zettelkasten-yields ()
  (append 
    (let ((bc (make-instance 'behavior-container)))
      (add-space-patterns bc)
      (set-behaviors
        bc
        (:word (make-wildcard-pattern "\\w+"
                                      (list "abracadabra" "a")
                                      "word"
                                      "*")
               (lambda (info word)
                 (make-result (getf info :name)
                              word))
               :use-only-named-results nil)
        (:number (make-wildcard-pattern "[1-9]\\d*"
                                        (list "1" "90" "31337")
                                        "number"
                                        "N")
                 (lambda (info num)
                   (make-result (getf info :name)
                                (parse-integer num)))
                 :use-only-named-results nil)
        (:next-note (:trivial "next" "n")
                    #'next-note-snippet))
      (pattern-let
        bc ((note-selector (:* (:next-note :name :note))))
        (with-immutable-parsers
          (collect-yields
            bc
            ((:+ :begin
                 (:trivial "home")
                 :end)
             #'command-home
             "Go to root note.")
            ((:+ :begin
                 (:trivial "edit" "e")
                 :end)
             #'command-edit
             "Edit note.")
            ((:+ :begin
                 (:? (:trivial "show" "s")
                     :space)
                 note-selector
                 :end)
             #'command-show-notes
             "Show contents of one or more notes.")
            ((:+ :begin
                 (:trivial "add" "a")
                 :space
                 (:trivial "note" "n")
                 (:? :space
                     (:* (:number :name :link-number)
                         (:trivial "continue" "c"
                                   (return-keyword :continue))))
                 :end)
             #'command-add-note
             "Add new note.")
            ))
        )
      )))

;
;          ((:+ :begin
;               (:trivial "hello" "h")
;               :fixed-space
;               (:word :name :name)
;               :end)
;           (lambda (&key name)
;             `(format t "Greetings, ~:(~A~)~&" ,name))
;           "Greet your guest.")
;          ((:+ :begin
;               (:trivial "dice")
;               :end)
;           (lambda ()
;             (with-return
;               return-from-app
;               `(progn (defparameter *dice-roll* nil)
;                       ,(return-application bc
;                                            (((:+ :begin
;                                                  (:trivial "throw")
;                                                  :space
;                                                  (:number :name :dice)
;                                                  :end)
;                                              (lambda (&key dice)
;                                                (return-from-app
;                                                  (once-only ((d dice))
;                                                             `(progn
;                                                                (setf *dice-roll* (list (1+ (random ,d)) ,d))
;                                                                (format t "Result: ~A~&" (first *dice-roll*))))))
;                                              "Throw the fukken dice!"))
;                                            :prompt-fun (constantly "THROW-DICE $ ")))))
;           "Application for throwing dice.")
;          ((:+ :begin
;               (:trivial "previous" "p")
;               :space
;               (:trivial "roll" "r")
;               :end)
;           (lambda ()
;             `(format t "Previous dice roll result: ~A/~A~&" (first *dice-roll*) (second *dice-roll*)))
;           "Show previous dice roll.")




;;; SERVICE


(defun zettelkasten-init-hook ()
  (when (select :* (from :note) (where (:= :id 0)))
    (set-current-note 0)))
