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
          (macrobody ((note single-note-snippet))
                     (if note
                       (show-note note)
                       (format *standard-output* "~A~&" +msg-no-notes+))))
        (multiple-notes-snippet
          (macrobody ((notes multiple-notes-snippet))
                     (let ((length (length notes)))
                       (cond ((= length 0)
                              (format *standard-output* "~A~&" +msg-no-notes+))
                             ((= length 1)
                              (show-note (first notes)))
                             (:else
                               (show-notes notes))))))))


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
               (:* (:next-note :name :note))
               :end)
           #'command-show-notes
           "Show contents of one or more notes.")
          )))))

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
;             (compile-into-application bc
;                                       (((:+ :begin
;                                             (:trivial "throw")
;                                             :space
;                                             (:number :name :dice)
;                                             :end)
;                                         (lambda (&key dice)
;                                           (with-gensyms
;                                             (dice-name)
;                                             `(let ((,dice-name ,dice))
;                                                (funcall (get-current-return) (list (1+ (random ,dice-name)) ,dice-name)))))
;                                         "Throw the fukken dice!"))
;                                       :result-callback (lambda (result)
;                                                          (setf previous-dice-roll (first result))
;                                                          (setf previous-dice (second result))
;                                                          (format t "Result: ~A~&" (first result)))
;                                       :prompt-fun (constantly "THROW-DICE $ ")))
;           "Application for throwing dice.")
;          ((:+ :begin
;               (:trivial "previous" "p")
;               :space
;               (:trivial "roll" "r")
;               :end)
;           (lambda ()
;             `(format t "Previous dice roll result: ~A/~A~&" ,previous-dice-roll ,previous-dice))
;           "Show previous dice roll."))))))




;;; SERVICE


(defun zettelkasten-init-hook ()
  (when (select :* (from :note) (where (:= :id 0)))
    (set-current-note 0)))
