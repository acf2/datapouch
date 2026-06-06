;;; main.lisp
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


(in-package :zettelkasten)




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
                 :use-only-named-results nil))
      (with-immutable-parsers
        (let ((previous-dice-roll nil) (previous-dice nil))
          (collect-yields
            bc
            ((:+ :begin
                 (:trivial "hello" "h")
                 :fixed-space
                 (:word :name :name)
                 :end)
             (lambda (&key name)
               `(format t "Greetings, ~:(~A~)~&" ,name))
             "Greet your guest.")
            ((:+ :begin
                 (:trivial "dice")
                 :end)
             (lambda ()
               (compile-into-application bc
                                         (((:+ :begin
                                               (:trivial "throw")
                                               :space
                                               (:number :name :dice)
                                               :end)
                                           (lambda (&key dice)
                                             (with-gensyms
                                               (dice-name)
                                               `(let ((,dice-name ,dice))
                                                  (funcall (get-current-return) (list (1+ (random ,dice-name)) ,dice-name)))))
                                           "Throw the fukken dice!"))
                                         :result-callback (lambda (result)
                                                            (setf previous-dice-roll (first result))
                                                            (setf previous-dice (second result))
                                                            (format t "Result: ~A~&" (first result)))
                                         :prompt-fun (constantly "THROW-DICE $ ")))
             "Application for throwing dice.")
            ((:+ :begin
                 (:trivial "previous" "p")
                 :space
                 (:trivial "roll" "r")
                 :end)
             (lambda ()
               `(format t "Previous dice roll result: ~A/~A~&" ,previous-dice-roll ,previous-dice))
             "Show previous dice roll.")))))
    ;(lambda (command-string)
    ))




;;; SERVICE


(defun zettelkasten-init-hook ()
  (when (select :* (from :note) (where (:= :id 0)))
    (set-current-note 0)))
