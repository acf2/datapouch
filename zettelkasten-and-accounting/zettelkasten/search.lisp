;;;; search.lisp


(in-package :zac.box)


;;; Has an ability to peek note text
;;; Because search is a hard operation, and it kinda fits
;;; For "ordinary goto" user can just go back in history and choose different note to go to


;;; rows is a list of lists of format (note-id note-text)
(defun choose-note-with-peeking (rows)
  (let ((current-mode :choosing)
        (peeked-note nil))
    (cond ((= (length rows) 0) nil)
          ((= (length rows) 1) (first rows))
          (:else (dialog :query-fun (lambda (&optional (error-form nil error-form-supplied?))
                                      (declare (ignore error-form))
                                      (cond ((and (eq current-mode :choosing)
                                                  error-form-supplied?)
                                             (format *standard-output* "Please, try again.~&"))
                                            ((eq current-mode :peeking)
                                             (setf current-mode :choosing))
                                            (:else 
                                              (pretty-print-table (list "Number" "Text")
                                                                  (loop :for row :in (map 'list #'cdr rows)
                                                                        :for i :from 1 :to (length rows)
                                                                        :collect (cons i row)))
                                              (format *standard-output* "S[how] list again, [choose] note number or p[eek] it:~&")
                                              (setf current-mode :choosing))))
                         :input-handler (lambda (unfiltered-input)
                                          (let* ((rx (make-scanner (concat "\\s*" 
                                                                           (combine (make-named-group :option "s(?:how)?")
                                                                                    (concat (make-named-group :option "(?:c(?:hoose)?)?|p(?:eek)?")
                                                                                            "\\s*"
                                                                                            (make-named-group :number "\\d+")))
                                                                           "\\s*")))
                                                 (input-match (when unfiltered-input
                                                                (multiple-value-bind (matched match-list) (scan-named-groups rx unfiltered-input)
                                                                  (and matched match-list))))
                                                 (peeking? (is-group :option input-match "peek" "p"))
                                                 (show-again? (is-group :option input-match "show" "s"))
                                                 (note-number (or (and (get-group :number input-match)
                                                                       (parse-integer (get-group :number input-match)))
                                                                  (and (string= unfiltered-input "")
                                                                       peeked-note)))
                                                 (accepted (and (integerp note-number) (<= 1 note-number (length rows)))))
                                            (cond (show-again? (setf current-mode :show-again)
                                                               (values nil :show-again))
                                                  (peeking?  (setf current-mode :peeking)
                                                             (setf peeked-note note-number)
                                                             (show-note (first (nth (1- note-number) rows)))
                                                             (values nil :peeking))
                                                  (:else (values accepted (and accepted (nth (1- note-number) rows)))))))
                         :prompt-fun *choose-note-prompt*
                         :raw-input t)))))


(defun command-search-note (string match)
  (declare (ignore string))
  (let* ((substring (get-group :substring match))
         (found-notes (select '(:id :text)
                              (from :note)
                              (where (:instr :text substring))))
         (chosen-note (choose-note-with-peeking found-notes)))
    (cond ((null found-notes)
           (format *standard-output* "~A~&" +msg-no-notes+))
          ((null chosen-note)
           (format *standard-output* "~%~A~&" +msg-note-is-not-chosen+))
          (:else
            (set-current-note (first chosen-note))))))
