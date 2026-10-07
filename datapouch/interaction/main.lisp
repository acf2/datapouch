;;;; interaction/main.lisp


(in-package :datapouch.interaction)


;;; Awful thing
;;; Shows how much of the actual complexity is hidden, when patterns are used
;;; routinely, without explicit definitions.
(d.c.aux:with-immutable-parsers
  (defun yes-or-no-dialog (continuation
                            &key
                            ((:prompt-msg prompt-msg) nil)
                            ((:yes-choice affirmative) "yes")
                            ((:no-choice negative) "no")
                            ((:error-msg error-msg) (format nil "Please type ~S for yes or ~S for no."
                                                            (concatenate 'string (string d.rmacro:*control-character*)
                                                                         affirmative)
                                                            (concatenate 'string (string d.rmacro:*control-character*)
                                                                         negative)))
                            ((:prompt-format prompt-format) "~@[~A~&~]~@[~A ~](~(~A~) or ~(~A~)) "))
    (d.app:with-return
      return-from-app
      (let* ((common-prefix-length (length (d.aux:common-string-prefix (list affirmative negative))))
             (short-affirmative (subseq affirmative 0 (1+ common-prefix-length)))
             (short-negative (subseq negative 0 (1+ common-prefix-length)))
             (bc (d.ptrn:make-preset-behavior-container)))
        (d.ptrn:set-behaviors
          bc
          (:yes (d.ptrn::make-pattern (d.regex:sampled-regex-from-string (concatenate 'string "(?i)" affirmative)
                                                                         (d.regex:make-anycase-samples affirmative))
                                      (d.regex:sampled-regex-from-string (concatenate 'string "(?i)" short-affirmative)
                                                                         (d.regex:make-anycase-samples short-affirmative))
                                      (list nil)
                                      affirmative
                                      short-affirmative)
                (lambda (&rest rest)
                  (declare (ignore rest))
                  t)
                :use-only-named-results nil
                :short-expander (lambda (info arg)
                                  (declare (ignore info arg))
                                  affirmative))
          (:no (d.ptrn::make-pattern (d.regex:sampled-regex-from-string (concatenate 'string "(?i)" negative)
                                                                        (d.regex:make-anycase-samples negative))
                                     (d.regex:sampled-regex-from-string (concatenate 'string "(?i)" short-negative)
                                                                        (d.regex:make-anycase-samples short-negative))
                                     (list nil)
                                     negative
                                     short-negative)
               (lambda (&rest rest)
                 (declare (ignore rest))
                 nil)
               :use-only-named-results nil
               :short-expander (lambda (info arg)
                                 (declare (ignore info arg))
                                 negative)))
        (d.ptrn:compile-into-application
          bc
          (((:+ :begin
                (:* :yes :no)
                :end)
            (lambda (result)
              (return-from-app
                (funcall continuation result)))
            "Available answers"
            :use-only-named-results nil))
          :prompt-fun (let ((first-call t))
                        (lambda (buffer)
                          (declare (ignore buffer))
                          (let ((result (format nil prompt-format
                                                (when (not first-call)
                                                  error-msg)
                                                prompt-msg
                                                affirmative
                                                negative)))
                            (setf first-call nil)
                            result))))))))


(defun old-find-row-dialog (column-names rows &key ((:choose-many choose-many) nil)
                                               ((:prompt-msg prompt-msg) (if choose-many "Enter row numbers:~&" "Enter row number:~&"))
                                               ((:error-msg error-msg) "Please, try again.~&")
                                               ((:id-column-name-map id-map) (lambda (column-names)
                                                                               (cons "Row number" column-names)))
                                               ((:row-mapping-function row-map) (lambda (row-number row)
                                                                                  (cons row-number row)))
                                               ((:row-transformation-function row-transform) #'identity)
                                               ((:get-index get-index) nil)
                                               ((:prompt-fun prompt-fun) *prompt-fun*)
                                               ((:pretty-print-table-function pretty-print-table) #'pretty-print-table)
                                               &allow-other-keys)
  (cond ((= (length rows) 0) nil)
        ((= (length rows) 1)
         (cond ((and (not choose-many) get-index) 0)
               ((and choose-many get-index) (list 0))
               ((and (not choose-many) (not get-index)) (first rows))
               ((and choose-many (not get-index)) rows)))
        (:else (dialog :query-fun (lambda (&optional (error-form nil error-form-supplied?))
                                    (declare (ignore error-form))
                                    (if error-form-supplied?
                                      (format *standard-output* error-msg)
                                      (progn
                                        (funcall pretty-print-table
                                                 (funcall id-map column-names)
                                                 (funcall row-transform (loop :for row :in rows
                                                                              :for i :from 1 :to (length rows)
                                                                              :collect (funcall row-map i row))))
                                        (format *standard-output* prompt-msg))))
                       :input-handler (lambda (unfiltered-input)
                                        (labels ((filter-input (input) (if choose-many
                                                                         (map 'list #'parse-integer (ppcre:split "\\D+" input))
                                                                         input))
                                                 (is-integer-accepted? (int) (and (integerp int) (<= 1 int (length rows))))
                                                 (is-input-accepted? (input) (if choose-many
                                                                               (and input (every #'is-integer-accepted? input))
                                                                               (is-integer-accepted? input)))
                                                 (index-fun (numbers) (if choose-many
                                                                        (map 'list #'1- numbers)
                                                                        (1- numbers)))
                                                 (get-rows (numbers) (if choose-many
                                                                       (map 'list
                                                                            (lambda (i) (nth i rows))
                                                                            (index-fun numbers))
                                                                       (nth (index-fun numbers) rows))))
                                          (let* ((input (filter-input unfiltered-input))
                                                 (accepted (is-input-accepted? input)))
                                            (values accepted
                                                    (when accepted
                                                      (if get-index
                                                        (index-fun input)
                                                        (get-rows input)))))))
                       :prompt-fun prompt-fun
                       :raw-input choose-many))))


;; It's... it's bad, okay?
;; Maybe someday I refactor, I refactor it all.
(defun find-row-with-peeking-dialog (column-names rows &key ((:choose-many choose-many) nil)
                                                            ((:prompt-msg prompt-msg) "S[how] list again, [choose] row number or p[eek] it:~&")
                                                            ((:error-msg error-msg) "Please, try again.~&")
                                                            ((:id-column-name-map id-map) (lambda (column-names)
                                                                                            (cons "Row number" column-names)))
                                                            ((:row-mapping-function row-map) (lambda (row-number row)
                                                                                               (cons row-number row)))
                                                            ((:row-transformation-function row-transform) #'identity)
                                                            ((:get-index get-index) nil)
                                                            ((:prompt-fun prompt-fun) *prompt-fun*)
                                                            ((:pretty-print-table-function pretty-print-table) #'pretty-print-table)
                                                            ((:peek-row-function show-row) (lambda (row last?)
                                                                                             (format *standard-output* "~A~&~@[~%~]" row last?)))
                                                            &allow-other-keys)
  (cond ((= (length rows) 0) nil)
        ((= (length rows) 1)
         (cond ((and (not choose-many) get-index) 0)
               ((and choose-many get-index) (list 0))
               ((and (not choose-many) (not get-index)) (first rows))
               ((and choose-many (not get-index)) rows)))
        (:else
          (let ((rx (make-scanner (concat "\\s*"
                                          (combine (make-named-group :option "s(?:h(?:ow)?)?")
                                                   (concat (make-named-group :option "(?:c(?:h(?:oose)?)?)?|p(?:eek)?")
                                                           "\\s*"
                                                           (make-named-group :numbers (if choose-many
                                                                                        "\\d+(?:\\D+\\d+)*"
                                                                                        "\\d+"))))
                                          "\\s*")))
                (current-mode :choosing)
                (peeked-rows nil))
            (labels ((get-row-numbers (input-match &optional (no-input? nil)) (or (and (get-group :numbers input-match)
                                                                                       (if choose-many
                                                                                         (map 'list #'parse-integer (ppcre:split "\\D+" (get-group :numbers input-match)))
                                                                                         (parse-integer (get-group :numbers input-match))))
                                                                                  (and no-input?
                                                                                       peeked-rows)))
                     (accepted-number? (row-number) (and (integerp row-number)
                                                         (<= 1 row-number (length rows))))
                     (accepted-input? (input) (if choose-many
                                                (every #'accepted-number? input)
                                                (accepted-number? input)))
                     (show-rows (input) (let ((row-numbers (if (listp input) input (list input))))
                                          (loop :for current-row-numbers := row-numbers :then (rest current-row-numbers)
                                                :for current-row-number := (first current-row-numbers)
                                                :while current-row-number
                                                :do (funcall show-row
                                                             (nth (1- current-row-number) rows)
                                                             (null (rest current-row-numbers)))))))
              (dialog :query-fun (lambda (&optional (error-form nil error-form-supplied?))
                                   (declare (ignore error-form))
                                   (cond ((and (eq current-mode :choosing)
                                               error-form-supplied?)
                                          (format *standard-output* error-msg))
                                         ((eq current-mode :peeking)
                                          (setf current-mode :choosing))
                                         (:else
                                           (funcall pretty-print-table
                                                    (funcall id-map column-names)
                                                    (funcall row-transform (loop :for row :in rows
                                                                                 :for i :from 1 :to (length rows)
                                                                                 :collect (funcall row-map i row))))
                                           (format *standard-output* prompt-msg)
                                           (setf current-mode :choosing))))
                      :input-handler (lambda (unfiltered-input)
                                       (let* ((input-match (when unfiltered-input
                                                             (multiple-value-bind (matched match-list) (scan-named-groups rx unfiltered-input)
                                                               (and matched match-list))))
                                              (peeking? (is-group :option input-match "peek" "p"))
                                              (show-again? (is-group :option input-match "show" "sh" "s"))
                                              (row-numbers (get-row-numbers input-match (string= unfiltered-input "")))
                                              (accepted (accepted-input? row-numbers)))
                                         (cond (show-again? (setf current-mode :show-again)
                                                            (values nil :show-again))
                                               (peeking? (setf current-mode :peeking)
                                                         (setf peeked-rows row-numbers)
                                                         (show-rows row-numbers)
                                                         (values nil :peeking))
                                               (:else (values accepted (and accepted
                                                                            (cond ((and get-index (not choose-many)) (1- row-numbers))
                                                                                  ((and get-index choose-many) (map 'list #'1- row-numbers))
                                                                                  ((and (not get-index) (not choose-many)) (nth (1- row-numbers) rows))
                                                                                  ((and (not get-index) choose-many)
                                                                                   (map 'list
                                                                                        (lambda (index) (nth index rows))
                                                                                        (map 'list #'1- row-numbers))))))))))
                      :prompt-fun prompt-fun
                      :raw-input t))))))
