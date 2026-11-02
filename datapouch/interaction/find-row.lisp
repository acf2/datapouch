;;;; interaction/find-row.lisp


(in-package :datapouch.interaction)


;(let ((seprx (ppcre:create-scanner (ppcre:parse-string "\\D+")))) 
;  (defun split-to-integers (string)
;    (ppcre:split seprx string)))
;
;
;(defun make-pager-selector-behavior-container ()
;  (let ((bc (d.ptrn:make-preset-behavior-container)))
;    (d.ptrn:set-behaviors
;      bc 
;      (:row-designator (d.ptrn:make-wildcard-pattern "[1-9]\\d*"
;                                                     (list "1" "90" "31337")
;                                                     "number"
;                                                     "N")
;                       (lambda (info num)
;                         (d.expr:make-result (getf info :name)
;                                             (parse-integer num)))
;                       :use-only-named-results nil)
;      (:row-designator-list (d.ptrn:make-wildcard-pattern "[1-9]\\d*(?:\\D+[1-9]\\d*)*"
;                                                          (list "1" "90" "31337" "1 2" "90,31" "31337 2,13")
;                                                          "numbers"
;                                                          "Ns")
;                            (lambda (info nums)
;                              (d.expr:make-result (getf info :name)
;                                                  (map 'list #'parse-integer
;                                                       (split-to-integers nums))))
;                            :use-only-named-results nil))
;    bc))


;; Make them index & index-list
;; Make their existense in behavior container required
;; That way, we invert the dependency

(d.c.aux:with-immutable-parsers
  (defun choose-command-yields (container choose-fun)
    (d.ptrn:collect-yields
      container
      ((:+ :begin
           (:trivial "choose" "c")
           :space
           (:row-designator :name :row-designator)
           :end)
       choose-fun
       "bla"))))


(d.c.aux:with-immutable-parsers
  (defun peek-command-yields (container peek-fun)
    (d.ptrn:collect-yields
      container
      ((:+ :begin 
           (:trivial "peek" "p")
           :space
           ;(:row-designator-list :name :row-designator-list) ; XXX: Wut? Peek many at once?
           (:row-designator :name :row-designator)
           :end)
       peek-fun
       "bla"))))


(d.c.aux:with-immutable-parsers
  (defun list-command-yields (container list-fun)
    (d.ptrn:collect-yields
      container
      ((:+ :begin
           (:trivial "list" "l")
           :end)
       list-fun
       "bla"))))


(defparameter *pagination-enabled* t)
(defparameter *page-length* 30)


;; -> choose, list, peek
;; paginate? -> forward [N], backward [N], begin [N]?, end [N]?
;; choose-many -> chosen?, choose* [+all], done, cart, leave/uncheck [+all]

(d.c.aux:with-immutable-parsers
  (defun pagination-commands-yields (container forward-fun backward-fun begin-fun end-fun)
    (d.ptrn:collect-yields
      container
      ((:+ :begin 
           (:trivial "forward" "f")
           (:? :space
               (:row-designator :name :pages))
           :end)
       forward-fun
       "[forward docs will be here]")
      ((:+ :begin 
           (:trivial "backward" "b")
           (:? :space
               (:row-designator :name :pages))
           :end)
       backward-fun
       "[backward docs will be here]")
      ((:+ :begin 
           (:trivial "start" "s")
           (:? :space
               (:row-designator :name :pages))
           :end)
       begin-fun
       "[begin docs will be here]")
      ((:+ :begin 
           (:trivial "end" "e")
           (:? :space
               (:row-designator :name :pages))
           :end)
       end-fun
       "[end docs will be here]"))))


;; Why row-map?
;; Why id-column-map?
;; Why row-transform?


(defun concat-yields (&rest yields)
  (map 'list (lambda (x)
               (reduce #'append x))
       (d.aux:rotate
         yields)))


;; Need to control printable-table index column + split function
;;   - Printable table creation fun \column-names rows -> printable-table
;;   - Base behavior container will have everything to parse and split

;;; Standartized lambda for all 'dialogs' will be
;;; (continuation column-names rows prompt message error-message)
;;; Where message and error-message could be functions

(defun pager-1choice-selector-dialog-template (index-pattern index-handler make-printable-table-fun)
  (let ((bc (d.ptrn:make-preset-behavior-container)))
    ;; Add index pattern manually
    (d.ptrn:set-behavior bc
                         :row-designator
                         index-pattern
                         index-handler
                         :use-only-named-results nil)
    ;; Generate dialog lambda: shift from 'template' to 'concrete'
    (lambda (continuation column-names rows prompt message error-message)
      (d.app:with-app-exit-progn
        exit-then-do
        (let ((assoc-table (make-simple-assoc-table column-names
                                                    rows
                                                    (funcall make-printable-table-fun
                                                             column-names
                                                             rows)))
              ;; These are application's internal 'state' to communicate between its parts
              (current-row-index 0)
              (paginate? (and *pagination-enabled*
                              (> (length rows) *page-length*)))
              (print-table? t)
              (print-message? t)
              (print-error-message? nil))
          (apply #'d.ptrn:yields-into-application
                 (append (concat-yields (choose-command-yields bc
                                                               (lambda (&key row-designator)
                                                                 ;; Accept predicate? 'Get index' parameter?
                                                                 (let ((result (assoc-row row-designator assoc-table)))
                                                                   (if result
                                                                     (exit-then-do
                                                                       (funcall continuation
                                                                                result))
                                                                     (setf print-error-message? row-designator)))))
                                        (peek-command-yields bc
                                                             (lambda (&key row-designator)
                                                               nil))
                                        (list-command-yields bc
                                                             (lambda ()
                                                               nil)))
                         (list :prompt-fun (lambda (buffer)
                                             (declare (ignore buffer))
                                             ;; TODO: Maybe redo with prompt-format??
                                             (prog1
                                               prompt
                                               (format *standard-output* "~@[~A~&~]~@[~A~&~]~@[~A~&~]"
                                                       (when print-table?
                                                         (if paginate?
                                                           (slice assoc-table
                                                                  current-row-index
                                                                  (+ current-row-index *page-length*))
                                                           assoc-table))
                                                       (when print-message?
                                                         (if (functionp message)
                                                           (funcall message print-message?)
                                                           message))
                                                       (when print-error-message?
                                                         (if (functionp error-message)
                                                           (funcall error-message print-error-message?)
                                                           error-message)))
                                               (setf print-table? nil
                                                     print-message? nil
                                                     print-error-message? nil)))))))))))


(let ((seprx (ppcre:create-scanner (ppcre:parse-string "\\D+")))) 
  (defun split-to-integers (string)
    (ppcre:split seprx string)))


(defun make-indexed-printable-table (column-names rows &key ((:index-column-name index-column-name) "Number"))
  (flet ((print-field (x) (wrap-string
                            (if (stringp x)
                              x
                              (write-to-string x)))))
    (let ((stringified-table (mapcar (lambda (row)
                                       (mapcar #'print-field
                                               row))
                                     (cons (cons index-column-name column-names)
                                           (loop :for row :in rows
                                                 :for i :from 1 :by 1
                                                 :collect (cons i row))))))
      (make-instance 'printable-table
                     :column-names (first stringified-table)
                     :rows (rest stringified-table)
                     :print-fun #'simple-pretty-print-table))))


(defun simple-pager-selector-dialog (continuation column-names rows prompt message error-message
                                     &key
                                     ((:choose-many choose-many) nil))
  (let ((index-pattern (d.ptrn:make-wildcard-pattern "[1-9]\\d*"
                                                     (list "1" "90" "31337")
                                                     "number"
                                                     "N"))
        (index-handler (lambda (info num)
                         (d.expr:make-result (getf info :name)
                                             num)))
        (index-list-pattern (d.ptrn:make-wildcard-pattern "[1-9]\\d*(?:\\D+[1-9]\\d*)*"
                                                          (list "1" "90" "31337" "1 2" "90,31" "31337 2,13")
                                                          "numbers"
                                                          "Ns"))
        (index-list-handler (lambda (info nums)
                              (d.expr:make-result (getf info :name)
                                                  (split-to-integers nums)))))
    (if choose-many
      nil
      (funcall (pager-1choice-selector-dialog-template index-pattern
                                                       index-handler
                                                       #'make-indexed-printable-table)
               continuation
               column-names
               rows
               prompt
               message
               error-message))))


(defun make-pager-selector-behavior-container ()
  (let ((bc (d.ptrn:make-preset-behavior-container)))
    (d.ptrn:set-behaviors
      bc 
      (:row-designator (d.ptrn:make-wildcard-pattern "[1-9]\\d*"
                                                     (list "1" "90" "31337")
                                                     "number"
                                                     "N")
                       (lambda (info num)
                         (d.expr:make-result (getf info :name)
                                             (parse-integer num)))
                       :use-only-named-results nil)
      (:row-designator-list (d.ptrn:make-wildcard-pattern "[1-9]\\d*(?:\\D+[1-9]\\d*)*"
                                                          (list "1" "90" "31337" "1 2" "90,31" "31337 2,13")
                                                          "numbers"
                                                          "Ns")
                            (lambda (info nums)
                              (d.expr:make-result (getf info :name)
                                                  (map 'list #'parse-integer
                                                       (split-to-integers nums))))
                            :use-only-named-results nil))
    bc))


(defun find-row-dialog (continuation column-names rows
                        &key
                        ((:choose-many choose-many) nil)
                        ((:prompt-msg prompt-msg) (if choose-many "(choose many)> " "(choose one)> "))
                        ((:error-fun error-fun) (lambda (incorrect-row-numbers)
                                                  (format *standard-output*
                                                          "These row numbers are incorrect: ~{~#[~;~A~:;~A ~]~}.~&Try again.~&"
                                                          (if (listp incorrect-row-numbers)
                                                            incorrect-row-numbers
                                                            (list incorrect-row-numbers)))))
                        ((:id-column-name-map id-map) (if choose-many
                                                        (lambda (column-names)
                                                          (list* "Chosen?" "Row number" column-names))
                                                        (lambda (column-names)
                                                          (cons "Row number" column-names))))
                        ((:row-mapping-function row-map) (if choose-many
                                                           (lambda (row-index row)
                                                             (list* nil (1+ row-index) row))
                                                           (lambda (row-index row)
                                                             (cons (1+ row-index) row))))
                        ((:row-designator-from-number index-fun) (lambda (row-number)
                                                                   (1- row-number)))
                        ((:row-transformation-function row-transform) #'identity)
                        ((:get-index get-index) nil)
                        ((:pretty-print-table-function pretty-print-table) #'pretty-print-table)
                        ((:peek-row-function show-row) (lambda (row last?)
                                                         (format *standard-output* "~A~&~@[~%~]" row last?)))
                        &allow-other-keys)
  (d.app:with-app-exit-progn
    exit-then-do
    (let* ((bc (make-pager-selector-behavior-container))
           (columns (funcall id-map column-names))
           (row-assoc (funcall row-transform (loop :for row :in rows
                                                   :for row-index :from 0 :to (1- (length rows))
                                                   :collect (funcall row-map row-index row)))))
      (labels ((is-integer-accepted? (int) (and (integerp int)
                                                (<= 1 int (length rows))))
               (print-rows (&optional row-numbers)
                           (let ((incorrect-row-numbers (remove-if #'is-integer-accepted? row-numbers)))
                             (if (null incorrect-row-numbers)
                               (funcall pretty-print-table
                                        columns
                                        (if row-numbers
                                          (map 'list (lambda (num)
                                                       (assoc num row-assoc))
                                               row-numbers)
                                          row-assoc))
                               (funcall error-fun incorrect-row-numbers))))
               (peek-rows (&optional row-numbers)
                          (let ((incorrect-row-numbers (remove-if #'is-integer-accepted? row-numbers)))
                            (if (null incorrect-row-numbers)
                              (loop :with chosen-rows := (if row-numbers
                                                           (map 'list (lambda (num)
                                                                        (assoc num row-assoc))
                                                                row-numbers)
                                                           row-assoc)
                                    :for rows-left := chosen-rows :then (rest rows-left)
                                    :for last-row := (null (rest rows-left))
                                    :do (funcall show-row (first rows-left) last-row))
                              (funcall error-fun incorrect-row-numbers)))))
        (let ((behavior-yields
                (cond (choose-many
                        nil)
                      (:else
                        (concat-yields (choose-command-yields bc
                                                              (lambda (&key row-designator)
                                                                (if (is-integer-accepted? row-designator)
                                                                  (exit-then-do
                                                                    (funcall continuation
                                                                             (if get-index
                                                                               (funcall index-fun row-designator)
                                                                               (nth (funcall index-fun row-designator) rows))))
                                                                  (funcall error-fun row-designator))))
                                       (peek-command-yields bc
                                                            (lambda (&key row-designator)
                                                              nil))
                                       (list-command-yields bc
                                                            (lambda ()
                                                              nil)))))))
          (apply #'d.ptrn:yields-into-application
                 (append behavior-yields
                         (list :prompt-fun (lambda (buffer)
                                             (declare (ignore buffer))
                                             (format nil prompt-msg))))))))))
