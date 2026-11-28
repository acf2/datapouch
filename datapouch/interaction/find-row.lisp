;;;; interaction/find-row.lisp


(in-package :datapouch.interaction)


;; Make them index & index-list
;; Make their existense in behavior container required
;; That way, we invert the dependency

(defun %choose-command-yields (choose-fun)
  (lambda (container)
    (d.ptrn:collect-yields-with-immutable-parsers
      container
      ((:+ :begin
           (:trivial "choose" "c")
           :space
           (:row-designator :name :row-designator)
           :end)
       choose-fun
       "[choose docs should be here]"))))


(defun %peek-command-yields (access-state assoc-table peek-fn)
  (lambda (container)
    (d.ptrn:collect-yields-with-immutable-parsers
      container
      ((:+ :begin
           (:trivial "peek" "p")
           :space
           (:row-designator-list :name :rows-to-peek)
           :end)
       (lambda (&key rows-to-peek)
         (once-only (assoc-table peek-fn)
           (with-gensyms (rows null-rows)
             `(let* ((,rows (mapcar (lambda (row-id)
                                      (cons row-id
                                            (assoc-row row-id ,assoc-table)))
                                    (list ,@rows-to-peek)))
                     (,null-rows (mapcar #'first (remove-if #'rest ,rows))))
                (if ,null-rows
                  (funcall ,access-state :error-message
                           (format nil "These rows weren't found: ~{~#[~;~A~:;~A, ~]~}" ,null-rows))
                  (loop :for rows-left := ,rows :then (rest rows-left)
                        :for last-row := (null (rest rows-left))
                        :while rows-left
                        :do (funcall ,peek-fn (first rows-left) last-row)))))))
       "[peek docs should be here]"))))


(defun %list-command-yields (access-state)
  (lambda (container)
    (d.ptrn:collect-yields-with-immutable-parsers
      container
      ((:+ :begin
           (:trivial "list" "l")
           :end)
       (lambda ()
         `(progn
            (funcall ,access-state :print-table t)
            (funcall ,access-state :message t)))
       "bla"))))


(defparameter *pagination-enabled* t)
(defparameter *page-length* 30)


;; -> choose, list, peek
;; paginate? -> forward [N], backward [N], begin [N]?, end [N]?
;; choose-many -> chosen?, choose* [+all], done, cart, leave/uncheck [+all]

(defun %pagination-commands-yields (access-state table-length)
  (let* ((last-page-row-number
           (with-gensyms (last-page-length)
             `(let ((,last-page-length (mod ,table-length
                                            *page-length*)))
                (- ,table-length
                   (if (= ,last-page-length 0)
                     *page-length*
                     ,last-page-length)))))
         (pages-available `(ceiling ,table-length
                                    *page-length*)))
    (lambda (container)
      (d.ptrn:collect-yields-with-immutable-parsers
        container
        ((:+ :begin
             (:trivial "forward" "f")
             (:? :space
                 (:number :name :pages-forward))
             :end)
         (lambda (&key pages-forward)
           (once-only (pages-forward last-page-row-number)
             (with-gensyms (target-row-index)
               `(let ((,target-row-index (min (+ (funcall ,access-state :current-row-index)
                                                 (* *page-length* (or ,pages-forward 1)))
                                              ,last-page-row-number)))
                  (cond ((/= (funcall ,access-state :current-row-index)
                             ,last-page-row-number)
                         (funcall ,access-state :current-row-index ,target-row-index)
                         (funcall ,access-state :print-table t))
                        (:else
                          (funcall ,access-state :error-message "Already at last page")))))))
         "[forward docs will be here]")
        ((:+ :begin
             (:trivial "backward" "b")
             (:? :space
                 (:number :name :pages-backward))
             :end)
         (lambda (&key pages-backward)
           (once-only (pages-backward)
             (with-gensyms (target-row-index)
               `(let ((,target-row-index (max (- (funcall ,access-state :current-row-index)
                                                 (* *page-length* (or ,pages-backward 1)))
                                              0)))
                  (cond ((/= (funcall ,access-state :current-row-index)
                             0)
                         (funcall ,access-state :current-row-index ,target-row-index)
                         (funcall ,access-state :print-table t))
                        (:else
                          (funcall ,access-state :error-message "Already at first page")))))))
         "[backward docs will be here]")
        ((:+ :begin
             (:trivial "start" "s")
             (:? :space
                 (:number :name :page-from-start))
             :end)
         (lambda (&key page-from-start)
           (once-only (page-from-start last-page-row-number)
             (with-gensyms (target-row-index)
               `(let ((,target-row-index
                        (max 0 (min ,last-page-row-number (* *page-length* (1- (or ,page-from-start 1)))))))
                  (when (/= (funcall ,access-state :current-row-index)
                            ,target-row-index)
                    (funcall ,access-state :print-table t))
                  (funcall ,access-state :current-row-index ,target-row-index)))))
         "[start docs will be here]")
        ((:+ :begin
             (:trivial "end" "e")
             (:? :space
                 (:number :name :page-from-end))
             :end)
         (lambda (&key page-from-end)
           (once-only (page-from-end last-page-row-number)
             (with-gensyms (target-row-index)
               `(let ((,target-row-index
                        (max 0 (min ,last-page-row-number (* *page-length*
                                                             (- ,pages-available (or ,page-from-end 1)))))))
                  (when (/= (funcall ,access-state :current-row-index)
                            ,target-row-index)
                    (funcall ,access-state :print-table t))
                  (funcall ,access-state :current-row-index ,target-row-index)))))
         "[end docs will be here]")))))


;; Why row-map?
;; Why id-column-map?
;; Why row-transform?


(defun concat-yields (&rest yields)
  (map 'list (lambda (x)
               (reduce #'append x))
       (d.aux:rotate
         (remove-if #'null yields))))


(defun gather-yields-using-container (container &rest yield-fns)
  (mapcar (lambda (yield-list)
            (reduce #'append yield-list))
          (d.aux:rotate
            (mapcar (lambda (yield-fn)
                      (funcall yield-fn container))
                    (remove-if #'null yield-fns)))))


(defun %get-dialog-prompt (prompt message error-message access-state assoc-table table-length paginate?)
  (lambda (buffer)
    (declare (ignore buffer))
    ;; TBD: Maybe redo with prompt-format??
    (prog1
      prompt
      (let ((current-row-index (funcall access-state :current-row-index))
            (print-table? (funcall access-state :print-table))
            (message-state (funcall access-state :message))
            (error-message-state (funcall access-state :error-message))
            (pages-available (ceiling table-length
                                      *page-length*)))
        (format *standard-output* "~@{~@[~A~&~]~}"
                ;; User error message
                (when error-message-state
                  (if (functionp error-message)
                    (funcall error-message error-message-state)
                    error-message))
                ;; User message
                (when message-state
                  (if (functionp message)
                    (funcall message message-state)
                    message))
                ;; Table (or slice/page)
                (when print-table?
                  (if paginate?
                    (slice assoc-table
                           current-row-index
                           (+ current-row-index *page-length*))
                    assoc-table))
                ;; Pagination info
                (when (and print-table?
                           paginate?)
                  (format nil "Showing ~A-~A of ~A entries~&Page ~A of ~A~&"
                          (nth-key current-row-index
                                   assoc-table)
                          (nth-key (1- (min (+ current-row-index *page-length*)
                                            table-length))
                                   assoc-table)
                          table-length
                          (1+ (floor current-row-index
                                     *page-length*))
                          pages-available))))
      (funcall access-state :print-table nil)
      (funcall access-state :message nil)
      (funcall access-state :error-message nil))))


;; Need to control printable-table index column + split function
;;   - Printable table creation fun \column-names rows -> printable-table
;;   - Base behavior container will have everything to parse and split

;;; Standartized lambda for all 'table dialogs' will be
;;; (cont-gen column-names rows prompt message error-message)
;;; Where message and error-message could be functions

(defmacro pager-1choice-selector-dialog-template (container make-printable-table-fun peek-rows-fn)
  (once-only
    (container make-printable-table-fun peek-rows-fn)
    ;; Generate dialog lambda: shift from 'template' to 'concrete'
    `(lambda (cont-gen column-names rows prompt message error-message)
       (d.app:with-app-exit-progn
         exit-then-do
         (let* ((assoc-table (make-simple-assoc-table column-names
                                                      rows
                                                      (funcall ,make-printable-table-fun
                                                               column-names
                                                               rows)))
                (table-length (length rows))
                (paginate? (and *pagination-enabled*
                                (> (length rows) *page-length*)))
                ;; These are application's internal "state" to communicate between its parts
                (print-state (list :current-row-index 0
                                   :print-table t
                                   :message t
                                   :error-message nil))
                (access-state (lambda (key &optional (value nil value?))
                                (if value?
                                  (setf (getf print-state key)
                                        value)
                                  (getf print-state key))))
                ;; These are application's parts
                (choosing (%choose-command-yields
                            ;; Cannot really hide this lambda, because of exit-then-do part.
                            ;; It has to be in here.
                            (lambda (&key row-designator)
                              ;; Accept predicate? 'Get index' parameter?
                              (once-only (row-designator)
                                (with-gensyms (check)
                                  `(let ((,check (assoc-row ,row-designator ,assoc-table)))
                                     (if ,check
                                       ,(exit-then-do
                                          (funcall cont-gen check))
                                       (funcall ,access-state :error-message ,row-designator))))))))
                (paginating
                  (when paginate?
                    (%pagination-commands-yields access-state
                                                 table-length)))
                (peeking (%peek-command-yields access-state
                                               assoc-table
                                               ,peek-rows-fn))
                (listing (%list-command-yields access-state))
                ;; And this one is generalized prompt
                (prompt-fun
                  (%get-dialog-prompt prompt
                                      message
                                      error-message
                                      access-state
                                      assoc-table
                                      table-length
                                      paginate?)))
           (apply #'d.ptrn:yields-into-application
                  (append
                    (gather-yields-using-container
                      ,container
                      choosing
                      paginating
                      peeking
                      listing)
                    (list :prompt-fun prompt-fun))))))))


(let ((seprx (ppcre:create-scanner (ppcre:parse-string "\\D+")))) 
  (defun %split-to-integers (string)
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


(defun %generate-container-for-selector (index-pattern index-handler index-list-pattern index-list-handler)
  `(d.aux:bake-call
     (d.ptrn:set-behaviors
       (load-time-value (d.ptrn:make-preset-behavior-container))
       ;; Add number behavior (for pagination)
       (:number (d.ptrn:make-wildcard-pattern "[1-9]\\d*"
                                              (list "1" "90" "31337")
                                              "number"
                                              "N")
                (lambda (info num)
                  (d.expr:make-result (getf info :name)
                                      (parse-integer num)))
                :use-only-named-results nil)
       ;; Add index behavior manually
       (:row-designator ,index-pattern
                        ,index-handler
                        :use-only-named-results nil)
       (:row-designator-list ,index-list-pattern
                             ,index-list-handler
                             :use-only-named-results nil))))


(defmacro simple-pager-selector-dialog (&key (choose-many nil)
                                             (peek-row-fn
                                               (lambda (row last?)
                                                 (format *standard-output* "~A~&~@[~%~]" row last?))))
  (once-only (choose-many peek-row-fn)
    (with-gensyms (behavior-container)
      `(let ((,behavior-container ,(%generate-container-for-selector
                                     (d.ptrn:make-wildcard-pattern "[1-9]\\d*"
                                                                   (list "1" "90" "31337")
                                                                   "number"
                                                                   "N")
                                     (lambda (info num)
                                       (d.expr:make-result (getf info :name)
                                                           num))
                                     (d.ptrn:make-wildcard-pattern "[1-9]\\d*(?:\\D+[1-9]\\d*)*"
                                                                   (list "1" "90" "31337" "1 2" "90,31" "31337 2,13")
                                                                   "numbers"
                                                                   "Ns")
                                     (lambda (info nums)
                                       (d.expr:make-result (getf info :name)
                                                           (%split-to-integers nums))))))
         (if ,choose-many
           nil
           (pager-1choice-selector-dialog-template ,behavior-container
                                                   #'make-indexed-printable-table
                                                   ,peek-row-fn))))))


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
                                                       (%split-to-integers nums))))
                            :use-only-named-results nil))
    bc))


(defun find-row-dialog (cont-gen column-names rows
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
                                                                    (funcall cont-gen
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
