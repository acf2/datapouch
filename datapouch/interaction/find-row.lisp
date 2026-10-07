;;;; interaction/find-row.lisp


(in-package :datapouch.interaction)


(let ((seprx (ppcre:create-scanner (ppcre:parse-string "\\D+")))) 
  (defun split-to-integers (string)
    (ppcre:split seprx string)))


(defun make-find-row-behavior-container ()
  (let ((bc (d.ptrn:make-preset-behavior-container)))
    (d.ptrn:set-behaviors
      bc 
      (:number (d.ptrn:make-wildcard-pattern "[1-9]\\d*"
                                             (list "1" "90" "31337")
                                             "number"
                                             "N")
               (lambda (info num)
                 (d.expr:make-result (getf info :name)
                                     (parse-integer num)))
               :use-only-named-results nil)
      (:number-list (d.ptrn:make-wildcard-pattern "[1-9]\\d*(?:\\D+[1-9]\\d*)*"
                                                  (list "1" "90" "31337" "1 2" "90,31" "31337 2,13")
                                                  "numbers"
                                                  "Ns")
                    (lambda (info nums)
                      (d.expr:make-result (getf info :name)
                                          (map 'list #'parse-integer
                                               (split-to-integers nums))))
                    :use-only-named-results nil))
    bc))


(d.c.aux:with-immutable-parsers
  (defun choose-command-yields (container choose-fun)
    (d.ptrn:collect-yields
      container
      ((:+ :begin
           (:trivial "choose" "c")
           :space
           (:number :name :row-number)
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
           (:number-list :name :row-number-list)
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
               (:number :name :pages))
           :end)
       forward-fun
       "[forward docs will be here]")
      ((:+ :begin 
           (:trivial "backward" "b")
           (:? :space
               (:number :name :pages))
           :end)
       backward-fun
       "[backward docs will be here]")
      ((:+ :begin 
           (:trivial "begin" "a") ; XXX: Like ^A, as opposite to ^E?
           ;;; Maybe start/s will be better?
           (:? :space
               (:number :name :pages))
           :end)
       begin-fun
       "[begin docs will be here]")
      ((:+ :begin 
           (:trivial "end" "e")
           (:? :space
               (:number :name :pages))
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


(defun pager-selector-dialog (continuation column-names rows prompt-msg
                              &key
                              ((:choose-many choose-many) nil)
                              ((:pretty-print-table-function pretty-print-table) #'pretty-print-table))
  (let ((paginate? (and *pagination-enabled*
                        (> (length rows) *page-length*))))
    (labels ((from-index (index) (1+ index))
             (to-index (row-number) (1- row-number))
             (correct-row-number? (int) (and (integerp int)
                                             (<= 1 int (length rows)))))
      (funcall pretty-print-table column-names rows))))


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
                        ((:index-from-number index-fun) (lambda (row-number)
                                                          (1- row-number)))
                        ((:row-transformation-function row-transform) #'identity)
                        ((:get-index get-index) nil)
                        ((:pretty-print-table-function pretty-print-table) #'pretty-print-table)
                        ((:peek-row-function show-row) (lambda (row last?)
                                                         (format *standard-output* "~A~&~@[~%~]" row last?)))
                        &allow-other-keys)
  (d.app:with-return
    return-from-app
    (let* ((bc (make-find-row-behavior-container))
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
                                                              (lambda (&key row-number)
                                                                (if (is-integer-accepted? row-number)
                                                                  (return-from-app
                                                                    (funcall continuation
                                                                             (if get-index
                                                                               (funcall index-fun row-number)
                                                                               (nth (funcall index-fun row-number) rows))))
                                                                  (funcall error-fun row-number))))
                                       (peek-command-yields bc
                                                            (lambda (&key row-number-list)
                                                              nil))
                                       (list-command-yields bc
                                                            (lambda ()
                                                              nil)))))))
          (apply #'d.ptrn:yields-into-application
                 (append behavior-yields
                         (list :prompt-fun (lambda (buffer)
                                             (declare (ignore buffer))
                                             (format nil prompt-msg))))))))))
