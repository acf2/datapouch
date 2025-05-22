;;;; editor.lisp


(in-package :datapouch.editor)


;;; This will only work while uiop:with-temporary-file ensures existance of temporary file until progn ends
(defun call-editor (editor-interface &key ((:initial-text initial-text) nil))
  (with-temporary-file (:stream stream :pathname pathname :prefix "" :suffix "")
    (when initial-text
      (write-string initial-text stream))
    ; or should it be (close stream)?
    ; like here https://github.com/koji-kojiro/cl-repl/blob/bfd1521f856a9f8020611e8d8b34634fe75fbbc8/src/command.lisp#L60
    :close-stream
    (funcall editor-interface (list pathname))
    (with-open-file (stream pathname)
      (read-file-string pathname))))


;;; This is the simplier version, maybe later I'll consider it.
;(defun call-editor (editor-interface &key initial-text/s)
;  (let ((streams))
;    (unwind-protect
;         (progn (dolist (it (alexandria:ensure-list initial-text/s))
;                  (push (cl-fad:open-temporary) streams)
;                  (write-string (or it "") (car streams)))
;                (funcall editor-interface (pathname (car streams))))
;      (itr (for s in streams)
;           (close s)
;           (collect (uiop:read-file-string (pathname s)))))))

;;; Also, there is the version with explicit catamorphism.
;;; Not that I know how it works. Maybe later too.


(defun call-editor-for-many (editor-interface initial-texts &optional (pathnames nil))
  (if initial-texts
    (let* ((other-results nil)
           (result (call-editor (lambda (new-pathname)
                                  (setf other-results
                                        (call-editor-for-many editor-interface
                                                              (cdr initial-texts)
                                                              (append pathnames new-pathname))))
                                :initial-text (car initial-texts))))
      (cons result other-results))
    (when pathnames
      (funcall editor-interface pathnames))))


(defun vim-editor-interface (pathnames)
  (run-program (append (list "vim" "-p") (map 'list #'namestring pathnames))
               :input :interactive
               :output :interactive))


(defparameter +default-editor-interface+ #'vim-editor-interface)
(defparameter *editor-interface* nil)


(defun edit-paths (&rest paths)
  (funcall (or *editor-interface*
               +default-editor-interface+)
           paths))


(defun edit-strings (&rest texts)
  (call-editor-for-many (or *editor-interface*
                            +default-editor-interface+)
                        texts))
