;;;; datapouch-main.lisp


(in-package :datapouch.main)


(defvar +config-path+ (merge-pathnames #P".config/datapouch/" (user-homedir-pathname)))
(defparameter +work-dir+ (directory-namestring (or *load-truename* *default-pathname-defaults*)))


(defun ensure-file-exists (path &optional (initial-text nil))
  (or (probe-file path)
      (progn
        (ensure-directories-exist (make-pathname :directory (pathname-directory path)))
        (with-open-file (file path :direction :output)
          (when initial-text
            (write initial-text :stream file))))))


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


(defparameter *editor-interface* #'vim-editor-interface)
(defparameter *database-path* (merge-pathnames #P"database" +config-path+))
(defparameter *history-path* (merge-pathnames #P"history" +config-path+))


(defun edit-paths (&rest paths)
  (funcall *editor-interface* paths))

(defun edit-strings (&rest texts)
  (call-editor-for-many *editor-interface* texts))


(defparameter *init-hooks* nil)
(defparameter *exit-hooks* nil)
(defparameter *debugger-hooks* nil)


(defun debugger-hook (con val)
  (loop for fun in *debugger-hooks* do
        (funcall fun con val)))


;;; Readline config

(defun no-newline-after-debugger (con val)
  (declare (ignore con val))
  (setf *there-is-no-fresh-line-now* t))


(defun init-readline ()
  (setf *there-is-no-fresh-line-now* t)
  (setf *buffer* "")
  (pushnew #'no-newline-after-debugger *debugger-hooks*)
  (when *history-path* (rl:read-history (namestring (truename *history-path*))))
  (disable-bracketed-paste))


(defun finalize-readline ()
  (when *history-path* (rl:write-history (namestring (truename *history-path*))))
  (restore-bracketed-paste))

;;; SQLite config

(defun init-sqlite ()
  (setf *db* (sqlite:connect *database-path*))
  (use-foreign-keys t)) ; XXX: Because FUCK YOU foreign key default support


(defun finalize-sqlite ()
  (sqlite:disconnect *db*))


;;; Config files initialization
(defun config-files-init ()
  (ensure-file-exists *database-path*)
  (when *history-path* (ensure-file-exists *history-path*)))


(defun make-image (&rest args)
  (setf sb-ext:*init-hooks* (remove-duplicates (append sb-ext:*init-hooks*
                                                       (list #'config-files-init
                                                             #'init-readline
                                                             #'init-sqlite)
                                                       *init-hooks*)))
  (setf sb-ext:*exit-hooks* (remove-duplicates (append sb-ext:*exit-hooks*
                                                       (list #'finalize-readline
                                                             #'finalize-sqlite)
                                                       *exit-hooks*)))
  (setf sb-ext:*invoke-debugger-hook* #'debugger-hook)
  (setf sb-int:*repl-prompt-fun* (constantly ""))
  (setf sb-int:*repl-read-form-fun* (get-repl))
  (d.regex:allow-named-registers) ; Need this to use named registers
  (set-macro-character #\/ #'command-reader-macro nil *readtable*)
  (apply #'sb-ext:save-lisp-and-die args))
