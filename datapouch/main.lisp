;;;; main.lisp


(in-package :datapouch.main)


(defvar +config-path+ (merge-pathnames #P".config/datapouch/" (user-homedir-pathname)))
(defparameter +work-dir+ (directory-namestring (or *load-truename* *default-pathname-defaults*)))


(defparameter *database-path* (merge-pathnames #P"database" +config-path+))
(defparameter *history-path* (merge-pathnames #P"history" +config-path+))


(defparameter *preload-hooks* nil)
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
                                                       *preload-hooks*
                                                       (list #'config-files-init
                                                             #'init-readline
                                                             #'init-sqlite)
                                                       *init-hooks*)))
  (setf sb-ext:*exit-hooks* (remove-duplicates (append sb-ext:*exit-hooks*
                                                       (list #'finalize-readline
                                                             #'finalize-sqlite)
                                                       *exit-hooks*)))
  (d.regex:allow-named-registers) ; Need this to use named registers
  (install-command-reader-macro)
  (setf sb-ext:*invoke-debugger-hook* #'debugger-hook)
  (setf sb-int:*repl-prompt-fun* (constantly ""))
  (setf sb-int:*repl-read-form-fun* (get-repl)) ; Best it to remain second to last
  (apply #'sb-ext:save-lisp-and-die args))
