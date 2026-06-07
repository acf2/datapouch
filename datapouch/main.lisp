;;;; main.lisp


(in-package :datapouch.main)


(defparameter *preload-hooks* nil)
(defparameter *post-unload-hooks* nil)
(defparameter *init-hooks* nil)
(defparameter *exit-hooks* nil)
(defparameter *debugger-hooks* nil)


(defun debugger-hook (con val)
  (loop :for fun :in *debugger-hooks* :do
        (funcall fun con val)))


;;; Readline config


;(defun remove-extra-new-line-after-debugger (con val)
  ;(declare (ignore con val))
  ;(setf d.cli:*there-is-no-fresh-line-now* t))


(defun ignore-hook (con val)
  (declare (ignore val))
  (print con)
  (clear-input)
  (abort))


(defun init-readline ()
  (setf d.cli:*buffer* "")
  ;(pushnew #'no-newline-after-debugger *debugger-hooks*)
  ;(pushnew #'ignore-debugger *debugger-hooks*)
  (when *history-path* (rl:read-history (namestring (truename *history-path*))))
  (d.cli:register-datapouch-autocomplete)
  ;; Check if readline version > 8?
  (d.cli:disable-bracketed-paste))


(defun finalize-readline ()
  (when *history-path* (rl:write-history (namestring (truename *history-path*))))
  (d.cli:restore-bracketed-paste))


;;; SQLite config

(defun init-sqlite ()
  (when *database-path*
    (d.sql:open-db *database-path*)
    (d.sql:use-foreign-keys t) ; XXX: Because FUCK YOU foreign key default support
    (unless (d.sql:check-integrity :fast (not d.crypto:*advise-full-sqlite-integrity-check*))
      (format *standard-output* "===== WARNING: Sqlite internal integrity check has failed. =====~%=====   It is *HIGHLY* advised to reload correct backup.   =====~&"))))


(defun finalize-sqlite ()
  (d.sql:close-db))


(defparameter *plugin-prompt-funs* nil)

(defparameter +default-prompt-combination-fun+
  (lambda (buffer)
    (format nil "~{~@[[~A]~#[~:;-~]~]~}~:[*~:;>~] "
            (loop :for prompt-fun :in *plugin-prompt-funs*
                  :collect (funcall prompt-fun buffer))
            buffer)))

(defparameter *prompt-combination-fun* +default-prompt-combination-fun+)


(defparameter *plugin-yields* nil)


(defun make-top-application ()
  (handler-case (apply #'d.ptrn:yields-into-application
                       (append 
                         (map 'list (lambda (yield-list)
                                      (remove nil (reduce #'append yield-list)))
                              (d.aux:rotate *plugin-yields*))
                         (list :prompt-fun *prompt-combination-fun*)))
                (d.ptrn::incompatiple-regexes (c) 
                                      (setf sb-int:*repl-read-form-fun* (d.cli:get-parametrized-repl-read-form
                                                                          (lambda (buffer)
                                                                            (let ((*readtable* d.cli:*datapouch-readtable*))
                                                                              (d.cli:read-form buffer d.cli:*prompt-fun*))))) ; Best leave it to remain third to last
                                      (format t "PAIRS: ~A~&" (map 'list (lambda (pair)
                                                                           (list (d.regex:tree (first pair))
                                                                                 (d.regex:tree (second pair))))
                                                                   (d.ptrn::incompatible-pairs c))))))


;;; TODO add fast resave to some path
(defun make-image (&rest args)
  (setf sb-ext:*init-hooks* (remove-duplicates (append sb-ext:*init-hooks*
                                                       *preload-hooks*
                                                       (list #'init-database-file
                                                             #'d.crypto:check-database-integrity
                                                             #'init-application-files
                                                             #'d.fs:process-all-backup-tiers
                                                             #'init-readline
                                                             #'init-sqlite)
                                                       *init-hooks*)
                                               :from-end t))
  (setf sb-ext:*exit-hooks* (remove-duplicates (append sb-ext:*exit-hooks* ; TODO move to end of the list
                                                       *exit-hooks*
                                                       (list #'finalize-readline
                                                             #'finalize-sqlite
                                                             #'d.crypto:rehash-database)
                                                       *post-unload-hooks*)
                                               :from-end t))
  (d.regex:allow-named-registers)
  (d.rmacro:install-command-reader-macro :readtable d.cli:*datapouch-readtable*)
  ;(d.rmacro:install-command-reader-autoprint-hook) ; disable for now
  (setf sb-ext:*invoke-debugger-hook* #'debugger-hook)
  (setf d.main:*init-hooks* (append d.main:*init-hooks*
                                    (list (lambda () (setf *package* (find-package "CL-USER"))))))
  (setf sb-int:*repl-prompt-fun* (constantly ""))

  (setf sb-int:*repl-read-form-fun* (d.cli:get-parametrized-repl-read-form
                                      (lambda (buffer)
                                        (let ((*readtable* d.cli:*datapouch-readtable*))
                                          (d.cli:read-form buffer d.cli:*prompt-fun*))))) ; Best leave it to remain third to last
  ;(setf sb-int:*repl-read-form-fun* (d.app:get-app-repl-read-form))

  (if d.cli:*heretical-repl-available*
    (setf sb-impl::*repl-fun-generator* (constantly #'d.cli:repl-fun-with-readline))
    (setf d.cli:*add-fresh-line-after-each-result-print* t))
  (setf d.rmacro:*rmacro-callbacks* (reduce #'append *plugin-yields*))
  ;(make-top-application)
  (apply #'sb-ext:save-lisp-and-die args))
