;;;; plugin.lisp


(in-package :zettelkasten.plugin)


;;; Very crude implementation of argument parsing
;;; I do not need it, except for one case
(defun parse-command-line-arguments (&optional (argv sb-ext:*posix-argv*))
  (let ((manual-db-path (loop :with next-one = nil
                              :for arg :in argv
                              :when next-one :return arg
                              :when (string= "--db" arg) :do (setf next-one t))))
    (when manual-db-path
      (setf *database-path* (pathname manual-db-path))
      (setf *history-path* nil)
      (setf *control-database-integrity* nil)
      (setf *backup-tiers* nil))))


;;; tbh I don't think it should be called as init hook
;;; this is needed only once, at the very beginning
;;; and if db is damaged, it will generate errors
(defun init-everything ()
  (zk.db:create-zettelkasten))


(defun make-plugin ()
  (setf d.cli:*noprint-result* nil) ; At least for now
  (setf d.main:*preload-hooks* (append d.main:*preload-hooks*
                                       (list #'parse-command-line-arguments)))
  (setf d.main:*init-hooks* (append d.main:*init-hooks*
                                    (list #'zettelkasten-init-hook)))
  (setf d.main:*plugin-prompt-funs* (append d.main:*plugin-prompt-funs*
                                            (list (lambda (buffer)
                                                    (declare (ignore buffer))
                                                    (zk:get-prompt)))))
  (setf d.main:*plugin-yields* (cons (get-zettelkasten-yields)
                                     d.main:*plugin-yields*)))
