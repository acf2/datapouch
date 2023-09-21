;;;; main.lisp


(in-package :zac.main)


(defun custom-prompt-fun (buffer)
  (let ((prompt (zac.box:get-prompt)))
    (format nil "~@[[~A]~]~:[*~;>~] " prompt (string= buffer ""))))


;;; Very crude implementation of argument parsing
;;; I do not need it, except for one case
(defun parse-command-line-arguments (&optional (argv sb-ext:*posix-argv*))
  (let ((manual-db-path (loop with next-one = nil
                              for arg in argv
                              when next-one return arg
                              when (string= "--db" arg) do (setf next-one t))))
    (when manual-db-path
      (setf *database-path* (pathname manual-db-path))
      (setf *history-path* nil)
      (setf *backup-path* nil))))


(defun make-zac (&rest args)
  (setf d.cli:*prompt-fun* #'custom-prompt-fun)
  (setf d.main:*preload-hooks* (append d.main:*preload-hooks*
                                       (list #'parse-command-line-arguments)))
  (setf d.main:*init-hooks* (append d.main:*init-hooks*
                                    (list (lambda ()
                                            (zac.box:create-zettelkasten) ; TODO make it only once, and only for user database
                                            (setf *package* (find-package "ZAC.USER"))))))
  (apply #'d.main:make-image args))
