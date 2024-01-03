;;;; main.lisp


(in-package :zac.main)


(defun custom-prompt-fun (buffer)
  (let ((box-prompt (zac.box:get-prompt)))
    (format nil "~@[[~A]~]~:[*~;>~] " box-prompt (string= buffer ""))))


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
  (zac.box:create-zettelkasten))


(defun add-all-commands ()
  (setf *commands*
        (zac.cmd:make-commands-from-wrappers
          (append
            (list (zac.cmd:make-command-wrapper '("init")
                                                (lambda (str match)
                                                  (declare (ignore str match))
                                                  (init-everything)))
                  (zac.cmd:make-command-wrapper '(("h(?:ello)?" ("dear" :optional) (:name . "\\w+"))
                                                  ("greet" (:name . "\\w+")))
                                                (lambda (str groups)
                                                  (declare (ignore str))
                                                  (format t "Hello, ~:(~A~)!~&"
                                                          (d.regex:get-group :name groups)))))
            (zac.box:get-zettelkasten-command-wrappers)))))
                                     

(defun make-zac (&rest args)
  (setf d.cli:*prompt-fun* #'custom-prompt-fun)
  (setf d.main:*preload-hooks* (append d.main:*preload-hooks*
                                       (list #'parse-command-line-arguments)))
  (setf d.main:*init-hooks* (append d.main:*init-hooks*
                                    (list (lambda () (setf *package* (find-package "ZAC.USER")))
                                          #'zettelkasten-init-hook)))
  (add-all-commands)
  (apply #'d.main:make-image args))
