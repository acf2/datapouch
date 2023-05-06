;;;; main.lisp


(in-package :zac.main)


(defun custom-prompt-fun (buffer)
  (let ((prompt (zac.box:get-prompt)))
    (format nil "~@[[~A]~]~:[*~;>~] " prompt (string= buffer ""))))


(defun make-zac (&rest args)
  (setf d.cli:*prompt-fun* #'custom-prompt-fun)
  (setf d.main:*init-hooks* (append d.main:*init-hooks*
                                    (list (lambda ()
                                            (zac.box:create-zettelkasten)
                                            (setf *package* (find-package "ZAC.USER"))))))
  (apply #'d.main:make-image args))
