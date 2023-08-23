;;;; filesystem.lisp


(in-package :datapouch.filesystem)


(defun ensure-file-exists (path &optional (initial-text nil))
  (or (probe-file path)
      (progn
        (ensure-directories-exist (make-pathname :directory (pathname-directory path)))
        (with-open-file (file path :direction :output)
          (when initial-text
            (write initial-text :stream file))))))
