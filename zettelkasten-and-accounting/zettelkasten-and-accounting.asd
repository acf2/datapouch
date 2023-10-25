;;;; zettelkasten-and-accounting.asd

(defsystem zettelkasten-and-accounting
  :description "Personal application"
  :version "0.1"
  :author "Dmitry Kiselev"
  :components ((:file "packages")
               (:file "command-wrapper" :depends-on ("packages"))
               (:file "zettelkasten" :depends-on ("packages" "command-wrapper"))
               (:file "bookkeeping" :depends-on ("packages" "command-wrapper"))
               (:file "main" :depends-on ("packages" "zettelkasten" "bookkeeping" "command-wrapper")))
  :depends-on (:datapouch))
