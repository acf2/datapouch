;;;; zettelkasten-and-accounting.asd

(defsystem zettelkasten-and-accounting
  :description "Personal application"
  :version "0.1"
  :author "Dmitry Kiselev"
  :components ((:file "packages")
               (:file "zettelkasten" :depends-on ("packages"))
               (:file "bookkeeping" :depends-on ("packages"))
               (:file "main" :depends-on ("packages" "zettelkasten" "bookkeeping")))
  :depends-on (:datapouch))
