;;;; zettelkasten-and-accounting.asd

(defsystem zettelkasten-and-accounting
  :description "Personal application"
  :version "0.1"
  :author "Dmitry Kiselev"
  :components ((:file "packages")
               (:file "auxiliary" :depends-on ("packages"))
               (:file "command-wrapper" :depends-on ("packages"))
               (:file "zettelkasten" :depends-on ("packages" "command-wrapper" "auxiliary"))
               (:file "bookkeeping" :depends-on ("packages" "command-wrapper" "auxiliary"))
               (:file "main" :depends-on ("packages" "zettelkasten" "bookkeeping" "command-wrapper")))
  :depends-on (:datapouch))
