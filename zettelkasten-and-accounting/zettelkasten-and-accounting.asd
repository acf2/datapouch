;;;; zettelkasten-and-accounting.asd

(defsystem zettelkasten-and-accounting
  :description "Personal application"
  :version "0.1"
  :author "Dmitry Kiselev"
  :components ((:file "packages")
               (:file "auxiliary" :depends-on ("packages"))
               (:file "command-wrapper" :depends-on ("packages"))

               (:file "zettelkasten/vars" :depends-on ("packages"))
               (:file "zettelkasten/schema" :depends-on ("packages"))
               (:file "zettelkasten/prompt" :depends-on ("packages" "auxiliary" "zettelkasten/vars"))
               (:file "zettelkasten/basic-interface" :depends-on ("zettelkasten/vars" "zettelkasten/schema" "zettelkasten/prompt" "auxiliary"))
               (:file "zettelkasten/search" :depends-on ("zettelkasten/basic-interface"))

               (:file "zettelkasten/main" :depends-on ("command-wrapper" "zettelkasten/basic-interface" "zettelkasten/search"))

               (:file "bookkeeping" :depends-on ("packages" "command-wrapper" "auxiliary"))
               (:file "main" :depends-on ("packages" "zettelkasten/main" "bookkeeping" "command-wrapper")))
  :depends-on (:datapouch))
