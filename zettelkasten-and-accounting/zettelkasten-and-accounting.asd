;;;; zettelkasten-and-accounting.asd

(defsystem zettelkasten-and-accounting
  :description "Personal application"
  :version "0.2"
  :author "Dmitry Kiselev"
  :components ((:file "packages")
               (:file "auxiliary" :depends-on ("packages"))

               ;; Zettelkasten
               (:file "zettelkasten/vars" :depends-on ("packages"))
               (:file "zettelkasten/schema" :depends-on ("packages"))
               (:file "zettelkasten/prompt" :depends-on ("packages" "auxiliary" "zettelkasten/vars"))
               (:file "zettelkasten/basic-interface" :depends-on ("zettelkasten/vars" "zettelkasten/schema" "zettelkasten/prompt" "auxiliary"))
               (:file "zettelkasten/pretty-traversal" :depends-on ("zettelkasten/schema" "zettelkasten/basic-interface"))
               (:file "zettelkasten/main" :depends-on ("zettelkasten/pretty-traversal" "zettelkasten/basic-interface"))

               ;; Bookkeeping
               (:file "bookkeeping" :depends-on ("packages" "auxiliary"))

               ;; Weight diary

               (:file "main" :depends-on ("packages" "zettelkasten/main" "bookkeeping")))
  :depends-on (:datapouch))
