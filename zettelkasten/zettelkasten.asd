;;;; zettelkasten.asd

(defsystem zettelkasten
  :description "Zettelkasten plugin for datapouch"
  :version "0.3"
  :author "Dmitry Kiselev"
  :components ((:file "packages")

               ;; Zettelkasten
               (:file "vars" :depends-on ("packages"))
               (:file "schema" :depends-on ("packages"))
               (:file "prompt" :depends-on ("packages" "vars"))
               (:file "basic-interface" :depends-on ("vars" "schema" "prompt"))
               (:file "pretty-traversal" :depends-on ("schema" "basic-interface"))
               (:file "main" :depends-on ("pretty-traversal" "basic-interface"))

               (:file "plugin" :depends-on ("packages" "main")))
  :depends-on (:datapouch :plugin-toolkit :alexandria)
  :perform (asdf:load-op (o c)
                         (symbol-call :zk.plugin :make-plugin)))
