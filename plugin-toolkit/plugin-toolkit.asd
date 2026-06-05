;;;; plugin-toolkit.asd


(defsystem plugin-toolkit
  :description "Common things to use in datapouch plugins"
  :version "0.1"
  :author "Dmitry Kiselev"
  :components ((:file "packages")
               (:file "auxiliary" :depends-on ("packages"))
               (:file "patterns" :depends-on ("packages")))
  :depends-on (:cl-reexport :datapouch))
