(asdf:defsystem :cl-makdon
  :description "Markdown like simple markup language"
  :version "0.1.0"
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "MIT/Expat"
  :components ((:module "src"
                :components ((:file "packages")
                             (:file "makdon" :depends-on ("packages")))))
  :depends-on (:alexandria))
