(defsystem "smartos-test"
  :description "Sample SmartOS operator console demo for cl-tui"
  :version "0.1.0"
  :author "SCS"
  :license "MIT"
  :depends-on ("asdf" "uiop")
  :pathname "src"
  :components ((:file "package")
               (:file "terminal" :depends-on ("package"))
               (:file "input" :depends-on ("package" "terminal"))
               (:file "subprocess" :depends-on ("package"))
               (:file "operator" :depends-on ("package" "terminal" "input"))
               (:file "data" :depends-on ("package"))
               (:module "platform"
                :pathname "platform"
                :depends-on ("data" "subprocess")
                :serial t
                :components ((:file "hardware")
                             (:file "vmadm")
                             (:file "imgadm")))
               (:file "app-operator"
                :depends-on ("operator" "platform"))
               (:file "main" :depends-on ("app-operator"))))
