(defsystem #:lem-config
  :author "Erik P Almaraz"
  :license "GPLv3"
  :version "0.0.1"
  :description "Lem Configuration."
  :depends-on ()
  :pathname "source"
  :serial t
  :components ((:file "appearance")
               (:file "paredit")
               (:file "completions")
               (:file "keybindings")
               (:file "file-prompt")
               (:file "time-stamp")
               (:file "utilities")))
