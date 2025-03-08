(defsystem #:lem-config
  :author "Erik P Almaraz"
  :license "GPLv3"
  :version "0.0.1"
  :description "Lem Configuration."
  :class :package-inferred-system
  ;; :defsystem-depends-on (:asdf-package-system)  
  :depends-on (;; ":lem-config/extensions/tbd/all"
               )
  :serial t
  :pathname "source"
  :components ((:file "appearance")
               (:file "paredit")
               (:file "completions")
               (:file "keybindings")
               (:file "file-prompt")
               (:file "time-stamp")
               (:file "utilities")))

;; (register-system-packages "lem-config/extensions/tbd/all" '(#:extension))

