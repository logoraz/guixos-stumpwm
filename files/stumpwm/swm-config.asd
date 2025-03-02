(defsystem #:swm-config
  :description "StumpWM config with batteries included!"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com"
  :license "BSD-3 Clause"
  :version "0.0.2"
  :serial t
  :depends-on ("stumpwm"
               ;; stumpwm-contrib modules
               "ttf-fonts"
               "swm-gaps"
               "kbd-layouts"
               "globalwindows"
               "cpu"
               "mem"
               "battery-portable"
               "wifi"
               "notify" ;; packaged w/ custom Guix StumpWM package recipe to work
               ;; modified stumpwm-contrib modules
               "swm-wpctl"
               "swm-end-session"
               "swm-screenshot"
               "swm-bluetooth"
               "swm-brightness")
  :pathname "source"
  :components ((:file "swm-config")
               (:file "syntax")
               (:file "theme")
               (:file "frames")
               (:file "keybindings")
               (:file "modeline")
               (:file "utilities")))
