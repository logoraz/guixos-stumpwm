(defsystem "swm-config"
  :description "StumpWM config with batteries included!"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com"
  :license "BSD-3 Clause"
  :version "0.0.2"
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
               ;; modified stumpwm-contrib modules
               "swm-notify"
               "swm-wpctl"
               "swm-end-session"
               "swm-screenshot"
               "swm-bluetooth"
               "swm-brightness")
  :serial t
  :pathname "source"
  :components ((:file "swm-config")
               (:file "syntax")
               (:file "theme")
               (:file "frames")
               (:file "keybindings")
               (:file "modeline")
               (:file "utilities")))
