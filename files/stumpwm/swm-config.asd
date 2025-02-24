(defsystem "swm-config"
  :depends-on ("stumpwm"
               "ttf-fonts"
               "swm-gaps"
               "globalwindows"
               "swm-wpctl"
               "kbd-layouts"
               "swm-bluetooth"
               "swm-screenshot"
               "end-session"
               "swm-brightness"
               "cpu"
               "mem"
               "battery-portable"
               "wifi")
  :serial t
  :pathname "source"
  :components ((:file "swm-config")
               (:file "syntax")
               ;; (:file "start-xenv")
               (:file "theme")
               (:file "frames")
               (:file "keybindings")
               (:file "modeline")
               (:file "utilities")))
