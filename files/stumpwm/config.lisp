;;;; StumpWM XDG Initialization File (config.lisp --> config)

(in-package :stumpwm)

;;; Setup StumpWM configuration as a Common Lisp System!
(let ((asdf:*central-registry*
        (append (list (asdf:system-source-directory :stumpwm)
                      #P"~/.config/stumpwm/"
                      #P"~/.config/stumpwm/library/swm-wpctl/"
                      #P"~/.config/stumpwm/library/swm-end-session/"
                      #P"~/.config/stumpwm/library/swm-screenshot/"
                      ;; #P"~/.config/stumpwm/library/wip-swm-notify/"
                      #P"~/.config/stumpwm/library/swm-bluetooth/"
                      #P"~/.config/stumpwm/library/swm-brightness/"
                      ;; #P"~/.config/stumpwm/library/wip-swm-nmctl/"
                      #P"~/.local/share/common-lisp/source/")
                asdf:*central-registry*)))
  (asdf:load-system :swm-config))

