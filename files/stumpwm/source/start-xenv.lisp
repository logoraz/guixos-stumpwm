(defpackage :swm-config/start-xenv
  (:use :cl
        :stumpwm))
(in-package :swm-config/start-xenv)


;;; X11 Settings

;;; Run xmodmap to remap keys
;; (run-shell-command "xmodmap ~/.xmodmap")
;; xrandr stuff would go here too?

;;; Set pointer/cursor paths & defaults -> theme set in .Xresources
;; Ref |--> https://github.com/ful1e5/XCursor-pro
(run-shell-command "xsetroot -xcf ~/.icons/XCursor-Pro-Dark/cursors/left_ptr 16")

;;; Turn off system bell & screen-saver control
(run-shell-command "xset b off")
(run-shell-command "xset s off")

;;; Disable Trackpad (set here as default)
(defvar *trackpadp* nil
  "Hold boolean state of trackpad.")

;; (run-shell-command "xinput set-prop 12 185 0")
;; Determine a way to dynamically find prop id as it seems to change when
;; devices are added, specifically wifi devices...
(defvar *trackpad-command* "xinput set-prop 10 185"
  "Set xinput set-prop specifics to Enable/Disable trackpad...")

(defun set-trackpad-state (&optional (state "0"))
  "Enable/Disable trackpad."
  (run-shell-command (concat *trackpad-command*
                             " "
                             state)))

(defcommand toggle-trackpad () ()
  "Toggle trackpad control..."
  (set-trackpad-state (if *trackpadp* "0" "1"))
  (setf *trackpadp* (not *trackpadp*)))

;; Always start with trackpad disabled
(set-trackpad-state)

;;; UI Settings

;;; Set Wallpaper
(run-shell-command "feh --bg-scale  ~/Pictures/wallpapers/desktop-bg.jpg")

;;; Enable screen locking on suspend
(run-shell-command "xss-lock -- slock")

;;; Additional Xorg resources
(run-shell-command "xrdb -merge ~/.Xresources")

;;; Enable screen compositing
;; Necessary to pecify xrender as backend (transparency/opacity effects)
(run-shell-command "picom --backend xrender")
