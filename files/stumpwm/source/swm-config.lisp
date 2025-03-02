(defpackage #:swm-config
  (:use #:cl
        :stumpwm)
  (:local-nicknames (#:re #:ppcre))
  (:export #:*guix-system-path*
           #:*guix-home-path*))
(in-package #:swm-config)


;;;; Set PATHs: modules & data directories, etc.

;;; TODO: clean up and follow better practice for these...
;;; i.e. something similar to what is employed in StumpWM/primitives.lisp

;; Define Guix System/Home Profiles
(defparameter *guix-system-path* "/run/current-system/profile/share/"
  "Define Guix System profile PATH.")

(defparameter *guix-home-path* "/home/logoraz/.guix-home/profile/share/"
  "Define Guix Home profile PATH.")

;;; Debugging Logs
;; See stumpwm/primitives.lisp
(setf *data-dir* (uiop:xdg-cache-home "stumpwm/"))
(stumpwm::ensure-data-dir)

(setf *debug-level* 5)
(redirect-all-output (data-dir-file "debug-output" "txt"))

;; A startup message can be used when initializing StumpWM, for now set to nil.
(setf *startup-message* nil)


;;;; X11 Settings

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

;; WIP [Issue# 1]
;; Determine a way to dynamically find prop id as it seems to change when
;; devices are added, specifically wifi devices...
(defun format-output (value)
  "Format output string VALUE to remove superflous content."
  (let* ((regexp "\\((.*?)\\)") ;; captures (#%), etc
         (filter "[^()]+") ;; Filters captured string to remove parentheses.
         (match (re:scan-to-strings regexp value))
         (content (re:scan-to-strings filter match)))
    content))

(defun get-trackpad-id ()
  "Dynamically retriev trackpad id from xinput"
  (format-output
   (run-shell-command "xinput")))

(defvar *trackpad-command* "xinput set-prop 12 185"
  "Set xinput set-prop specifics to Enable/Disable trackpad...")

;;
;; END WIP

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
