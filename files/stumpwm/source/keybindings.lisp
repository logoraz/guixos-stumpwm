(defpackage #:swm-config/keybindings
  (:use #:cl
        :stumpwm))
(in-package #:swm-config/keybindings)


;;; Enable multiple keyboard layouts (English and TBD)
;; TODO - disable message for this, I don't want to see it at start up.
;; function immediately runs switch-keyboard-layout which provides message!
;; (kbd-layouts:keyboard-layout-list "us")
;; (setf kbd-layouts:*caps-lock-behavior* :ctrl)

;;;  Defaults s-SPC for this command, reset & set this to prefix-key below!
(define-key *top-map* (kbd "s-k") "switch-keyboard-layout")

;; TODO: Determine what the value of AltGr key is and then let's set it.
;; (setf *altgr-offset* 4)
;; (register-altgr-as-modifier)

;;; Change the prefix key to Super-SPACE
(set-prefix-key (kbd "s-SPC"))

;;; Enable which-key
(which-key-mode)

;; Brightness Control parameters
(setf swm-brightness:*step* 10)


;;; Keymaps
;;; Ref: https://github.com/stumpwm/stumpwm/issues/480

;; Reference of others keybinding configs
;; https://systemcrafters.net/live-streams/december-3-2021/
;; https://github.com/aartaka/stumpwm-config

(defun update-keymap (keymap bindings)
  "Helper function to update desired StumpWM KEYMAP with BINDINGS."
  (loop :for (binding command) :in bindings
        :do (define-key keymap (kbd binding) command)))

(defun create-keymap (bindings)
  "Helper function to create desired StumpWM KEYMAP with BINDINGS.
BINDINGS is an alis '(('binding' 'command')...)"
  (let ((key-map (make-sparse-keymap)))
    (update-keymap! key-map bindings)
    key-map))

(defvar *kbds-top-map* 
  `(;; Audio/Mic Controls
    ("XF86AudioRaiseVolume" "wpctl-volume-up")
    ("XF86AudioLowerVolume" "wpctl-volume-down")
    ("XF86AudioMute"        "wpctl-toggle-mute")
    ("XF86AudioMicMute"     "wpctl-source-toggle-mute")

    ;; Brightness Controls
    ("XF86MonBrightnessUp"   "increase-brightness")
    ("XF86MonBrightnessDown" "decrease-brightness")

    ;; Window/Frame Groups
    ("s-1" "gselect 1")
    ("s-2" "gselect 2")
    ("s-3" "gselect 3")
    ("s-4" "gselect 4")
    ("s-5" "gselect 5")

    ;; Window/Frame Controls
    ("s-r"   "iresize")
    ("s-f"   "fullscreen")
    ("s-q"   "delete")
    ("C-s-1" "gmove HOME")
    ("C-s-2" "gmove DEV")
    ("C-s-3" "gmove WWW")
    ("C-s-4" "gmove ETC")
    ("C-s-5" "gmove PRIV")

    ;; Other (to be defined)
    ("s-h" ,*help-map*)
    ("s-g" ,*groups-map*)))

(defvar *kbds-root-map* 
  `(;; base root keybindings
    (":"       "eval")
    ("!"       "exec")
    ("k"       "delete")
    ;; ("C-k"     "delete") these do the same thing...
    ("h"       "vsplit")
    ("v"       "hsplit")
    ("x"       "remove-split")
    ("r"       "iresize")
    ("M-Up"    "move-window up")
    ("M-Down"  "move-window down")
    ("M-Left"  "move-window left")
    ("M-Right" "move-window right")))

;; Clear & Update prefix-key maps
;; see bindings.lisp for reference to originally defined maps
(defvar *root-map-bak* *root-map*
  "Backup for temporary purposes, to remove when refactor complete.")
(defvar *group-root-map-bak* stumpwm::*group-root-map*
  "Backup for temporary purposes, to remove when refactor complete.")
(defvar *tile-group-root-map-bak* stumpwm::*tile-group-root-map*
  "Backup for temporary purposes, to remove when refactor complete.")

(setf *root-map*                     (make-sparse-keymap)
      stumpwm::*group-root-map*      (make-sparse-keymap)
      stumpwm::*tile-group-root-map* (make-sparse-keymap))
(update-keymap *top-map*  *kbds-top-map*)
(update-keymap *root-map* *kbds-root-map*)


;;; Applications
(defvar *applications-keymap*
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (kbd "l") "exec lem")
    (define-key key-map (kbd "e") "exec emacs")
    (define-key key-map (kbd "x") "exec ~/.config/xorg/start-xterm.sh")
    (define-key key-map (kbd "b") "exec flatpak run app.zen_browser.zen")
    (define-key key-map (kbd "n") "exec flatpak run engineer.atlas.Nyxt-Electron")
    (define-key key-map (kbd "f") "exec flatpak run com.github.tchx84.Flatseal")
    (define-key key-map (kbd "k") "exec keepassxc")
    (define-key key-map (kbd "c") "exec gnucash")
    (define-key key-map (kbd "g") "exec gimp")
    (define-key key-map (kbd "p") "exec inkscape")
    (define-key key-map (kbd "r") "exec blender")
    (define-key key-map (kbd "o") "exec obs")
    (define-key key-map (kbd "z") "exec zathura")
    key-map))
(define-key *root-map* (kbd "a") '*applications-keymap*)

;;; Custom X11 System Controls
(defvar *xorg-keymap*
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (kbd "t") "toggle-trackpad")
    key-map))
(define-key *root-map* (kbd "C-t") '*xorg-keymap*)


;;; Screenshots
(defvar *screenshot-keymap*
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (kbd "s") "screenshot")
    (define-key key-map (kbd "w") "screenshot-window")
    (define-key key-map (kbd "a") "screenshot-area")
    key-map))
(define-key *root-map* (kbd "V") '*screenshot-keymap*)
(define-key *top-map* (kbd "Print") '*screenshot-keymap*)


;;; Session Controls (swm-end-session)
;; Screensaver command for slock
(defvar *screenlock-command* "slock"
  "Set screenlock command executable, default is slock.")

(defcommand lock-screen () ()
  "Screenlock command using slock - bound in keybindings under end-session map."
  (run-shell-command *screenlock-command*))

;; swm-end-session uses loginctl by default instead of systemctl
;; (setf swm-end-session:*end-session-command* "loginctl")

(defvar *end-session-keymap*
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (kbd "q") "end-session")
    (define-key key-map (kbd "l") "lock-screen")
    ;; FIXME - set so reload configuration if possible
    ;; (define-key key-map (kbd "l")   "loadrc")
    (define-key key-map (kbd "R") "restart-hard")
    key-map))
(define-key *root-map* (kbd "q") '*end-session-keymap*)

;;; Bluetooth Controls (bluetooth)
(defvar *bluetooth-keymap*
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (kbd "c") "bluetooth-connect")
    (define-key key-map (kbd "i") "bluetooth-turn-on")
    (define-key key-map (kbd "o") "bluetooth-turn-off")
    key-map))
(define-key *root-map* (kbd "B") '*bluetooth-keymap*)


;;; Slynk/Swank Server Controls
(defvar *stumpwm-port* 4005
  "Default port to establish a connection to either slynk or micros")

;; Emacs connection to StumpWM
(defcommand slynk-start-server () ()
  "Start a slynk server for sly."
  (slynk:create-server :port *stumpwm-port* :dont-close t)
  (echo-string (current-screen) "Starting slynk."))

(defcommand slynk-stop-server () ()
  "Stop current slynk server for sly."
  (slynk:stop-server *stumpwm-port*)
  (echo-string (current-screen) "Closing slynk."))


;; Lem connection to StumpWM *EXPERIMENTAL*
(defcommand micros-start-server () ()
  "Start a micros server for StumpWM/Lem."
  (micros:create-server :port *stumpwm-port* :dont-close t)
  (echo-string (current-screen) "Starting micros for StumpWM."))

(defcommand micros-stop-server () ()
  "Stop current micros server for StumpWM/Lem."
  (micros:stop-server *stumpwm-port*)
  (echo-string (current-screen) "Closing micros."))

(defvar *cl-server-keymap*
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (kbd "w") "slynk-start-server")
    (define-key key-map (kbd "x") "slynk-stop-server")
    (define-key key-map (kbd "y") "micros-start-server")
    (define-key key-map (kbd "z") "micros-stop-server")
    key-map))
(define-key *root-map* (kbd "L") '*cl-server-keymap*)
