(defpackage :swm-config/keybindings
  (:use :cl
        :stumpwm))
(in-package :swm-config/keybindings)

;;; TODO - delete *root-map* and build back with only needed keybindings
;;;        problem is, by default it is a bit polluted and has a long load time.

;;; StumpWM contrib + custom modules used
;; (load-module "swm-wpctl")
;; (load-module "kbd-layouts")
;; (load-module "swm-bluetooth")
;; (load-module "swm-screenshot")
;; (load-module "end-session")
;; (load-module "swm-brightness")

;;; Enable multiple keyboard layouts (English and TBD)
;; TODO - disable message for this, I don't want to see it at start up.
;; function immediately runs switch-keyboard-layout which provides message!
;; (kbd-layouts:keyboard-layout-list "us")

;;;  Defaults s-SPC for this command, reset & set this to prefix-key below!
(define-key *top-map* (kbd "s-k") "switch-keyboard-layout")

;; TODO: Determine what the value of AltGr key is and then let's set it.
;; (setf *altgr-offset* 4)
;; (register-altgr-as-modifier)

;;; Change the prefix key to Super-SPACE
(set-prefix-key (kbd "s-SPC"))

;;; Enable which-key
(which-key-mode)

;;; Audio/Mic Controls
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "wpctl-volume-up")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "wpctl-volume-down")
(define-key *top-map* (kbd "XF86AudioMute") "wpctl-toggle-mute")
(define-key *top-map* (kbd "XF86AudioMicMute") "wpctl-source-toggle-mute")

;;; Brightness Controls
(setf swm-brightness:*step* 10)
(define-key *top-map* (kbd "XF86MonBrightnessUp")
  "increase-brightness")

(define-key *top-map* (kbd "XF86MonBrightnessDown")
  "decrease-brightness")


;;; Groups
(define-key *top-map* (kbd "s-1") "gselect 1")
(define-key *top-map* (kbd "s-2") "gselect 2")
(define-key *top-map* (kbd "s-3") "gselect 3")
(define-key *top-map* (kbd "s-4") "gselect 4")
(define-key *top-map* (kbd "s-5") "gselect 5")

;;; Applications
(defvar *my-applications-keymap*
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (kbd "e") "exec emacs")
    (define-key key-map (kbd "l") "exec lem")
    (define-key key-map (kbd "x") "exec ~/.config/xorg/start-xterm.sh")
    (define-key key-map (kbd "b") "exec flatpak run app.zen_browser.zen")
    (define-key key-map (kbd "n") "exec flatpak run engineer.atlas.Nyxt")
    (define-key key-map (kbd "k") "exec keepassxc")
    (define-key key-map (kbd "c") "exec gnucash")
    (define-key key-map (kbd "g") "exec gimp")
    (define-key key-map (kbd "p") "exec inkscape")
    (define-key key-map (kbd "r") "exec blender")
    (define-key key-map (kbd "o") "exec obs")
    (define-key key-map (kbd "z") "exec zathura")
    key-map))
(define-key *root-map* (kbd "a") '*my-applications-keymap*)

;;; Custom X11 System Controls
(defvar *my-xorg-keymap*
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (kbd "t") "toggle-trackpad")
    key-map))
(define-key *root-map* (kbd "C-t") '*my-xorg-keymap*)

;;; Screenshots
;; TODO - determine how to preset location for screenshots so I don't have to
;; type it in every time. Also, screenshot captures the stumpwm prompt as well...

(defvar *my-screenshot-dir* "~/desktop/screenshots/"
  "Path to custom screenshots directory.")

(defvar *my-screenshot-keymap*
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (kbd "s") "screenshot")
    (define-key key-map (kbd "w") "screenshot-window")
    (define-key key-map (kbd "a") "screenshot-area")
    key-map))
(define-key *root-map* (kbd "V") '*my-screenshot-keymap*)
(define-key *top-map* (kbd "Print") '*my-screenshot-keymap*)

;;; Session Controls (end-session)
;; Screensaver command for slock
(defvar *screenlock-command* "slock"
  "Set screenlock command executable, default is slock.")

(defcommand lock-screen () ()
  "Screenlock command using slock - bound in keybindings under end-session map."
  (run-shell-command *screenlock-command*))

;; Use loginctl instead of the default systemctl
(setf end-session:*end-session-command* "loginctl")

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
(asdf:load-systems :slynk :micros)

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
