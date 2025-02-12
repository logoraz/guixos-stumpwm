;;;; StumpWM Initialization File (config.lisp --> config)

;;; Author:
;;; Erik P Almaraz

;;; License:
;;; GPLv3

;;; Commentary:
;;;

;;; References:
;;;

(in-package :stumpwm)


;;; Set PATHs: modules & data directories, etc.

;; Define Guix profiles
(defconstant +guix-system-path+ "/run/current-system/profile/share/"
  "Define Guix System profile PATH.")

(defconstant +guix-home-path+ "/home/logoraz/.guix-home/profile/share/"
  "Define Guix Home profile PATH.")

(defconstant +xdg-data-home-path+ (concat (getenv "XDG_DATA_HOME") "/") 
  "Define XDG_DATA_HOME PATH.")

(defconstant +xdg-cache-home-path+ (concat (getenv "XDG_CACHE_HOME") "/") 
  "Define XDG_CACHE_HOME PATH.")

(defconstant +swm-config-source-dir+ (concat (getenv "XDG_CONFIG_HOME")
                                             "/stumpwm/source/")
  "Define StumpWM Config Source Directory.")

(defconstant +swm-config-module-dir+ (concat (getenv "XDG_CONFIG_HOME")
                                             "/stumpwm/modules/")
  "Define StumpWM Config Modules Directory.")


;; Set StumpWM *official* contrib modules directory - at system level!
(set-module-dir (concat +guix-system-path+
                        "common-lisp/sbcl/"))

;;; Add custom modules to StumpWM Load Path:
(set-module-dir +swm-config-module-dir+)

;; Set StumpWM as default package
(setf *default-package* :stumpwm)

;; A startup message can be used when initializing StumpWM, for now set to nil.
(setf *startup-message* nil)

;;; Initialize X11 Desktop Environment & Resources.
(load #P"~/.config/stumpwm/source/start-xenv.lisp")

;;; Configure the core UIX
(load #P"~/.config/stumpwm/source/theme.lisp")
(load #P"~/.config/stumpwm/source/frames.lisp")
(load #P"~/.config/stumpwm/source/keybindings.lisp")

;;; Customize mode-line
(load #P"~/.config/stumpwm/source/modeline.lisp")

;;; Utilities & Experimental Feautures
(load #P"~/.config/stumpwm/source/utilities.lisp")

;; Notify that everything is ready!
(setf *startup-message* (concat "^6*^BGreetings logoraz! "
                                "Your StumpWM session is ready...^b"))
