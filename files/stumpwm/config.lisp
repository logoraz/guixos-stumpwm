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

;;; TODO: clean up and follow better practice for these...
;;; i.e. something similar to what is employed in StumpWM/primitives.lisp

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

(defconstant +swm-config-library-dir+ (concat (getenv "XDG_CONFIG_HOME")
                                              "/stumpwm/library/")
  "Define StumpWM Config Module Library Directory.")

;;; Debugging Logs
;; See stumpwm/primitives.lisp
(setf *data-dir* (concat +xdg-cache-home-path+ "stumpwm/"))
(ensure-data-dir)

(setf *debug-level* 5)
(redirect-all-output (data-dir-file "debug-output" "txt"))

;; Set StumpWM *official* contrib modules directory - at system level!
(set-module-dir (concat +guix-system-path+
                        "common-lisp/sbcl/"))

;;; Add custom modules to StumpWM Load Path:
(set-module-dir +swm-config-library-dir+)

;; Set StumpWM as default package
(setf *default-package* :stumpwm)

;; A startup message can be used when initializing StumpWM, for now set to nil.
(setf *startup-message* nil)


;;; Helper Functions & Macros
;; (load #P"~/.config/stumpwm/source/syntax.lisp")

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
