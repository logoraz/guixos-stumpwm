;;;; Nyxt Configuration - Initialization File

;;; Commentary:
;;; Set buffer settings, password interface, Extensions & Hacks
;;;
;;; Nyxt as a FlatPak on Guix
;;; 1. flatpak override --user --filesystem=/run/current-system/profile:ro
;;;    --filesystem=/home/logoraz/.guix-home/profile:ro
;;;    --filesystem=/gnu/store:ro engineer.atlas.Nyxt
;;; 2.flatpak info --show-permissions engineer.atlas.Nyxt-Electron
;;;

(in-package #:nyxt-user)

;;; Reset ASDF registries to allow loading Lisp systems from
;;; everywhere...
;; (reset-asdf-registries)

(define-configuration buffer
    ((default-modes `(emacs-mode ,@%slot-value%))))

;; TODO: setup as a nyxt extension?
;; Loading files from the same directory (~/.config/nyxt/).
#-nyxt-4
(define-nyxt-user-system-and-load nyxt-user/basic-config
  :description "Nyxt Interface Configuration."
  :components ("keepassxc-pwi"
               "keepassxc-3431"))


;;; Nyxt Extensions

;; Borrowed from aartaka (see #:ref-1)
;; Loads extentions from #P"~/.local/share/nyxt/extensions/"
(defmacro defextsystem (system &optional file)
  "Helper macro to load configuration for extensions.
Loads a newly-generated ASDF system depending on SYSTEM.
FILE, if provided, is loaded after the generated system successfully
loads."
  `(define-nyxt-user-system-and-load ,(gensym "NYXT-USER/")
     :depends-on (,system) ,@(when file `(:components (,file)))))

(defextsystem #:nx-nord-theme)
(defextsystem #:nx-code)
(defextsystem #:nx-micros)


;;; Hacks

;; Borrowed from aartaka (see #:ref-1)
(defmethod files:resolve ((profile nyxt:nyxt-profile)
                          (file nyxt/mode/bookmark:bookmarks-file))
  "Re-route bookmarks to the `.config/nyxt/' directory."
  #P"~/.config/nyxt/bookmarks.lisp")

;; Hack to get rid of white echo-area
;; (nyxt:execute-command (echo "Hello, Welcome to Nyxt Electron Version 4!"))
;; (execute-command (nyxt-version))
