;;;; Nyxt Passwords Modules

;;; Commentary:
;;; Note: Inorder to activate and be able to save new passwords, `save-new-password',
;;; you need to first instantiate via `copy-passowrd' or `copy-username'

;;; References
;;; 1. https://nyxt.atlas.engineer/documentation#keepassxc-support
;;; 2. https://github.com/aartaka/nyxt-config/tree/master
;;; 3. Look into `sera:resolve-executable'
;;;    (sera:resolve-executable "keepassxc-cli")

;; (in-package :nyxt-user)

(in-package :nyxt-user)

;; https://forums.linuxmint.com/viewtopic.php?t=313358
(defvar *keepassxc-exe* (concatenate 'string
                                     "flatpak-spawn --host "
                                     "keepassxc-cli")
  "KeePassXC executable for password interface")

(defvar *keepassxc-db* "/home/logoraz/Documents/moses/keepassxc/p.kdbx")

(defvar *keepassxc-kf* "/home/logoraz/Documents/moses/keepassxc/pkf")

(defvar *yubikey-slot* "") ; set as `empty string' to avoid propmt

(defmethod initialize-instance :after ((interface password:keepassxc-interface)
                                       &key &allow-other-keys)
           (setf (password:password-file interface) *keepassxc-db*
                 (password:key-file interface) *keepassxc-kf*
                 (password:yubikey-slot interface) *yubikey-slot*
                 (password:executable interface) *keepassxc-exe*))

(define-configuration nyxt/mode/password:password-mode
  ((password-interface (make-instance 'password:keepassxc-interface))))

(define-configuration buffer
    ((default-modes `(password-mode ,@%slot-value%))))

