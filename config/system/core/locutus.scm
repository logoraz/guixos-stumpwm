(define-module (config system core locutus)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix packages)
  #:use-module (guix transformations)
  #:use-module (config system core base-system)

  #:export (%locutus))

(use-system-modules keyboard)

;;TODO: cleanup/organize
(use-package-modules
 fonts wm lisp lisp-xyz
 file-systems linux audio)

(use-service-modules
 desktop xorg
 guix networking)


;;; operating-system parameters
(set! guixos-user-name "logoraz")

(set! guixos-keyboard-layout
  (keyboard-layout "us"))

(set! guixos-bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets '("/boot/efi"))
   (keyboard-layout guixos-keyboard-layout)))

(set! guixos-swap-devices
  (list (swap-space
         (target
          (uuid "08b16346-aa6b-4744-9856-6070d78e38ca")))))

(set! guixos-file-systems
  ;; Use 'blkid' to find unique file system identifiers ("UUIDs").
  (cons* (file-system
          (mount-point  "/boot/efi")
          (device (uuid "F8E9-9C22"
			'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device (uuid "872f93f3-bcb4-4907-bb4c-d6002496d3a8"
			'ext4))
          (type "ext4"))
	 %base-file-systems))

(set! guixos-groups
  ;; Add the 'seat' group
  (cons
   (user-group (system? #t) (name "seat"))
   %base-groups))

(set! guixos-users
  (cons* (user-account
          (name "logoraz")
          (comment "Worker Bee")
          (home-directory "/home/logoraz")
          (group "users")
          (supplementary-groups '("wheel"    ;; sudo
                                  "seat"     ;; -
                                  "netdev"   ;; network devices
                                  "tty"      ;; -
                                  "input"    ;; -
                                  "lp"       ;; control bluetooth devices
                                  "audio"    ;; control audio devices
                                  "video"))) ;; control video devices
         %base-user-accounts))


;;; Packages & Transformations
;; ref: https://guix.gnu.org/manual/en/guix.html#Defining-Package-Variants
(define latest-sbcl
  (options->transformation
   '((with-latest . "sbcl"))))

(define %stumpwm-packages
  (list))

(define %guixos-system-packages
  (list
   ;; file systems
   bcachefs-tools))

(define %guixos-packages
  (append %guixos-system-packages
          %guixos-base-packages
          %base-packages))


;;; System Services
(define %guixos-services
  (cons*
   ;; Add other system services needed for heizenberg here...

   ;; See: https://guix.gnu.org/manual/en/html_node/Desktop-Services.html
   (modify-services %guixos-base-services
                    (guix-service-type
                     config =>
                     (substitutes->services config)))))


;;; Define GuixOS StumpWm - Crystallized Momentum

(define %locutus
  (operating-system
   (inherit %guixos-base-system)
   (host-name "locutus")
   (timezone "America/Los_Angeles")
   (locale "en_US.utf8")
   (keyboard-layout guixos-keyboard-layout)

   (bootloader guixos-bootloader)

   (swap-devices guixos-swap-devices)

   ;; Use 'blkid' to find unique file system identifiers ("UUIDs").
   (file-systems guixos-file-systems)
   
   (groups guixos-groups)

   ;; List of user accounts ('root' is implicit).
   (users guixos-users)

   ;; Use 'guix search KEYWORD' to search for packages.
   (packages %guixos-packages)

   (services %guixos-services)))
