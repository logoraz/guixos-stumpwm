(define-module (config system core base-system)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix ci)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (config packages stumpwm)
  ;; #:use-module (config packages clasp)

  #:export (guixos-user-name
            guixos-keyboard-layout
            guixos-bootloader
            guixos-swap-devices
            guixos-file-systems
            guixos-groups
            guixos-users
            %dev-base-packages
            %cl-base-packages
            %stumpwm-base-packages
            %guixos-base-system-packages
            %guixos-base-packages
            substitutes->services
            %guixos-base-services
            %guixos-base-system))

(use-system-modules keyboard)

;;TODO: cleanup/organize
(use-package-modules
 ssh cups suckless fonts file-systems
 wm lisp lisp-xyz xorg xdisorg gnome
 linux audio version-control
 wget curl compression compton image-viewers 
 commencement base build-tools)

(use-service-modules
 cups ssh desktop xorg
 guix networking)


;;; operating-system parameters

(define guixos-user-name "logoraz")

(define guixos-keyboard-layout
  (keyboard-layout "us"))

(define guixos-bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets '("/boot/efi"))
   (keyboard-layout guixos-keyboard-layout)))

(define guixos-swap-devices
  (list (swap-space
         (target
          (uuid "0bf14f0b-3a99-439c-8ee0-130ce0792694")))))

(define guixos-file-systems
  ;; Use 'blkid' to find unique file system identifiers ("UUIDs").
  (cons* (file-system
          (mount-point  "/boot/efi")
          (device (uuid "645B-9C47"
			'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device (uuid "3d82108c-2c00-4238-b775-9f0c8894a638"
			'ext4))
          (type "ext4"))
	 %base-file-systems))

(define guixos-groups
  ;; Add the 'seat' group
  (cons
   (user-group (system? #t) (name "seat"))
   %base-groups))

(define guixos-users
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
;; (define latest-sbcl
;;   (options->transformation
;;    '((with-latest . "sbcl"))))

(define %dev-base-packages
  (list
   gcc-toolchain
   binutils
   (specification->package "make")
   meson))

(define %cl-base-packages
  (list 
   ;;clasp
   sbcl
   ecl
   ccl))

(define %stumpwm-base-packages
;; TODO: Organize and sort out modules/depencies...
  (list
   sbcl
   stumpwm-dev+servers ;; custom package
   sbcl-iterate
   sbcl-parse-float ;;|--> gnu packages lisp-xyz
   sbcl-local-time
   sbcl-cl-ppcre
   sbcl-zpng
   sbcl-salza2
   sbcl-clx
   sbcl-zpb-ttf
   sbcl-cl-vectors
   sbcl-cl-store
   sbcl-trivial-features
   sbcl-global-vars
   sbcl-trivial-garbage
   sbcl-bordeaux-threads
   sbcl-cl-fad
   sbcl-clx-truetype
   ;; external stumpwm-contrib packages
   ;; TODO: put vetted external contrib modules in stumpwm-package (works better)
   sbcl-stumpwm-ttf-fonts ;;|--> gnu packages wm;
   sbcl-stumpwm-kbd-layouts
   sbcl-stumpwm-swm-gaps
   sbcl-stumpwm-globalwindows
   sbcl-stumpwm-cpu
   sbcl-stumpwm-mem
   sbcl-stumpwm-wifi
   sbcl-stumpwm-battery-portable))

(define %guixos-base-system-packages
  (list
   ;; file systems
   bcachefs-tools

   ;; StumpWM Desktop Utilities
   picom
   feh
   libnotify
   pipewire
   wireplumber
   alsa-lib
   alsa-utils
   alsa-plugins
   bluez
   bluez-alsa
   brightnessctl
   lm-sensors
   openssh-sans-x
   git
   (list git "send-email")
   curl
   wget
   zip
   unzip

   ;; Xorg
   xterm ;;|--> gnu packages xorg
   transset
   xhost
   xset
   xsetroot
   xinput
   xrdb
   xrandr
   xclip ;;|--> gnu packages xdisorg
   xsel
   xss-lock
   xmodmap
   xsettingsd

   ;; Fonts
   font-hack
   font-jetbrains-mono
   font-awesome))

(define %guixos-base-packages
  (append %dev-base-packages
          %cl-base-packages
          %stumpwm-base-packages
          %guixos-base-system-packages
          %base-packages))


;;; System Services
;; https://guix.gnu.org/manual/en/html_node/Getting-Substitutes-from-Other-Servers.html
(define (substitutes->services config)
  (guix-configuration
   (inherit config)
   (substitute-urls
    (cons* "https://substitutes.nonguix.org"
           "https://ci.guix.gnu.org"
           %default-substitute-urls))
   (authorized-keys
    (cons* (origin
            (method url-fetch)
            (uri "https://substitutes.nonguix.org/signing-key.pub")
            (file-name "nonguix.pub")
            (sha256
             (base32
              "0j66nq1bxvbxf5n8q2py14sjbkn57my0mjwq7k1qm9ddghca7177")))
           %default-authorized-guix-keys))))

(define %guixos-base-services
  (cons*
   ;; Ref: https://guix.gnu.org/manual/en/html_node/X-Window.html
   (set-xorg-configuration
    (xorg-configuration
     (keyboard-layout guixos-keyboard-layout)))

   (service screen-locker-service-type
            (screen-locker-configuration
             (name "slock")
             (program (file-append slock "/bin/slock"))))

   ;; See: https://guix.gnu.org/manual/en/html_node/Desktop-Services.html
   (service bluetooth-service-type
            (bluetooth-configuration
             (bluez bluez)
             (auto-enable? #t)))

   (service cups-service-type
            (cups-configuration
             (web-interface? #t)
             (default-paper-size "Letter")
             (extensions (list cups-filters hplip-minimal))))

   ;; ssh user@host -p 2222
   (service openssh-service-type
            (openssh-configuration
             (openssh openssh)
             (port-number 2222)))

   ;; TODO: New - need to look into & configure!!
   (service tor-service-type)

   ;; See: https://guix.gnu.org/manual/en/html_node/Desktop-Services.html
   (modify-services %desktop-services
                    (guix-service-type
                     config =>
                     (substitutes->services config)))))


;;; Define GuixOS StumpWm - Crystallized Momentum
(define %guixos-base-system
  (operating-system
   (host-name "base-system")
   (timezone "America/Los_Angeles")
   (locale "en_US.utf8")
   (keyboard-layout guixos-keyboard-layout)

   (kernel linux)
   (firmware (list linux-firmware))

   (initrd microcode-initrd)
   ;; Fixes Xorg Lag - https://gitlab.com/nonguix/nonguix/-/issues/212
   (kernel-arguments (cons "i915.enable_psr=0" %default-kernel-arguments))

   (bootloader guixos-bootloader)

   (swap-devices guixos-swap-devices)

   ;; Use 'blkid' to find unique file system identifiers ("UUIDs").
   (file-systems guixos-file-systems)
   
   (groups guixos-groups)

   ;; List of user accounts ('root' is implicit).
   (users guixos-users)

   ;; Use 'guix search KEYWORD' to search for packages.
   (packages %guixos-base-packages)

   (services %guixos-base-services)))
