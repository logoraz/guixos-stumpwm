(define-module (config home home-config)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services pm)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services dotfiles)
  #:use-module (guix gexp)
  #:use-module (guix transformations)
  #:use-module (config home services environment)
  #:use-module (config home services home-impure-symlinks)
  #:use-module (config home services xdg-files)
  #:use-module (config home services mutable-files)
  #:use-module (config home services streaming)
  #:use-module (config home services udiskie))

;;TODO: cleanup/organize
(use-package-modules fonts web-browsers gnuzilla password-utils gnupg mail
                     gstreamer video compton image-viewers linux music
                     gnucash gimp inkscape graphics image gnome gnome-xyz
                     guile guile-xyz emacs emacs-xyz sdl text-editors
                     shellutils pdf glib enchant pulseaudio webkit
                     lisp lisp-xyz lisp-check maths wm
                     freedesktop kde-frameworks
                     ssh cups suckless networking package-management)


;;; Package Transformations
(define latest-nyxt
  (options->transformation
   '((without-tests . "nyxt")
     (with-latest   . "nyxt"))))

;;; Packages
(define %guile-packages
  (list guile-next
        guile-ares-rs))

(define %cl-packages
  (list ccl
        ecl
        ;; clasp-cl (??)
        maxima
        cl-sketch
        cl-hunchentoot
        cl-easy-routes
        cl-djula
        cl-clack
        cl-mito
        cl-transducers
        cl-autowrap
        cl-jzon
        cl-rove
        cl-serapeum
        cl-trivial-types
        cl-closer-mop
        cl-lparallel))

(define %logoraz-packages
  (list picom                      ;;|--> StumpWM Tools
        feh
        libnotify

        ;; Mail
        mu
        isync
        msmtp

        ;; Flatpak & XDG Utilities
        flatpak
        xdg-desktop-portal
        xdg-desktop-portal-gnome
        xdg-utils
        xdg-dbus-proxy
        shared-mime-info
        (list glib "bin")

        ;; Appearance
        matcha-theme
        papirus-icon-theme
        adwaita-icon-theme
        breeze-icons ;; for KDE apps
        gnome-themes-extra
        bibata-cursor-theme

        ;; Fonts
        font-fira-code
        font-iosevka-aile
        font-google-noto
        font-google-noto-emoji
        font-google-noto-sans-cjk

        ;; Browsers
        ;; Install Nyxt via flatpak -> https://flathub.org/oc/setup/GNU%20Guix
        ;; (latest-nyxt nyxt)           ;;|--> gnu packages web-browsers :www-mail
        enchant
        icecat                       ;;|--> gnu packages gnuzilla

        ;; Editors/IDE's
        lem
        sdl2

        ;; Authentication/Encryption
        gnupg
        pinentry
        keepassxc
        password-store ;; move to password-store eventually...

        ;; Audio devices & Media playback
        mpv                        ;;|--> gnu packages video
        mpv-mpris
        vlc
        youtube-dl
        playerctl                  ;;|--> gnu packages music
        gstreamer
        gst-plugins-base
        gst-plugins-good
        gst-plugins-bad
        gst-plugins-ugly
        gst-libav
        pavucontrol

        ;; PDF reader
        zathura
        zathura-pdf-mupdf

        ;; Applications
        gnucash
        gimp
        inkscape
        blender

        ;; Utilities
        udiskie
        network-manager-applet
        trash-cli))

(define %emacs-packages
  (list  emacs
         emacs-diminish
         emacs-delight
         emacs-nord-theme
         emacs-doom-themes
         emacs-nerd-icons
         emacs-doom-modeline
         emacs-ligature
         emacs-no-littering
         emacs-ws-butler
         emacs-undo-tree
         emacs-paredit
         emacs-visual-fill-column
         emacs-ace-window
         emacs-mct
         emacs-orderless
         emacs-corfu
         emacs-marginalia
         emacs-beframe
         emacs-denote
         emacs-magit
         emacs-vterm
         emacs-guix
         emacs-arei
         emacs-sly
         emacs-mbsync
         emacs-org-superstar
         emacs-org-appear
         emacs-0x0
         emacs-erc-hl-nicks
         emacs-erc-image
         emacs-emojify))


(define %home-base-packages
  (append %cl-packages
          %guile-packages
          %emacs-packages
          %logoraz-packages))

;;; home-environment
(define %gosr (string-append "sudo guix system -L ~/dotfiles/ "
                             "reconfigure "
                             "~/dotfiles/config/system/system-config.scm"))

(define %gohr (string-append "guix home -L ~/dotfiles/ "
                             "reconfigure "
                             "~/dotfiles/config/home/home-config.scm"))

(define stumpwm-home
  (home-environment
   (packages %home-base-packages)

   (services
    (append (list
             ;; Enable pipewire audio
             (service home-pipewire-service-type)

             ;; Enable bluetooth connections to be handled properly
             ;; bluetooth service only currently available at system level.
             (service home-dbus-service-type)

             ;; Streaming profile service
             (service home-streaming-service-type)

             ;; Monitor battery levels
             (service home-batsignal-service-type)

             ;; Udiskie for auto-mounting
             (service home-udiskie-service-type)

             ;; XDG local files configuration
             (service home-xdg-local-files-service-type)

             ;; Mutable Local files symlinks configuration
             (service home-mutable-files-service-type)
             
             ;; Set environment variables for every session
             (service home-env-vars-configuration-service-type)

             (service home-bash-service-type
                      (home-bash-configuration
                       (guix-defaults? #f)
                       (aliases `(("grep" . "grep --color=auto")
                                  ("ls"   . "ls -p --color=auto")
                                  ("ll"   . "ls -l")
                                  ("la"   . "ls -la")
                                  ("gosr" . ,%gosr)
                                  ("gohr" . ,%gohr)))
                       (bashrc
                        (list (local-file "dot-bashrc.sh"
                                          #:recursive? #t)))
                       (bash-profile
                        (list (local-file "dot-bash_profile.sh"
                                          #:recursive? #t))))))
            %base-home-services))))


;; Enable Home
stumpwm-home
