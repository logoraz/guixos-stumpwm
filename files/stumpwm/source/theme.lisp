(defpackage :swm-config/theme
  (:use :cl
        :stumpwm
        :swm-config)
  (:export #:+nord00+
           #:+nord01+
           #:+nord02+
           #:+nord03+
           #:+nord04+
           #:+nord05+
           #:+nord06+
           #:+nord07+
           #:+nord08+
           #:+nord09+
           #:+nord10+
           #:+nord11+
           #:+nord12+
           #:+nord13+
           #:+nord14+
           #:+nord15+))
(in-package :swm-config/theme)

;;; Fonts

;; Enable TTF fonts
;; (load-module "ttf-fonts")
(setf xft:*font-dirs* (list (concat +guix-system-path+ "fonts/")
                            (concat +guix-home-path+ "fonts/")
                            (concat +xdg-data-home-path+ "fonts/"))
      clx-truetype:+font-cache-filename+ (concat +xdg-data-home-path+
                                                 "fonts/"
                                                 "font-cache.sexp"))
(xft:cache-fonts)
(set-font `(,(make-instance ;; f0
              'xft:font :family "Hack"
                        :subfamily "Regular" :size 11 :antialias t)
            ,(make-instance ;; f1
              'xft:font :family "JetBrains Mono"
                        :subfamily "Regular" :size 11 :antialias t)
            ,(make-instance ;; f2
              'xft:font :family "Symbols Nerd Font Mono"
                        :subfamily "Regular" :size 13 :antialias t)
            ,(make-instance ;; f3
              'xft:font :family "FontAwesome"
              :subfamily "Regular" :size 13 :antialias t)))


;;; Colors

;; Nord Color Palette
(defparameter +nord00+ "#2e3440")  ;; 'Black'
(defparameter +nord01+ "#3b4252")  ;; "Dark Gray'
(defparameter +nord02+ "#434c5e")  ;; 'Medium Gray'
(defparameter +nord03+ "#4c566a")  ;; 'Gray'
(defparameter +nord04+ "#d8dee9")  ;; 'Light Gray'
(defparameter +nord05+ "#e5e9f0")  ;; 'Off-white'
(defparameter +nord06+ "#eceff4")  ;; 'White'
(defparameter +nord07+ "#8fbcbb")  ;; 'Blue/Green'
(defparameter +nord08+ "#88c0d0")  ;; 'Teal'
(defparameter +nord09+ "#81a1c1")  ;; 'Blue/Gray'
(defparameter +nord10+ "#5e81ac")  ;; 'Blue'
(defparameter +nord11+ "#bf616a")  ;; 'Red'
(defparameter +nord12+ "#d08770")  ;; 'Orange'
(defparameter +nord13+ "#ebcb8b")  ;; 'Yellow'
(defparameter +nord14+ "#a3be8c")  ;; 'Green'
(defparameter +nord15+ "#b48ead")  ;; 'Purple'

(setq *colors*
      (list (list +nord00+ +nord01+)          ;; 0 Black
            +nord11+                          ;; 1 Red
            +nord14+                          ;; 2 Green
            +nord13+                          ;; 3 Yellow
            +nord10+                          ;; 4 Dark Blue
            +nord14+                          ;; 5 Magenta -> 'Green'
            +nord09+                          ;; 6 Cyan
            (list +nord05+ +nord06+)          ;; 7 White
            ;; Extra Colors
            +nord12+            ;; 8 optional-1 - 'Orange'
            +nord15+))               ;; 9 optional-2 - 'Purple'

;;; Initiate Color Theme
(when *initializing*
  (update-color-map (current-screen)))
