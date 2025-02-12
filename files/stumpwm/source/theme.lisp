;;;; Theme settings for StumpWM

;;; Author:
;;; Erik P Almaraz

;;; License:
;;; GPLv3

;;; Commentary:
;;;

;;; References:
;;;


(in-package :stumpwm)

;;; Fonts

;; Enable TTF fonts
(load-module "ttf-fonts")
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
(defconstant +nord00+ "#2e3440")  ;; 'Black'
(defconstant +nord01+ "#3b4252")  ;; "Dark Gray'
(defconstant +nord02+ "#434c5e")  ;; 'Medium Gray'
(defconstant +nord03+ "#4c566a")  ;; 'Gray'
(defconstant +nord04+ "#d8dee9")  ;; 'Light Gray'
(defconstant +nord05+ "#e5e9f0")  ;; 'Off-white'
(defconstant +nord06+ "#eceff4")  ;; 'White'
(defconstant +nord07+ "#8fbcbb")  ;; 'Blue/Green'
(defconstant +nord08+ "#88c0d0")  ;; 'Teal'
(defconstant +nord09+ "#81a1c1")  ;; 'Blue/Gray'
(defconstant +nord10+ "#5e81ac")  ;; 'Blue'
(defconstant +nord11+ "#bf616a")  ;; 'Red'
(defconstant +nord12+ "#d08770")  ;; 'Orange'
(defconstant +nord13+ "#ebcb8b")  ;; 'Yellow'
(defconstant +nord14+ "#a3be8c")  ;; 'Green'
(defconstant +nord15+ "#b48ead")  ;; 'Purple'

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
