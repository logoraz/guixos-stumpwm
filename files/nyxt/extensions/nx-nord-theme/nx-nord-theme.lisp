(nyxt:define-package #:nx-nord-theme)
(in-package #:nx-nord-theme)

;;; Nord Color Palette
(defparameter *nord00* "#2e3440")  ; 'Black'
(defparameter *nord01* "#3b4252")  ; "Dark Gray'
(defparameter *nord02* "#434c5e")  ; 'Medium Gray'
(defparameter *nord03* "#4c566a")  ; 'Gray'
(defparameter *nord04* "#d8dee9")  ; 'Light grey'
(defparameter *nord05* "#e5e9f0")  ; 'Off-white'
(defparameter *nord06* "#eceff4")  ; 'White'
(defparameter *nord07* "#8fbcbb")  ; 'Blue/Green'
(defparameter *nord08* "#88c0d0")  ; 'Teal'
(defparameter *nord09* "#81a1c1")  ; 'Blue/Gray'
(defparameter *nord10* "#5e81ac")  ; 'Blue'
(defparameter *nord11* "#bf616a")  ; 'Red'
(defparameter *nord12* "#d08770")  ; 'Orange'
(defparameter *nord13* "#ebcb8b")  ; 'Yellow'
(defparameter *nord14* "#a3be8c")  ; 'Green'
(defparameter *nord15* "#b48ead")  ; 'Purple'


;;; Invader Theme
(defvar *nx-nord-theme*
  (make-instance 'theme:theme
                 :background-color-   "#303240"
                 :background-color    "#282A36"
                 :background-color+   "#1E2029"
                 :on-background-color "#F7FBFC"

                 :primary-color-      "#679BCF"
                 :primary-color       "#789FE8"
                 :primary-color+      "#7FABD7"
                 :on-primary-color    "#0C0C0D"

                 :secondary-color-    "#44475A"
                 :secondary-color     "#44475A"
                 :secondary-color+    "#535A6E"
                 :on-secondary-color  "#F7FBFC"

                 :action-color-       "#6BE194"
                 :action-color        "#4FDB71"
                 :action-color+       "#27BF4C"
                 :on-action-color     "#0C0C0D"

                 :success-color-      "#86D58E"
                 :success-color       "#8AEA92"
                 :success-color+      "#71FE7D"
                 :on-success-color    "#0C0C0D"

                 :highlight-color-    "#EA43DD"
                 :highlight-color     "#F45DE8"
                 :highlight-color+    "#FC83F2"
                 :on-highlight-color  "#0C0C0D"

                 :warning-color-      "#FCA904"
                 :warning-color       "#FCBA04"
                 :warning-color+      "#FFD152"
                 :on-warning-color    "#0C0C0D"))

(define-configuration :browser
    ((theme *nx-nord-theme*)))

(define-configuration :status-buffer
    ((style (str:concat %slot-value%
                        (theme:themed-css (theme *browser*))))))



;;; Deprecated Methods
;;:dark-p              t
;; :codeblock-color-   "#3C5FAA"
;; :codeblock-color    "#355496"
;; :codeblock-color+   "#2D4880"
;; :on-codeblock-color "#F7FBFC"

;; (make-instance
;;  'theme:theme
;;  :background-color "#1e1e2e"
;;  :background-color+ "#181825"
;;  :background-color- "#313244"

;;  :primary-color "#45475a"
;;  :primary-color+ "#585b70"
;;  :primary-color- "#313244"

;;  :secondary-color "#45475a"
;;  :secondary-color+ "#313244"
;;  :secondary-color- "#585b70"

;;  :highlight-color "#b4befe"
;;  :highlight-color+ "#b4befe"
;;  :highlight-color- "#b4befe"

;;  :action-color "#89b4fa"
;;  :action-color+ "#89b4fa"
;;  :action-color- "#89b4fa"

;;  :success-color "#a6e3a1"
;;  :success-color+ "#a6e3a1"
;;  :success-color- "#a6e3a1"

;;  :warning-color "#f9e2af"
;;  :warning-color+ "#f9e2af"
;;  :warning-color- "#f9e2af"

;;  :on-background-color "white"
;;  :on-primary-color "white"
;;  :on-secondary-color "white"
;;  :on-highlight-color "white"
;;  :on-action-color "white"
;;  :on-success-color "white"
;;  :on-warning-color "white")
