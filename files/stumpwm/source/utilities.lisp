(defpackage :swm-config/utilities
  (:use :cl
        :stumpwm))
(in-package :swm-config/utilities)


;;; Experimental stumpwm-contrib packages (to trial)
;; Needed to include sbcl-stumpwm-notify in stumpwm package definition as input
;; in order to work and have access to sb-rotate-byte, otherwise fails.
(notify:notify-server-toggle)
(setf notify:*notify-server-max-title-lines* 2
      notify:*notify-server-max-body-lines* 20
      notify:*notify-server-max-line-length* 100
      notify:*notify-server-title-color* "^B^6"
      notify:*notify-server-body-color* "^7")

;; (defun notification-handler (app icon summary body)
;;   "Does things with incoming notifications"
;;   ...)

;; You can test notifications (if you have notify-send installed) with:
;; notify-send 'Hello world!' 'This is an example notification.'


;;; Notify that everything is ready!
(setf *startup-message* (concat "^6*^BGreetings logoraz! "
                                "Your StumpWM session is ready...^b"))
