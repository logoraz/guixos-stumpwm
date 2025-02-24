(defpackage :swm-config/utilities
  (:use :cl
        :stumpwm))
(in-package :swm-config/utilities)


;;; Experimental stumpwm-contrib packages (to trial)
;; requires sbcl-xml-emitter and sbcl-dbus -> failing w/ error on sb-rotate-byte...
;; (load-module "notify")
;; (notify:notify-server-toggle)


;;; Notify that everything is ready!
(setf *startup-message* (concat "^6*^BGreetings logoraz! "
                                "Your StumpWM session is ready...^b"))
