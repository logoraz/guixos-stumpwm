(defpackage #:lem-config/misc
  (:use #:cl 
        #:lem))
(in-package #:lem-config/misc)


;; Load Theme
;; (load-theme "decaf") ; default

;; Logs on the terminal output:
(log:config :info)

;; Dashboard
;; (setf lem-dashboard:*dashboard-enable* nil)
(lem-dashboard:set-default-dashboard :project-count 3 
                                     :file-count 7
                                     :hide-links t)

;;; Commands
(define-command open-init-file () ()
  ;; @sasanidas
  (lem:find-file
   (merge-pathnames "init.lisp" (lem-home))))

