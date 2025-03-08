(defpackage #:lem-config/utilities
  (:use #:cl 
        :lem))
(in-package #:lem-config/utilities)


(defvar *lisp-implementations* (list  "sbcl" "ecl" "ccl"))

;; TODO
;; Need to find a way to diversify 'slime' command so the user can provide
;; other Common Lisp implementations to start a REPL with...

;; lem/extensions/lisp-mode/implementation.lisp
;; (defun list-installed-implementations ()
;;   (when (exist-program-p "sbcl")
;;     (list "sbcl" "ecl" "ccl")))

;;; Commands
(define-command open-init-file () ()
  ;; @sasanidas
  (lem:find-file
   (merge-pathnames "init.lisp" (lem-home))))

(define-command lisp-scratch-2 () ()
  (let ((buffer (primordial-buffer)))
    (change-buffer-mode buffer 'lem-lisp-mode:lisp-mode)
    (change-buffer-mode buffer 'lem-paredit-mode:paredit-mode t)
    (switch-to-buffer buffer)))
