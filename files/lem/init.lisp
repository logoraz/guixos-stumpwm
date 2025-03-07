;;; Ref: https://github.com/garlic0x1/.lem/
;;; Ref: https://github.com/fukamachi/.lem
(defpackage #:lem-config
  (:use #:cl 
        :lem))
(in-package #:lem-config)


;; Load init source files.
(let ((asdf:*central-registry*
        (append (list (asdf:system-source-directory :lem)
                      #P"~/.config/lem/"
                      #P"~/.local/share/common-lisp/source/")
                asdf:*central-registry*)))
  (asdf:load-system :lem-config))
