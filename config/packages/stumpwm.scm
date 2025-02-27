(define-module (config packages stumpwm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages texinfo))

(define-public stumpwm-dev
  (package
   (name "stumpwm")
   (version "019e44a110fff6d600802901c30d5be7a16da8d0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/stumpwm/stumpwm")
           (commit version)))
     (file-name (git-file-name "stumpwm" version))
     (hash
      (content-hash
       "0ja8njc88rffn2y164335sqcjlxrhc382yj3ykhdwn6my7v58c1b"))))
   (build-system asdf-build-system/sbcl)
   (native-inputs
    (list sbcl-fiasco
          texinfo

          ;; To build the manual.
          autoconf
          automake))
   (inputs
    (list sbcl-alexandria
          sbcl-cl-ppcre
          sbcl-clx))
   (arguments
    (list
     #:phases
     #~(modify-phases %standard-phases
                      (add-after 'create-asdf-configuration 'build-program
                                 (lambda* (#:key outputs #:allow-other-keys)
                                   (build-program
                                    (string-append #$output "/bin/stumpwm")
                                    outputs
                                    #:entry-program '((stumpwm:stumpwm) 0))))
                      (add-after 'build-program 'create-desktop-file
                                 (lambda* (#:key outputs #:allow-other-keys)
                                   (let* ((out #$output)
                                          (xsessions (string-append out "/share/xsessions")))
                                     (mkdir-p xsessions)
                                     (call-with-output-file
                                         (string-append xsessions "/stumpwm.desktop")
                                       (lambda (file)
                                         (format file
                                                 "[Desktop Entry]~@
                        Name=stumpwm~@
                        Comment=The Stump Window Manager~@
                        Exec=~a/bin/stumpwm~@
                        TryExec=~@*~a/bin/stumpwm~@
                        Icon=~@
                        Type=Application~%"
                                                 out))))))
                      (add-after 'create-desktop-file 'install-manual
                                 (lambda* (#:key (make-flags '()) outputs #:allow-other-keys)
                                   (let* ((out  #$output)
                                          (info (string-append out "/share/info")))
                                     (invoke "./autogen.sh")
                                     (invoke "sh" "./configure" "SHELL=sh")
                                     (apply invoke "make" "stumpwm.info" make-flags)
                                     (install-file "stumpwm.info" info)))))))
   (synopsis "Window manager written in Common Lisp")
   (description
    "Stumpwm is a window manager written entirely in Common Lisp.
It attempts to be highly customizable while relying entirely on the keyboard
for input.  These design decisions reflect the growing popularity of
productive, customizable lisp based systems.")
   (home-page "https://github.com/stumpwm/stumpwm")
   (license license:gpl2+)
   (properties `((cl-source-variant . ,(delay cl-stumpwm-dev))))))

(define-public cl-stumpwm-dev
  (package
    (inherit (sbcl-package->cl-source-package stumpwm-dev))
    (name "cl-stumpwm-dev")))

(define-public stumpwm-dev+servers
  (package
    (inherit stumpwm-dev)
    (name "stumpwm-with-servers")
    (inputs
     (list stumpwm-dev
           sbcl-slynk
           sbcl-micros
           sbcl-stumpwm-notify))
    (arguments
     (substitute-keyword-arguments (package-arguments stumpwm-dev)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'build-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (program (string-append out "/bin/stumpwm")))
                 (setenv "HOME" "/tmp")
                 (build-program program outputs
                                #:entry-program '((stumpwm:stumpwm) 0)
                                #:dependencies '("stumpwm"
                                                 "slynk"
                                                 "micros"
                                                 "notify")
                                #:dependency-prefixes
                                (map (lambda (input) (assoc-ref inputs input))
                                     '("stumpwm"
                                       "sbcl-slynk"
                                       "sbcl-micros"
                                       "sbcl-stumpwm-notify"))))))
           (delete 'copy-source)
           (delete 'build)
           (delete 'check)
           (delete 'remove-temporary-cache)
           (delete 'cleanup)))))))
