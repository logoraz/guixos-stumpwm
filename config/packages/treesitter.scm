(define-module (config packages treesitter)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (guix utils)
  #:use-module (guix transformations)
  #:use-module (gnu packages)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages tree-sitter))

;;; Note: generate new sha256/base32 via
;;; guix hash -x --serializer=nar .
;;; Get commit via git log

(define latest-tree-sitter
  (options->transformation
   '((with-latest . "tree-sitter"))))

(define-public sbcl-cl-treesitter
  (let ((commit "5bc751beb50cbf084cd19be4e32aaeb45ba55568")
        (revision "4"))
    (package
     (name "sbcl-cl-treesitter")
     (version (git-version "0.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/garlic0x1/cl-treesitter")
             (commit commit)))
       (file-name (git-file-name name version))
       (hash
        (content-hash 
         "0b6gwzd3w8z1v8r3s6j6w6kkjrm3sn6wsx3z5hsqcrpvnwvkdil4"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      '(#:phases
        (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
          ;;TODO: Use gexps
          ;; see: https://guix.gnu.org/manual/en/html_node/G_002dExpressions.html 
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "bindings.lisp"
                         ;; TODO: Handle case for MacOS libtree-sitter.dylib
                         (("libtree-sitter.so" all)
                          (string-append
                           (assoc-ref inputs "tree-sitter") "/lib/" all))))))))
     (native-inputs ;; run-time dependencies (tests)
      (list sbcl-alexandria
            sbcl-fiveam))
     (inputs ;; build dependencies (package)
      (list (latest-tree-sitter tree-sitter)
            sbcl-cffi
            sbcl-trivial-garbage))
     (home-page "https://github.com/garlic0x1/cl-treesitter")
     (synopsis "libtree-sitter bindings for Common Lisp")
     (description
      "Safe and Complete Common Lisp bindings for libtree-sitter.")
     (license license:expat))))

(define-public cl-treesitter
  (sbcl-package->cl-source-package sbcl-cl-treesitter))

sbcl-cl-treesitter

;; (define-public sbcl-cl+ssl
;;   (let ((commit "17d5cdd65405f1d26e26f3e875e70027d0c8eedb")
;;         (revision "6"))
;;     (package
;;      (name "sbcl-cl+ssl")
;;      (version (git-version "0.0.0" revision commit))
;;      (source
;;       (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/cl-plus-ssl/cl-plus-ssl")
;;              (commit commit)))
;;        (file-name (git-file-name "cl+ssl" version))
;;        (sha256
;;         (base32 "0v0kx2m5355jkdshmj0z923c5rlvdl2n11rb3hjbv3kssdfsbs0s"))))
;;      (build-system asdf-build-system/sbcl)
;;      (arguments
;;       '(#:phases
;;         (modify-phases %standard-phases
;;          (add-after 'unpack 'fix-paths
;;           (lambda* (#:key inputs #:allow-other-keys)
;;             (substitute* "src/reload.lisp"
;;                          (("libssl.so" all)
;;                           (string-append
;;                            (assoc-ref inputs "openssl") "/lib/" all))
;;                          (("libcrypto.so" all)
;;                           (string-append
;;                            (assoc-ref inputs "openssl") "/lib/" all)))))
;;          (add-after 'fix-paths 'fix-tests
;;           (lambda _
;;             ;; Disable coverall support in tests because of a circular
;;             ;; dependency: cl+ssl -> cl-coverall -> dexador
;;             ;;          -> clack -> hunchentoot -> cl+ssl
;;             (substitute* "cl+ssl.test.asd"
;;                          (("\\(:feature \\(:or :sbcl :ccl\\) :cl-coveralls\\)")
;;                           "")))))))
;;      (native-inputs
;;       (list ;sbcl-cl-coveralls
;;        sbcl-fiveam
;;        sbcl-trivial-sockets))
;;      (inputs
;;       (list openssl
;;             sbcl-alexandria
;;             sbcl-bordeaux-threads
;;             sbcl-cffi
;;             sbcl-flexi-streams
;;             sbcl-trivial-features
;;             sbcl-trivial-garbage
;;             sbcl-trivial-gray-streams
;;             sbcl-usocket))
;;      (home-page "https://common-lisp.net/project/cl-plus-ssl/")
;;      (synopsis "Common Lisp bindings to OpenSSL")
;;      (description
;;       "This library is a fork of SSL-CMUCL.  The original SSL-CMUCL source
;; code was written by Eric Marsden and includes contributions by Jochen Schmidt.
;; Development into CL+SSL was done by David Lichteblau.")
;;      (license license:expat))))

;; (define-public cl-cl+ssl
;;   (sbcl-package->cl-source-package sbcl-cl+ssl))