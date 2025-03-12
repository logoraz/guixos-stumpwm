(define-module (config packages cl-treesitter)
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

;;; TODO: 1. Update `tree-sitter` package version bump and new commit hash
;;;       2. Clean up and submit this package recipe upstream to Guix!
;;; Note: generate new sha256/base32 via
;;; guix hash -x --serializer=nar .
;;; Get commit via git log
(define latest-tree-sitter
  (options->transformation
   '((with-latest . "tree-sitter"))))

(define-public sbcl-cl-treesitter
  (let ((commit "5bc751beb50cbf084cd19be4e32aaeb45ba55568")
        (revision "0"))
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
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "bindings.lisp"
                         ;; TODO: Handle case for MacOS libtree-sitter.dylib
                         (("libtree-sitter.so" all)
                          (string-append
                           (assoc-ref inputs "tree-sitter") "/lib/" all))))))))
     (native-inputs
      (list sbcl-alexandria
            sbcl-fiveam))
     (inputs
      (list (latest-tree-sitter tree-sitter)
            sbcl-cffi
            sbcl-trivial-garbage))
     (home-page "https://github.com/garlic0x1/cl-treesitter")
     (synopsis "libtree-sitter bindings for Common Lisp")
     (description
      "Complete memory-safe libtree-sitter bindings for Common Lisp.")
     (license license:expat))))

(define-public cl-treesitter
  (sbcl-package->cl-source-package sbcl-cl-treesitter))

;; sbcl-cl-treesitter

