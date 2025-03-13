(define-module (config packages cl-treesitter)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  ;; Modules for version bump of tree-sitter
  #:use-module (guix build-system gnu) ;; for tree-sitter
  #:use-module (gnu packages icu4c))

;;; TODO: 1. Update `tree-sitter` package version bump and new commit hash
;;;       2. Clean up and submit this package recipe upstream to Guix!
;;;
;;; Create package recipe to not have to set:
;; export LD_LIBRARY_PATH=~/home/logoraz/.guix-home/profile/lib
;; needed to compile local-copy of cl-treesitter...

;;; Note: generate new sha256/base32 via
;;; guix hash -x --serializer=nar .
;;; Get commit via git log
(define-public tree-sitter
  (package
    (name "tree-sitter")
    (version "0.25.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter")
                    (commit "2a835ee029dca1c325e6f1c01dbce40396f6123e")))
              (file-name (git-file-name name version))
              (hash
               (content-hash
                "0cck2wa17figxww7lb508sgwy9sbyqj89vxci07hiscr5sgdx9y5"))
              (modules '((guix build utils)))
              (snippet #~(begin
                           ;; Remove bundled ICU parts
                           (delete-file-recursively "lib/src/unicode")))))
    (build-system gnu-build-system)
    (inputs (list icu4c))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))
           #:tests? #f ; there are no tests for the runtime library
           #:make-flags
           #~(list (string-append "PREFIX=" #$output)
                   (string-append "CC=" #$(cc-for-target)))))
    (home-page "https://tree-sitter.github.io/tree-sitter/")
    (synopsis "Incremental parsing system for programming tools")
    (description
     "Tree-sitter is a parser generator tool and an incremental parsing
library.  It can build a concrete syntax tree for a source file and
efficiently update the syntax tree as the source file is edited.

Tree-sitter aims to be:

@itemize
@item General enough to parse any programming language
@item Fast enough to parse on every keystroke in a text editor
@item Robust enough to provide useful results even in the presence of syntax errors
@item Dependency-free so that the runtime library (which is written in pure C)
can be embedded in any application
@end itemize

This package includes the @code{libtree-sitter} runtime library.")
    (license license:expat)))


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
      (list tree-sitter
            sbcl-cffi
            sbcl-trivial-garbage))
     (home-page "https://github.com/garlic0x1/cl-treesitter")
     (synopsis "libtree-sitter bindings for Common Lisp")
     (description
      "libtree-sitter bindings for Common Lisp.")
     (license license:expat))))

(define-public cl-treesitter
  (sbcl-package->cl-source-package sbcl-cl-treesitter))

