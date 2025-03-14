(define-module (config system system-config)
  #:use-module (gnu)
  #:use-module (config system core heizenberg))

;;; Define GuixOS StumpWM - Crystallized Momentum for specified Machine:
(define guixos-stumpwm
  (operating-system
   (inherit %heizenberg)))

;;; Instantiate GuixOS StumpWM Crystallized Momentum
guixos-stumpwm
