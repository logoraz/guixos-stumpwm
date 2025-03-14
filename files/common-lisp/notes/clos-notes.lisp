(defpackage #:common-lisp/notes/clos
  (:use #:cl)
  (:export #:draw))
(in-package #:common-lisp/notes/clos)

;; Simple exposition of the power of CLOS
;; let's extend `+` to add strings as well...

;; Make a generic + in your own package, just don't try to do it to the one in the CL
;; package.
;; You can still use the cl package, you just declare that your package will 
;; shadow CL's +, and will need to refer to CL's + with package qualified cl:+ 
;; references.
;; For added mischief, look into symbol-macrolet  :-)

(defmethod + (a string) (b string)
  (concatonate 'string a b))

;;; Classes
(defclass point ()
  (x
   :initarg  :x
   :accessor :x)
  (y
   :initarg  :y
   :accessor :y))

(defclass line (point)
  (x1
   :initarg  :x1
   :accessor :x1)
  (x2
   :initarg  :x2
   :accessor :x2)
  (y1
   :initarg  :y1
   :accessor :y1)
  (y2
   :initarg  :y2
   :accessory :y2))

(defclass shape (line)
  direct-slots
  options)


(defclass triangle (shape)
  direct-slots
  options)

(defclass square (shape)
  direct-slots
  options)

(defclass circle (shape)
  direct-slots
  options)


;;; Generic Functions
(defgeneric draw (object shape) 
  "Generic function for Shape class instance.")


;;; Methods


