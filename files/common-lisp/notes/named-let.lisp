;;;; Named Let functionality in Common Lisp
;;; see Common Lisp `labels`
;;; https://www.lispworks.com/documentation/HyperSpec/Body/s_flet_.htm

;; Perhaps one could use CLOS to create a method for let, that provides
;; named-let features...
;; Ref: https://gist.github.com/yosugi/4766190
;;; named let for common lisp
(defmacro nlet (tag var-vals &body body)
  `(labels ((,tag ,(mapcar #'car var-vals) ,@body))
           (,tag ,@(mapcar #'cadr var-vals))))

