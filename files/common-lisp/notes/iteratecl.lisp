(defpackage #:common-lisp/notes/iteratecl
  (:use #:cl
        #:iterate)
  (:export #:leaf))

;;; New Drivers (extensibility) in Iterate
;; Iterate over the leaves of a tree:
;; Ref: https://sites.google.com/site/sabraonthehill/loop-v-iter

(iter:defmacro-driver (FOR leaf IN-TREE tree)
  "Iterate over the leaves in a tree"
  (let ((gtree (gensym))
        (stack (gensym))
        (kwd (if generate 'generate 'for)))
    `(progn
       (with ,gtree = ,tree)
       (with ,stack = (list ,gtree))
       (,kwd ,leaf next
             (let ((next-leaf
                     (iter (while ,stack)
                       (for node = (pop ,stack))
                       (if (consp node)
                           (destructuring-bind (l . r)
                               node
                             (unless (endp r)
                               (push r ,stack))
                             (push l ,stack))
                           (return node)))))
               (or next-leaf (terminate)))))))

(iter (for leaf in-tree '(((2 3) (5) 1) 8 (4 (1 (2)) 2) 3))
  (collect leaf into leaves)
  (multiply leaf into product)
  (finally (return (values leaves product))))
