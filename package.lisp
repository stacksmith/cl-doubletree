;;;; package.lisp

(defpackage #:cl-doubletree
  (:nicknames :tree)
  (:use #:cl)
  (:shadow cl:first)
  (:export
   #:linkage #:prev #:next
   #:link #:link-before #:link-after #:unlink
   #:link-as-child-of
   #:node #:children #:dad
   #:first #:first-node #:next-node
   #:for-next-until #:for-prev-until
   #:for-next #:for-prev #:for-up
   #:traverse-next
   #:terminal #:nodep))

 
