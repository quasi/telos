(defpackage :telos
  (:use :cl)
  (:export
   ;; Struct
   #:intent
   #:make-intent
   #:intent-purpose
   #:intent-failure-modes
   #:intent-goals
   #:intent-constraints
   #:intent-assumptions
   #:intent-verification
   #:intent-belongs-to
   #:intent-role
   #:intent-members

   ;; Definition macros
   #:deffeature
   #:defun/i
   #:defclass/i
   #:defstruct/i
   #:define-condition/i
   #:defintent

   ;; Query API
   #:get-intent
   #:feature-intent
   #:feature-members
   #:feature-parent
   #:feature-children
   #:intent-feature
   #:intent-chain
   #:list-features

   ;; Metaclass
   #:intentful-class
   #:class-intent))
