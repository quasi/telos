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

   ;; Decision tracking
   #:decision
   #:make-decision
   #:decision-id
   #:decision-chose
   #:decision-over
   #:decision-because
   #:decision-date
   #:decision-decided-by
   #:record-decision
   #:feature-decisions

   ;; Query API
   #:get-intent
   #:method-intent
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
