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

   ;; Intent entries
   #:intent-entry-list
   #:intent-entry-id
   #:intent-entry-description
   #:intent-entry-option

   ;; Declaration validation
   #:define-entry-option
   #:add-entry-option
   #:add-entry-options
   #:invalid-intent-declaration
   #:invalid-intent-declaration-context
   #:invalid-intent-declaration-field
   #:invalid-intent-declaration-entry
   #:invalid-intent-declaration-key
   #:invalid-intent-declaration-expected
   #:invalid-intent-declaration-reason

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

   ;; Programmatic registration
   ;;
   ;; The macros above are the ordinary way to declare intent. These are for a
   ;; caller that recorded annotations itself and replays them into telos at
   ;; runtime — a library whose fasls must not depend on telos being loaded, for
   ;; instance. Exported so such a caller need not reach for TELOS:: internals,
   ;; which a :depends-on does not license: a rename there would break the caller
   ;; at load time having warned it at compile time only.
   #:register-feature
   #:register-entity-intent
   #:entity-intent
   #:register-member
   #:replace-feature-decisions
   #:classify-symbol-intent-target

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
   #:list-decisions
   #:all-intentful-classes

   ;; Intent graph audit
   #:check-intent-references
   #:assert-intent-references
   #:intent-reference-error
   #:intent-reference-error-findings

   ;; Metaclass
   #:intentful-class
   #:class-intent))
