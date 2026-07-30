(in-package :telos)

;;; Storage
;;; Registries and storage mechanisms for intent

;;; Entity intent registry
(defvar *entity-intent-registry* (make-hash-table :test 'equal)
  "Maps (kind symbol) to intent struct for function/struct/condition intent.")

;;; Feature registry
(defvar *feature-registry* (make-hash-table :test 'eq)
  "Maps feature-name (symbol) to intent struct")

;;; Class intent registry (for retrofitted classes)
(defvar *class-intent-registry* (make-hash-table :test 'eq)
  "Maps class-name (symbol) to intent struct. Used for retrofitted classes.")

;;; Decision registry
;;; Lives here rather than in decision.lisp because REPLACE-FEATURE-DECISIONS below
;;; refers to it and storage.lisp is loaded first.
(defvar *decision-registry* (make-hash-table :test 'eq)
  "Maps feature-name (symbol) to list of decision structs, most recent first")

;;; Method intent registry (for method specializers)
(defvar *method-intent-registry* (make-hash-table :test 'equal)
  "Maps method-spec (list) to intent struct. Key is (generic-name . specializers).")

(defun register-entity-intent (kind name intent)
  "Register KIND intent for NAME."
  (setf (gethash (list kind name) *entity-intent-registry*) intent))

(defun entity-intent (kind name)
  "Get KIND intent for NAME, or nil if not found."
  (gethash (list kind name) *entity-intent-registry*))

;;; Feature storage
(defun register-feature (name intent)
  "Register a feature with its intent"
  (setf (gethash name *feature-registry*) intent))

(defun feature-intent (name)
  "Get the intent for a feature, or nil if not found"
  (gethash name *feature-registry*))

(defun all-features ()
  "Return list of all registered feature names"
  (loop for name being the hash-keys of *feature-registry*
        collect name))

(defun replace-feature-decisions (feature-name decisions)
  "Replace FEATURE-NAME's decisions with DECISIONS, preserving source order."
  (setf (gethash feature-name *decision-registry*) (reverse decisions)))
