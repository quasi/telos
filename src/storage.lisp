(in-package :telos)

;;; Storage
;;; Registries and storage mechanisms for intent

;;; Feature registry
(defvar *feature-registry* (make-hash-table :test 'eq)
  "Maps feature-name (symbol) to intent struct")

;;; Class intent registry (for retrofitted classes)
(defvar *class-intent-registry* (make-hash-table :test 'eq)
  "Maps class-name (symbol) to intent struct. Used for retrofitted classes.")

;;; Method intent registry (for method specializers)
(defvar *method-intent-registry* (make-hash-table :test 'equal)
  "Maps method-spec (list) to intent struct. Key is (generic-name . specializers).")

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
