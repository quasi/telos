(in-package :telos)

;;; Intent struct
;;; The core data structure representing intent at any level (feature, function, class)

(defstruct intent
  "Intent captures the WHY behind code - purpose, goals, constraints, and failure modes."
  ;; Required
  (purpose nil :type (or null string))
  ;; Optional but valuable
  (failure-modes nil :type list)
  ;; Optional
  (goals nil :type list)
  (constraints nil :type list)
  (assumptions nil :type list)
  (verification nil :type list)
  ;; Structural (for hierarchy)
  (belongs-to nil :type (or null symbol))
  (role nil :type (or null string))
  (members nil :type list))

;;; Entry accessors
;;;
;;; The entries in :goals, :failure-modes and the rest are literal lists of the
;;; shape (id) or (id "description" . options). These read that shape so callers
;;; do not have to know it — and so the option keys the validator accepts have a
;;; query path rather than only a list to walk.
;;;
;;; All three are total. The declaration macros reject a malformed entry, but
;;; MAKE-INTENT is exported and validates nothing, so a malformed entry can reach
;;; here — and CHECK-INTENT-REFERENCES sweeps every intent in the image, where one
;;; bad entry must not take the whole audit down. An entry that is not one has no
;;; id, description, or options; it does not signal.
;;;
;;; The INTENT-ENTRY- prefix is deliberate. ENTRY- is what (DEFSTRUCT ENTRY ID
;;; DESCRIPTION ...) generates, and a downstream package that :USEs TELOS would
;;; clobber these with nothing but a warning — silently breaking the audit, which
;;; is precisely the failure this library exists to prevent.

(defun intent-entry-id (entry)
  "The id of an intent entry, e.g. :FM1 from (:fm1 \"desc\" :violates :g1)."
  (when (consp entry)
    (car entry)))

(defun intent-entry-description (entry)
  "The description of an intent entry, or NIL for the bare (:id) shape."
  (when (and (consp entry) (consp (cdr entry)))
    (second entry)))

(defun intent-entry-option (entry key)
  "The value of entry option KEY, e.g. :VIOLATES or :MITIGATION, or NIL.
   A malformed option tail — dotted, or an odd number of elements — has no
   options rather than signalling the way GETF would."
  (let ((options (and (consp entry) (consp (cdr entry)) (cddr entry))))
    (when (and (proper-list-p options) (evenp (length options)))
      (getf options key))))
