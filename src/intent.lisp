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

(defun entry-id (entry)
  "The id of an intent entry, e.g. :FM1 from (:fm1 \"desc\" :violates :g1)."
  (when (consp entry)
    (car entry)))

(defun entry-description (entry)
  "The description of an intent entry, or NIL for the bare (:id) shape."
  (when (consp entry)
    (second entry)))

(defun entry-option (entry key)
  "The value of entry option KEY, e.g. :VIOLATES or :MITIGATION, or NIL.
   Read-side: an entry that is not one simply has no options."
  (when (consp entry)
    (getf (cddr entry) key)))
