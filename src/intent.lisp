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
