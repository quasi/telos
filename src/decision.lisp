(in-package :telos)

;;; Decision tracking
;;; Records of what was chosen, what was rejected, and why

(defstruct decision
  (id nil :type (or null keyword))
  (chose nil :type (or null string))
  (over nil :type list)
  (because nil :type (or null string))
  (date nil :type (or null string))
  (decided-by nil :type (or null string)))

(defvar *decision-registry* (make-hash-table :test 'eq)
  "Maps feature-name (symbol) to list of decision structs, most recent first")

(defun record-decision (feature-name &key id chose over because date decided-by)
  "Record a decision for FEATURE-NAME. Returns the decision struct.
   Decisions accumulate — multiple calls add to the list (most recent first)."
  (let ((decision (make-decision :id id
                                 :chose chose
                                 :over over
                                 :because because
                                 :date date
                                 :decided-by decided-by)))
    (push decision (gethash feature-name *decision-registry*))
    decision))

(defun feature-decisions (feature-name)
  "Get all decisions for FEATURE-NAME, most recent first. Returns nil if none."
  (gethash feature-name *decision-registry*))
