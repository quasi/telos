(in-package :telos)

;;; Feature definitions
;;; Macros and functions for defining and querying features

(defmacro deffeature (name &key purpose goals constraints assumptions
                               failure-modes verification belongs-to
                               decisions)
  "Define a feature with its intent.

   NAME - symbol naming the feature
   PURPOSE - string describing why this feature exists (required in practice)
   GOALS - list of (:id \"description\") for success criteria
   CONSTRAINTS - list of (:id \"description\") for boundaries
   ASSUMPTIONS - list of (:id \"description\") for world assumptions
   FAILURE-MODES - list of (:id \"description\" :violates :goal-id)
   VERIFICATION - list of (:id \"description\")
   BELONGS-TO - parent feature symbol
   DECISIONS - list of decision plists (:id :chose :over :because :date :decided-by)"
  `(progn
     (register-feature
      ',name
      (make-intent :purpose ,purpose
                   :goals ',goals
                   :constraints ',constraints
                   :assumptions ',assumptions
                   :failure-modes ',failure-modes
                   :verification ',verification
                   :belongs-to ',belongs-to))
     ,@(when decisions
         `((replace-feature-decisions
            ',name
            (list
             ,@(loop for dec in decisions
                     collect `(make-decision
                               :id ,(getf dec :id)
                               :chose ,(getf dec :chose)
                               :over ',(getf dec :over)
                               :because ,(getf dec :because)
                               :date ,(getf dec :date)
                               :decided-by ,(getf dec :decided-by)))))))
     ',name))

(defun feature-parent (name)
  "Get the parent feature of NAME, or nil if no parent"
  (let ((intent (feature-intent name)))
    (when intent
      (intent-belongs-to intent))))

(defun feature-children (name)
  "Get all features that have NAME as their parent"
  (loop for feature-name being the hash-keys of *feature-registry*
        for intent = (gethash feature-name *feature-registry*)
        when (eq name (intent-belongs-to intent))
          collect feature-name))

(defun list-features (&rest args)
  "List features, optionally filtered.

   Supports both legacy positional FILTER and keyword arguments:
   (list-features \"auth\")
   (list-features :filter \"auth\" :parent 'security)

   FILTER - substring to match against name or purpose
   PARENT - only direct children of this feature
   UNDER - all descendants of this feature (not implemented yet)"
  (let* ((filter (if (and args (not (keywordp (car args))))
                     (pop args)
                     nil))
         (parent nil)
         (under nil))
    (when (oddp (length args))
      (error "LIST-FEATURES options must be keyword/value pairs: ~S" args))
    (loop for (key value) on args by #'cddr
          do (case key
               (:filter (setf filter value))
               (:parent (setf parent value))
               (:under (setf under value))
               (otherwise
                (error "Unknown LIST-FEATURES option ~S." key))))
    (when under nil)
    (let ((features (if parent
                        (feature-children parent)
                        (all-features))))
      (if filter
          (let ((filter-down (string-downcase filter)))
            (remove-if-not
             (lambda (name)
               (let* ((intent (feature-intent name))
                      (name-str (string-downcase (symbol-name name)))
                      (purpose (when intent (intent-purpose intent))))
                 (or (search filter-down name-str)
                     (and purpose
                          (search filter-down (string-downcase purpose))))))
             features))
          features))))
