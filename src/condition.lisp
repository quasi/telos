(in-package :telos)

;;; Condition intent
;;; define-condition/i macro for conditions with intent

(defmacro define-condition/i (name parent-types slots &rest options)
  "Define a condition with intent.

   Like define-condition, but supports intent clauses in options:
   - (:feature feature-name) - which feature this belongs to
   - (:role \"description\") - role within the feature
   - (:purpose \"description\") - why this condition exists
   - (:failure-modes ((id \"desc\") ...))
   - (:goals ((id \"desc\") ...))
   - (:constraints ((id \"desc\") ...))
   - (:assumptions ((id \"desc\") ...))
   - (:verification ((id \"desc\") ...))"
  (multiple-value-bind (intent-plist remaining-options)
      (parse-class-intent-options options)
    (let ((feature (getf intent-plist :belongs-to)))
      `(progn
         (define-condition ,name ,parent-types ,slots ,@remaining-options)
         (setf (get ',name 'telos:intent)
               (make-intent
                ,@(loop for (k v) on intent-plist by #'cddr
                        collect k
                        collect (if (member k '(:role :purpose))
                                    v
                                    `',v))))
         ,@(when feature `((register-member ',feature ',name :condition)))
         ',name))))
