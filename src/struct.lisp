(in-package :telos)

;;; Struct intent
;;; defstruct/i macro for structs with intent

(defun parse-struct-slots-and-intent (body)
  "Separate struct slots from intent clauses.
   Slots are symbols or lists not starting with a keyword.
   Intent clauses are lists starting with a keyword.
   Returns (values slots intent-clauses)."
  (let ((slots nil)
        (intent-clauses nil))
    (dolist (item body)
      (if (and (consp item) (keywordp (car item)))
          (push item intent-clauses)
          (push item slots)))
    (values (nreverse slots) (nreverse intent-clauses))))

(defun intent-clauses-to-plist (clauses &optional context)
  "Convert intent clauses to a plist for make-intent.
   A keyword-headed clause that is not recognized signals
   INVALID-INTENT-DECLARATION — PARSE-STRUCT-SLOTS-AND-INTENT has already removed
   it from the slot list, so accepting it silently would lose it entirely."
  (let ((plist nil))
    (flet ((clause-value (clause) (intent-clause-value clause context)))
      (dolist (clause clauses)
        (case (car clause)
          (:feature (setf (getf plist :belongs-to) (clause-value clause)))
          (:role (setf (getf plist :role) (clause-value clause)))
          (:purpose (setf (getf plist :purpose) (clause-value clause)))
          (:failure-modes (setf (getf plist :failure-modes) (clause-value clause)))
          (:goals (setf (getf plist :goals) (clause-value clause)))
          (:constraints (setf (getf plist :constraints) (clause-value clause)))
          (:assumptions (setf (getf plist :assumptions) (clause-value clause)))
          (:verification (setf (getf plist :verification) (clause-value clause)))
          (otherwise (invalid-intent-clause clause context)))))
    plist))

(defmacro defstruct/i (name-and-options &body slots-and-intent)
  "Define a struct with intent.

   Like defstruct, but supports intent clauses after slots:
   - (:feature feature-name) - which feature this belongs to
   - (:role \"description\") - role within the feature
   - (:purpose \"description\") - why this struct exists
   - (:failure-modes ((id \"desc\") ...))
   - (:goals ((id \"desc\") ...))
   - (:constraints ((id \"desc\") ...))
   - (:assumptions ((id \"desc\") ...))
   - (:verification ((id \"desc\") ...))"
  (multiple-value-bind (slots intent-clauses)
      (parse-struct-slots-and-intent slots-and-intent)
    (let* ((name (if (consp name-and-options)
                     (car name-and-options)
                     name-and-options))
           (context (declaration-context "DEFSTRUCT/I" name))
           (intent-plist (intent-clauses-to-plist intent-clauses context))
           (feature (getf intent-plist :belongs-to)))
      (validate-intent-fields intent-plist context)
      `(progn
         (defstruct ,name-and-options ,@slots)
         (register-entity-intent
          :struct
          ',name
          (make-intent
           ,@(loop for (k v) on intent-plist by #'cddr
                   collect k
                   collect (if (member k '(:role :purpose))
                               v
                               `',v))))
         ,@(when feature `((register-member ',feature ',name :struct)))
         ',name))))
