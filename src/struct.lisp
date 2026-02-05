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

(defun intent-clauses-to-plist (clauses)
  "Convert intent clauses to a plist for make-intent."
  (let ((plist nil))
    (dolist (clause clauses)
      (case (car clause)
        (:feature (setf (getf plist :belongs-to) (cadr clause)))
        (:role (setf (getf plist :role) (cadr clause)))
        (:purpose (setf (getf plist :purpose) (cadr clause)))
        (:failure-modes (setf (getf plist :failure-modes) (cadr clause)))
        (:goals (setf (getf plist :goals) (cadr clause)))
        (:constraints (setf (getf plist :constraints) (cadr clause)))
        (:assumptions (setf (getf plist :assumptions) (cadr clause)))
        (:verification (setf (getf plist :verification) (cadr clause)))))
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
           (intent-plist (intent-clauses-to-plist intent-clauses))
           (feature (getf intent-plist :belongs-to)))
      `(progn
         (defstruct ,name-and-options ,@slots)
         (setf (get ',name 'telos:intent)
               (make-intent
                ,@(loop for (k v) on intent-plist by #'cddr
                        collect k
                        collect (if (member k '(:role :purpose))
                                    v
                                    `',v))))
         ,@(when feature `((register-member ',feature ',name :struct)))
         ',name))))
