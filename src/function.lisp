(in-package :telos)

;;; Function intent
;;; defun/i macro and defintent for retrofitting

(defun parse-intent-clauses (body)
  "Parse intent clauses from the beginning of a function body.
   Returns (values intent-plist remaining-body).
   Intent clauses are forms like (:feature foo) (:role \"...\") etc."
  (let ((intent-plist nil)
        (remaining body))
    (loop while (and remaining
                     (consp (car remaining))
                     (keywordp (caar remaining)))
          do (let ((clause (pop remaining)))
               (case (car clause)
                 (:feature (setf (getf intent-plist :belongs-to) (cadr clause)))
                 (:role (setf (getf intent-plist :role) (cadr clause)))
                 (:purpose (setf (getf intent-plist :purpose) (cadr clause)))
                 (:failure-modes (setf (getf intent-plist :failure-modes) (cadr clause)))
                 (:goals (setf (getf intent-plist :goals) (cadr clause)))
                 (:constraints (setf (getf intent-plist :constraints) (cadr clause)))
                 (:assumptions (setf (getf intent-plist :assumptions) (cadr clause)))
                 (:verification (setf (getf intent-plist :verification) (cadr clause))))))
    (values intent-plist remaining)))

(defmacro defun/i (name lambda-list &body body)
  "Define a function with intent.

   Like defun, but supports intent clauses after the optional docstring:
   - (:feature feature-name) - which feature this belongs to
   - (:role \"description\") - role within the feature
   - (:purpose \"description\") - why this function exists
   - (:failure-modes ((id \"desc\") ...)) - what can go wrong
   - (:goals ((id \"desc\") ...))
   - (:constraints ((id \"desc\") ...))
   - (:assumptions ((id \"desc\") ...))
   - (:verification ((id \"desc\") ...))"
  (let* ((docstring (when (stringp (car body)) (pop body)))
         (declarations nil))
    ;; Collect declarations
    (loop while (and body (consp (car body)) (eq 'declare (caar body)))
          do (push (pop body) declarations))
    (setf declarations (nreverse declarations))
    ;; Parse intent clauses
    (multiple-value-bind (intent-plist remaining-body)
        (parse-intent-clauses body)
      (let* ((feature (getf intent-plist :belongs-to))
             (intent-form (when intent-plist
                            `(setf (get ',name 'telos:intent)
                                   (make-intent
                                    ,@(loop for (k v) on intent-plist by #'cddr
                                            collect k
                                            collect (if (member k '(:role :purpose))
                                                        v
                                                        `',v)))))))
        `(progn
           (defun ,name ,lambda-list
             ,@(when docstring (list docstring))
             ,@declarations
             ,@remaining-body)
           ,@(when intent-form (list intent-form))
           ,@(when feature `((register-member ',feature ',name :function)))
           ',name)))))

(defmacro defintent (name &key feature role purpose failure-modes
                               goals constraints assumptions verification)
  "Add intent to an existing function, class, or method.

   NAME - symbol naming the function/class, or list for method specializers:
          (generic-name specializer1 specializer2 ...)
          where specializers are class-names or (eql value)
   FEATURE - which feature this belongs to
   ROLE - role within the feature
   PURPOSE - why this exists
   Other fields same as deffeature."
  (let ((intent-form `(make-intent :belongs-to ',feature
                                   :role ,role
                                   :purpose ,purpose
                                   :failure-modes ',failure-modes
                                   :goals ',goals
                                   :constraints ',constraints
                                   :assumptions ',assumptions
                                   :verification ',verification)))
    (if (consp name)
        ;; Method specializer: store in method registry
        `(progn
           (setf (gethash ',name *method-intent-registry*) ,intent-form)
           ,@(when feature `((register-member ',feature ',name :method)))
           ',name)
        ;; Symbol: existing behavior
        (let ((member-type `(if (find-class ',name nil) :class :function)))
          `(progn
             (setf (get ',name 'telos:intent) ,intent-form)
             ,@(when feature `((register-member ',feature ',name ,member-type)))
             ',name)))))

(defun method-intent (method-spec)
  "Get intent for a method specialization.
   METHOD-SPEC is a list like (generic-name specializer1 specializer2 ...)
   where specializers are class-names or (eql value)."
  (gethash method-spec *method-intent-registry*))

(defun get-intent (name)
  "Get intent for a function, class, or method.
   NAME can be a symbol (function/class) or list (method specializers).
   Checks: 1) method registry (if list), 2) symbol plist, 3) intentful-class metaclass, 4) class registry."
  (if (consp name)
      ;; Method specializer
      (method-intent name)
      ;; Symbol
      (or (get name 'telos:intent)
          (let ((class (find-class name nil)))
            (when (and class (typep class 'intentful-class))
              (class-intent class)))
          (gethash name *class-intent-registry*))))

(defun intent-feature (name)
  "Get which feature a function or class belongs to."
  (let ((intent (get-intent name)))
    (when intent
      (intent-belongs-to intent))))
