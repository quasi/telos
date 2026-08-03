(in-package :telos)

;;; Function intent
;;; defun/i macro and defintent for retrofitting

(defun classify-symbol-intent-target (name)
  "Classify NAME as a single intent-bearing entity kind.
   Returns one of :function or :class, or signals an error if ambiguous."
  (let ((functionp (fboundp name))
        (classp (not (null (find-class name nil)))))
    (cond
      ((and functionp classp)
       (error "Ambiguous intent target ~S names both a function and a class. Use (:function ~S) or (:class ~S)."
              name name name))
      (classp :class)
      (functionp :function)
      (t :function))))

(defun normalize-intent-target (name)
  "Normalize NAME into one of:
   - (:function symbol), (:class symbol), (:struct symbol), (:condition symbol)
   - (generic-name specializer...) for method intent
   - symbol for legacy unambiguous lookup."
  (if (and (consp name) (keywordp (car name)))
      (destructuring-bind (kind target) name
        (unless (member kind '(:function :class :struct :condition))
          (error "Unsupported intent target kind ~S." kind))
        (unless (symbolp target)
          (error "Intent target name must be a symbol, got ~S." target))
        (values kind target :entity))
      (if (consp name)
          (values nil name :method)
          (values nil name :symbol))))

(defun symbol-intent-candidates (name)
  "Collect all intent-bearing entity kinds currently associated with NAME."
  (remove nil
          (list (when (entity-intent :function name) :function)
                (when (entity-intent :struct name) :struct)
                (when (entity-intent :condition name) :condition)
                (when (let ((class (find-class name nil)))
                        (when (and class (typep class 'intentful-class))
                          (class-intent class)))
                  :class)
                (when (gethash name *class-intent-registry*) :class))
          :test #'eq))

(defun resolve-symbol-intent (name)
  "Resolve NAME to a unique intent and entity kind.
   Returns (values intent kind) or (values nil nil) if none exists."
  (let ((kinds (remove-duplicates (symbol-intent-candidates name) :test #'eq)))
    (cond
      ((null kinds) (values nil nil))
      ((cdr kinds)
       (error "Ambiguous intent query ~S matches multiple entity kinds: ~{~S~^, ~}. Use (:function ~S), (:class ~S), (:struct ~S), or (:condition ~S)."
              name kinds name name name name))
      (t
       (let ((kind (car kinds)))
         (values (case kind
                   (:function (entity-intent :function name))
                   (:struct (entity-intent :struct name))
                   (:condition (entity-intent :condition name))
                   (:class (or (let ((class (find-class name nil)))
                                 (when (and class (typep class 'intentful-class))
                                   (class-intent class)))
                               (gethash name *class-intent-registry*)))
                   (otherwise nil))
                 kind))))))

(defun parse-intent-clauses (body &optional context)
  "Parse intent clauses from the beginning of a function body.
   Returns (values intent-plist remaining-body).
   Intent clauses are forms like (:feature foo) (:role \"...\") etc.

   A keyword-headed form that is not a recognized clause signals
   INVALID-INTENT-DECLARATION: a keyword can never name a function or macro, so
   such a form is always a typo'd clause, and consuming it silently would drop
   both the intent and — since clauses are popped off the body — the form itself."
  (let ((intent-plist nil)
        (remaining body))
    (flet ((clause-value (clause) (intent-clause-value clause context)))
      (loop while (and remaining
                       (consp (car remaining))
                       (keywordp (caar remaining)))
            do (let ((clause (pop remaining)))
                 (case (car clause)
                   (:feature (setf (getf intent-plist :belongs-to) (clause-value clause)))
                   (:role (setf (getf intent-plist :role) (clause-value clause)))
                   (:purpose (setf (getf intent-plist :purpose) (clause-value clause)))
                   (:failure-modes
                    (setf (getf intent-plist :failure-modes) (clause-value clause)))
                   (:goals (setf (getf intent-plist :goals) (clause-value clause)))
                   (:constraints (setf (getf intent-plist :constraints) (clause-value clause)))
                   (:assumptions (setf (getf intent-plist :assumptions) (clause-value clause)))
                   (:verification (setf (getf intent-plist :verification) (clause-value clause)))
                   (otherwise (invalid-intent-clause clause context)))))
      (values intent-plist remaining))))

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
    (let ((context (declaration-context "DEFUN/I" name)))
      (multiple-value-bind (intent-plist remaining-body)
          (parse-intent-clauses body context)
        (validate-intent-fields intent-plist context)
        (let* ((feature (getf intent-plist :belongs-to))
               (intent-form (when intent-plist
                              `(register-entity-intent
                                :function
                                ',name
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
             ',name))))))

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
  (validate-intent-fields (list :failure-modes failure-modes
                                :goals goals
                                :constraints constraints
                                :assumptions assumptions
                                :verification verification)
                          (declaration-context "DEFINTENT" name))
  (let ((intent-form `(make-intent :belongs-to ',feature
                                   :role ,role
                                   :purpose ,purpose
                                   :failure-modes ',failure-modes
                                   :goals ',goals
                                   :constraints ',constraints
                                   :assumptions ',assumptions
                                   :verification ',verification)))
    (multiple-value-bind (kind target target-type)
        (normalize-intent-target name)
      (case target-type
        (:method
         `(progn
            (setf (gethash ',target *method-intent-registry*) ,intent-form)
            ,@(when feature `((register-member ',feature ',target :method)))
            ',target))
        ;; REGISTER-ENTITY-INTENT knows about :class now, so both branches below
        ;; are the same shape. They used to open-code the class registry twice.
        (:entity
         `(progn
            (register-entity-intent ,kind ',target ,intent-form)
            ,@(when feature `((register-member ',feature ',target ,kind)))
            ',target))
        (:symbol
         (let ((resolved-kind (classify-symbol-intent-target target)))
           `(progn
              (register-entity-intent ,resolved-kind ',target ,intent-form)
              ,@(when feature `((register-member ',feature ',target ,resolved-kind)))
              ',target)))))))

(defun method-intent (method-spec)
  "Get intent for a method specialization.
   METHOD-SPEC is a list like (generic-name specializer1 specializer2 ...)
   where specializers are class-names or (eql value)."
  (gethash method-spec *method-intent-registry*))

(defun get-intent (name)
  "Get intent for a function, class, or method.
   NAME can be a symbol (function/class) or list (method specializers).
   For ambiguous names that exist in multiple namespaces, use (:function foo),
   (:class foo), (:struct foo), or (:condition foo)."
  (multiple-value-bind (kind target target-type)
      (normalize-intent-target name)
    (case target-type
      (:method (method-intent target))
      (:entity
       (case kind
         (:function (entity-intent :function target))
         (:struct (entity-intent :struct target))
         (:condition (entity-intent :condition target))
         (:class (or (let ((class (find-class target nil)))
                       (when (and class (typep class 'intentful-class))
                         (class-intent class)))
                     (gethash target *class-intent-registry*)))))
      (:symbol
       (nth-value 0 (resolve-symbol-intent target))))))

(defun get-intent-entry (name)
  "Get intent plus entity kind for NAME.
   Returns (values intent kind) or (values nil nil) if not found."
  (multiple-value-bind (kind target target-type)
      (normalize-intent-target name)
    (case target-type
      (:method (values (method-intent target) :method))
      (:entity (values (get-intent name) kind))
      (:symbol (resolve-symbol-intent target)))))

(defun intent-feature (name)
  "Get which feature a function or class belongs to."
  (let ((intent (get-intent name)))
    (when intent
      (intent-belongs-to intent))))
