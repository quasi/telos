(in-package :telos)

;;; Class intent
;;; Metaclass for classes with intent, and defclass/i macro

(defclass intentful-class (standard-class)
  ((intent :accessor class-intent :initarg :intent :initform nil))
  (:documentation "Metaclass for classes that carry intent information."))

;; Required for SBCL/closer-mop: validate that intentful-class can be used
;; as metaclass for classes with standard-class superclasses
(defmethod c2mop:validate-superclass ((class intentful-class)
                                       (superclass standard-class))
  t)

(defmethod class-intent ((name symbol))
  "Get intent for a class by name (symbol convenience method)."
  (let ((class (find-class name nil)))
    (when (and class (typep class 'intentful-class))
      (slot-value class 'intent))))

(defvar *intentful-class-names* (make-hash-table :test 'eq)
  "Candidate index of class names defined with INTENTFUL-CLASS.

   Never treated as truth: DEFCLASS/I stores intent on the class metaobject, so
   nothing else in the image can enumerate those classes, but an index that is
   only ever added to goes stale — the same disease that made FEATURE-MEMBERS
   report an entity under a feature it had left. Readers re-derive through
   FIND-CLASS, so a stale name simply drops out.")

(defmethod shared-initialize :after ((class intentful-class) slot-names &rest initargs)
  "Record the class name so the image can be swept for intentful classes."
  (declare (ignore slot-names))
  (let ((name (or (getf initargs :name) (class-name class))))
    (when (symbolp name)
      (setf (gethash name *intentful-class-names*) t))))

(defun all-intentful-classes ()
  "Names of classes currently defined with the INTENTFUL-CLASS metaclass.
   Each candidate is re-derived through FIND-CLASS, so a class redefined with
   another metaclass, or a name that never became a class, is dropped here rather
   than reported as a phantom."
  (loop for name being the hash-keys of *intentful-class-names*
        for class = (find-class name nil)
        when (and class (typep class 'intentful-class))
          collect name))

(defparameter *defclass-passthrough-options*
  '(:default-initargs :documentation :metaclass)
  "Standard DEFCLASS options DEFCLASS/I forwards untouched. :METACLASS gets its own
   dedicated error in DEFCLASS/I, so it is listed here rather than rejected as a typo.")

(defparameter *define-condition-passthrough-options*
  '(:default-initargs :documentation :report)
  "Standard DEFINE-CONDITION options DEFINE-CONDITION/I forwards untouched.")

(defun parse-class-intent-options (options &optional context passthrough)
  "Extract intent-related options from defclass/define-condition options.
   Returns (values intent-plist remaining-options).

   With CONTEXT, an option that is neither an intent clause nor one of PASSTHROUGH
   signals INVALID-INTENT-DECLARATION here, at macroexpansion time. Without this a
   misspelled intent clause is forwarded to DEFCLASS, which reports it — if at all —
   as an unrelated initarg error at load time."
  (let ((intent-plist nil)
        (remaining nil))
    (flet ((clause-value (opt) (intent-clause-value opt context)))
      (dolist (opt options)
        (case (and (consp opt) (car opt))
          (:feature (setf (getf intent-plist :belongs-to) (clause-value opt)))
          (:role (setf (getf intent-plist :role) (clause-value opt)))
          (:purpose (setf (getf intent-plist :purpose) (clause-value opt)))
          (:failure-modes (setf (getf intent-plist :failure-modes) (clause-value opt)))
          (:goals (setf (getf intent-plist :goals) (clause-value opt)))
          (:constraints (setf (getf intent-plist :constraints) (clause-value opt)))
          (:assumptions (setf (getf intent-plist :assumptions) (clause-value opt)))
          (:verification (setf (getf intent-plist :verification) (clause-value opt)))
          (otherwise
           (when (and context (consp opt) (not (member (car opt) passthrough)))
             (invalid-intent-clause opt context (append *intent-clause-keys* passthrough)))
           (push opt remaining)))))
    (values intent-plist (nreverse remaining))))

(defmacro defclass/i (name superclasses slots &rest options)
  "Define a class with intent.

   Like defclass, but uses intentful-class as metaclass and supports:
   - (:feature feature-name) - which feature this belongs to
   - (:role \"description\") - role within the feature
   - (:purpose \"description\") - why this class exists
   - (:failure-modes ((id \"desc\") ...))
   - (:goals ((id \"desc\") ...))
   - (:constraints ((id \"desc\") ...))
   - (:assumptions ((id \"desc\") ...))
   - (:verification ((id \"desc\") ...))"
  (multiple-value-bind (intent-plist remaining-options)
      (parse-class-intent-options options
                                  (declaration-context "DEFCLASS/I" name)
                                  *defclass-passthrough-options*)
    (validate-intent-fields intent-plist (declaration-context "DEFCLASS/I" name))
    (when (assoc :metaclass remaining-options)
      (error "DEFCLASS/I does not accept :METACLASS. It always uses INTENTFUL-CLASS for ~S." name))
    (let* ((feature (getf intent-plist :belongs-to))
           (intent-form `(make-intent
                          ,@(loop for (k v) on intent-plist by #'cddr
                                  collect k
                                  collect (if (member k '(:role :purpose))
                                              v
                                              `',v)))))
      `(progn
         (defclass ,name ,superclasses
           ,slots
           (:metaclass intentful-class)
           ,@remaining-options)
         ;; Set intent after class is created (defclass doesn't eval option values)
         (setf (class-intent (find-class ',name)) ,intent-form)
         ,@(when feature `((register-member ',feature ',name :class)))
         (find-class ',name)))))
