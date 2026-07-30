(in-package :telos)

;;; Query API
;;; intent-chain, feature-members, and related queries

;;; Track members of features (populated by defun/i, defclass/i, defstruct/i, define-condition/i, defintent)
(defvar *feature-members* (make-hash-table :test 'eq)
  "Maps feature-name to (:functions (list) :classes (list) :structs (list) :conditions (list) :methods (list))")

(defun register-member (feature-name name type)
  "Register NAME as a member of FEATURE-NAME with TYPE (:function, :class, :struct, :condition, or :method)"
  (when feature-name
    (let ((members (gethash feature-name *feature-members*)))
      (unless members
        (setf members (list :functions nil :classes nil :structs nil :conditions nil :methods nil))
        (setf (gethash feature-name *feature-members*) members))
      (case type
        (:function (pushnew name (getf members :functions)))
        (:class (pushnew name (getf members :classes)))
        (:struct (pushnew name (getf members :structs)))
        (:condition (pushnew name (getf members :conditions)))
        (:method (pushnew name (getf members :methods) :test #'equal))))))

(defun intent-chain (name)
  "Get full intent chain from an entity up to its root feature.
   NAME may be a symbol, a typed entity spec like (:class foo),
   or a method spec like (generic-name specializer ...)."
  (multiple-value-bind (intent entry-type)
      (get-intent-entry name)
    (when intent
      (let ((chain nil)
            (entry-name (if (and (consp name) (keywordp (car name)))
                            (second name)
                            name)))
        ;; First entry: the function/class itself
        (push (list :type entry-type
                    :name entry-name
                    :role (intent-role intent)
                    :purpose (intent-purpose intent)
                    :failure-modes (intent-failure-modes intent))
              chain)
        ;; Walk up the feature hierarchy. SEEN stops a cyclic :belongs-to —
        ;; without it the loop pushes an entry per iteration and exhausts the
        ;; heap. A cycle is a topology mistake to be reported by an audit, not a
        ;; reason for a query to take the image down.
        (let ((feature (intent-belongs-to intent))
              (seen nil))
          (loop while (and feature (not (member feature seen)))
                for feature-intent = (feature-intent feature)
                while feature-intent
                do (push feature seen)
                   (push (list :type :feature
                               :name feature
                               :purpose (intent-purpose feature-intent)
                               :failure-modes (intent-failure-modes feature-intent))
                         chain)
                   (setf feature (intent-belongs-to feature-intent))))
        (nreverse chain)))))

(defun member-intent (name type)
  "Current intent for a registered member NAME of member-list TYPE, or nil."
  (case type
    (:functions (entity-intent :function name))
    (:structs (entity-intent :struct name))
    (:conditions (entity-intent :condition name))
    (:methods (method-intent name))
    (:classes (or (let ((class (find-class name nil)))
                    (when (and class (typep class 'intentful-class))
                      (class-intent class)))
                  (gethash name *class-intent-registry*)))))

(defun current-members (feature-name members type)
  "Members of TYPE whose intent still says they belong to FEATURE-NAME.

   REGISTER-MEMBER only ever pushes, so re-declaring an entity under a second
   feature leaves it registered under the first as well. The registry is a
   candidate index; the entity's own intent is the truth, and it is consulted
   here rather than trying to un-register on redefinition — which could not see a
   later DEFINTENT re-pointing the entity anyway."
  (remove-if-not (lambda (name)
                   (let ((intent (member-intent name type)))
                     (and intent (eq feature-name (intent-belongs-to intent)))))
                 (getf members type)))

(defun feature-members (feature-name &optional type-filter)
  "Get members (functions, classes, structs, conditions, methods, sub-features) of a feature.

   TYPE-FILTER can be:
   - nil: return plist with all member types
   - :functions: return just the function list
   - :classes: return just the class list
   - :structs: return just the struct list
   - :conditions: return just the condition list
   - :methods: return just the method list (specializer specs)
   - :features: return just the sub-feature list"
  (let* ((members (gethash feature-name *feature-members*))
         (functions (current-members feature-name members :functions))
         (classes (current-members feature-name members :classes))
         (structs (current-members feature-name members :structs))
         (conditions (current-members feature-name members :conditions))
         (methods (current-members feature-name members :methods))
         (sub-features (feature-children feature-name)))
    (case type-filter
      (:functions functions)
      (:classes classes)
      (:structs structs)
      (:conditions conditions)
      (:methods methods)
      (:features sub-features)
      (otherwise (list :functions functions
                       :classes classes
                       :structs structs
                       :conditions conditions
                       :methods methods
                       :features sub-features)))))

(defun list-decisions (&optional feature-name)
  "List decisions. With FEATURE-NAME, return that feature's decisions.
   Without, return alist of (feature-name . decisions) for all features with decisions."
  (if feature-name
      (feature-decisions feature-name)
      (loop for name being the hash-keys of *decision-registry*
            for decisions = (gethash name *decision-registry*)
            when decisions collect (cons name decisions))))
