(in-package :telos)

;;; Query API
;;; intent-chain, feature-members, and related queries

;;; Track members of features (populated by defun/i, defclass/i, defstruct/i, defintent)
(defvar *feature-members* (make-hash-table :test 'eq)
  "Maps feature-name to (:functions (list) :classes (list) :structs (list))")

(defun register-member (feature-name name type)
  "Register NAME as a member of FEATURE-NAME with TYPE (:function, :class, or :struct)"
  (when feature-name
    (let ((members (gethash feature-name *feature-members*)))
      (unless members
        (setf members (list :functions nil :classes nil :structs nil))
        (setf (gethash feature-name *feature-members*) members))
      (case type
        (:function (pushnew name (getf members :functions)))
        (:class (pushnew name (getf members :classes)))
        (:struct (pushnew name (getf members :structs)))))))

(defun intent-chain (name)
  "Get full intent chain from function/class up to root feature.
   Returns list of plists with :type, :name, :purpose, :role, :failure-modes etc."
  (let ((intent (get-intent name)))
    (when intent
      (let ((chain nil)
            (entry-type (if (find-class name nil) :class :function)))
        ;; First entry: the function/class itself
        (push (list :type entry-type
                    :name name
                    :role (intent-role intent)
                    :purpose (intent-purpose intent)
                    :failure-modes (intent-failure-modes intent))
              chain)
        ;; Walk up the feature hierarchy
        (let ((feature (intent-belongs-to intent)))
          (loop while feature
                for feature-intent = (feature-intent feature)
                while feature-intent
                do (push (list :type :feature
                               :name feature
                               :purpose (intent-purpose feature-intent)
                               :failure-modes (intent-failure-modes feature-intent))
                         chain)
                   (setf feature (intent-belongs-to feature-intent))))
        (nreverse chain)))))

(defun feature-members (feature-name &optional type-filter)
  "Get members (functions, classes, structs, sub-features) of a feature.

   TYPE-FILTER can be:
   - nil: return plist (:functions (...) :classes (...) :structs (...) :features (...))
   - :functions: return just the function list
   - :classes: return just the class list
   - :structs: return just the struct list
   - :features: return just the sub-feature list"
  (let* ((members (gethash feature-name *feature-members*))
         (functions (getf members :functions))
         (classes (getf members :classes))
         (structs (getf members :structs))
         (sub-features (feature-children feature-name)))
    (case type-filter
      (:functions functions)
      (:classes classes)
      (:structs structs)
      (:features sub-features)
      (otherwise (list :functions functions
                       :classes classes
                       :structs structs
                       :features sub-features)))))
