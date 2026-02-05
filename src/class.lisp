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

(defun parse-class-intent-options (options)
  "Extract intent-related options from defclass options.
   Returns (values intent-plist remaining-options)."
  (let ((intent-plist nil)
        (remaining nil))
    (dolist (opt options)
      (case (car opt)
        (:feature (setf (getf intent-plist :belongs-to) (cadr opt)))
        (:role (setf (getf intent-plist :role) (cadr opt)))
        (:purpose (setf (getf intent-plist :purpose) (cadr opt)))
        (:failure-modes (setf (getf intent-plist :failure-modes) (cadr opt)))
        (:goals (setf (getf intent-plist :goals) (cadr opt)))
        (:constraints (setf (getf intent-plist :constraints) (cadr opt)))
        (:assumptions (setf (getf intent-plist :assumptions) (cadr opt)))
        (:verification (setf (getf intent-plist :verification) (cadr opt)))
        (otherwise (push opt remaining))))
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
      (parse-class-intent-options options)
    (let ((intent-form `(make-intent
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
         (find-class ',name)))))
