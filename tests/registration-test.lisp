(in-package :telos/tests)

(def-suite registration-tests :in :telos-tests)
(in-suite registration-tests)

;;; The programmatic registration API.
;;;
;;; Telos's macros are the ordinary way to declare intent, but they are not the
;;; only one: a library that records its own annotations (so that its fasls do
;;; not depend on telos being loaded) needs to replay them into telos at runtime.
;;; Before these were exported, the only way to do that was to reach into
;;; TELOS:: internals, which a :depends-on does not license — a rename would
;;; break the caller at load time with no warning at compile time.

(test registration-api-is-external
  "Every function needed to register intent programmatically is exported."
  (dolist (name '("REGISTER-FEATURE" "REGISTER-ENTITY-INTENT" "ENTITY-INTENT"
                  "REGISTER-MEMBER" "REPLACE-FEATURE-DECISIONS"
                  "CLASSIFY-SYMBOL-INTENT-TARGET"))
    (multiple-value-bind (symbol status) (find-symbol name :telos)
      (is (eq status :external)
          "TELOS:~A must be external, got ~S" name status)
      (is (fboundp symbol) "TELOS:~A must be fbound" name))))

(test register-feature-round-trips
  "A feature registered programmatically is queryable like a declared one."
  (register-feature 'reg-test-feature
                    (make-intent :purpose "Registered by hand"))
  (is (member 'reg-test-feature (list-features)))
  (is (string= "Registered by hand"
               (intent-purpose (feature-intent 'reg-test-feature)))))

(test register-entity-intent-round-trips-per-kind
  "Function, struct and condition intents are stored and read back by kind."
  (dolist (kind '(:function :struct :condition))
    (let ((name (intern (format nil "REG-TEST-ENTITY-~A" kind) :telos/tests)))
      (register-entity-intent kind name (make-intent :purpose "e"))
      (is (string= "e" (intent-purpose (entity-intent kind name)))
          "~S intent for ~S should read back" kind name))))

(test register-entity-intent-handles-classes
  "REGISTER-ENTITY-INTENT accepts :class, so a caller never has to know that
   telos keeps class intent in a separate registry."
  (register-entity-intent :class 'reg-test-class (make-intent :purpose "c"))
  (is (string= "c" (intent-purpose (entity-intent :class 'reg-test-class))))
  ;; and it is the SAME place get-intent looks for a retrofitted class
  (is (string= "c" (intent-purpose (get-intent 'reg-test-class)))))

(test register-member-populates-feature-members
  "Members registered programmatically show up under the right kind.

   FEATURE-MEMBERS treats the registry as a candidate index and filters it by
   each entity's own intent (see CURRENT-MEMBERS), so REGISTER-MEMBER alone is
   not enough — the entity's intent must also name the feature via :belongs-to.
   Registering the member without the intent yields an empty list, silently."
  (register-feature 'reg-member-feature (make-intent :purpose "p"))
  (register-entity-intent :function 'reg-member-fn
                          (make-intent :purpose "f" :belongs-to 'reg-member-feature))
  (register-entity-intent :condition 'reg-member-cond
                          (make-intent :purpose "c" :belongs-to 'reg-member-feature))
  (register-member 'reg-member-feature 'reg-member-fn :function)
  (register-member 'reg-member-feature 'reg-member-cond :condition)
  (let ((members (feature-members 'reg-member-feature)))
    (is (member 'reg-member-fn (getf members :functions)))
    (is (member 'reg-member-cond (getf members :conditions)))))

(test replace-feature-decisions-normalises-to-most-recent-first
  "REPLACE-FEATURE-DECISIONS takes decisions in SOURCE order and normalises them
   into the same representation RECORD-DECISION builds by pushing — so
   FEATURE-DECISIONS reads back most-recent-first, as its docstring says.
   A caller replaying a feature's decisions passes them in declaration order."
  (register-feature 'reg-decision-feature (make-intent :purpose "p"))
  (replace-feature-decisions
   'reg-decision-feature
   (list (make-decision :id :first :chose "a" :because "r1")
         (make-decision :id :second :chose "b" :because "r2")))
  (let ((decisions (feature-decisions 'reg-decision-feature)))
    (is (= 2 (length decisions)))
    (is (eq :second (decision-id (first decisions))))
    (is (eq :first (decision-id (second decisions))))))

(test classify-symbol-intent-target-is-usable-by-callers
  "The classifier telos uses to route a bare defintent is available to callers
   replaying annotations, so they file things exactly where telos would."
  (defclass reg-classify-class () ())
  (defun reg-classify-fn () nil)
  (is (eq :class (classify-symbol-intent-target 'reg-classify-class)))
  (is (eq :function (classify-symbol-intent-target 'reg-classify-fn)))
  ;; An unknown symbol is assumed to be a function rather than signalling —
  ;; retrofitting intent onto something not yet defined is legitimate.
  (is (eq :function (classify-symbol-intent-target 'reg-classify-nothing))))
