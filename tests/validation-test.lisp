(in-package :telos/tests)

(def-suite validation-tests :in :telos-tests)
(in-suite validation-tests)

;;; Nested plists in intent declarations must be as strict as the top level.
;;; A stray key used to be silently swallowed — the declaration then answered
;;; queries as though the field had never been written.

(defmacro signals-invalid (form)
  "Assert that macroexpanding FORM signals INVALID-INTENT-DECLARATION."
  `(signals invalid-intent-declaration (macroexpand-1 ',form)))

(defun invalid-declaration-report (form)
  "Return the report string of the condition signalled by expanding FORM."
  (handler-case (progn (macroexpand-1 form) nil)
    (invalid-intent-declaration (e) (princ-to-string e))))

;;; deffeature — decisions

(test deffeature-rejects-unknown-decision-key
  "A stray key in a decision plist is an error, not silence"
  (signals-invalid
   (deffeature probe-a :purpose "p"
     :decisions ((:id :d1 :chose "x" :over ("y") :because "z" :bogus-key "swallowed")))))

(test deffeature-decision-error-names-key-and-accepted-keys
  "The message names the offending key and lists the valid ones"
  (let ((report (invalid-declaration-report
                 '(deffeature probe-a2 :purpose "p"
                   :decisions ((:id :d1 :chose "x" :bogus-key "swallowed"))))))
    (is (not (null report)))
    (is (search "BOGUS-KEY" report))
    (is (search "DECIDED-BY" report))
    (is (search "PROBE-A2" report))))

(test deffeature-rejects-odd-length-decision-plist
  "A decision plist with a dangling key is an error"
  (signals-invalid
   (deffeature probe-a3 :purpose "p" :decisions ((:id :d1 :chose)))))

(test deffeature-rejects-non-list-decision
  "A decision must be a plist, not an atom"
  (signals-invalid
   (deffeature probe-a4 :purpose "p" :decisions (:d1))))

(test deffeature-rejects-non-keyword-decision-key
  "Decision plist keys must be keywords"
  (signals-invalid
   (deffeature probe-a5 :purpose "p" :decisions ((id :d1 :chose "x")))))

;;; deffeature — goals, constraints, assumptions, verification

(test deffeature-rejects-unknown-goal-key
  "Goal entries take no keyword options"
  (signals-invalid
   (deffeature probe-b :purpose "p" :goals ((:g1 "a goal" :bogus-key "swallowed")))))

(test deffeature-goal-error-mentions-field-and-shape
  "The message says which field and what shape it expects"
  (let ((report (invalid-declaration-report
                 '(deffeature probe-b2 :purpose "p"
                   :goals ((:g1 "a goal" :bogus-key "swallowed"))))))
    (is (not (null report)))
    (is (search "BOGUS-KEY" report))
    (is (search "GOALS" report))))

(test deffeature-rejects-unknown-constraint-key
  (signals-invalid
   (deffeature probe-b3 :purpose "p" :constraints ((:c1 "a constraint" :bogus-key "x")))))

(test deffeature-rejects-unknown-assumption-key
  (signals-invalid
   (deffeature probe-b4 :purpose "p" :assumptions ((:a1 "an assumption" :bogus-key "x")))))

(test deffeature-rejects-unknown-verification-key
  (signals-invalid
   (deffeature probe-b5 :purpose "p" :verification ((:v1 "a check" :bogus-key "x")))))

(test deffeature-rejects-violates-on-a-goal
  ":violates belongs to failure modes, not goals"
  (signals-invalid
   (deffeature probe-b6 :purpose "p" :goals ((:g1 "a goal" :violates :other)))))

;;; deffeature — failure modes

(test deffeature-rejects-unknown-failure-mode-key
  (signals-invalid
   (deffeature probe-c :purpose "p" :failure-modes ((:fm1 "a failure" :bogus-key "swallowed")))))

(test deffeature-failure-mode-error-lists-violates
  "Failure modes accept :violates, and the message says so"
  (let ((report (invalid-declaration-report
                 '(deffeature probe-c2 :purpose "p"
                   :failure-modes ((:fm1 "a failure" :bogus-key "x"))))))
    (is (not (null report)))
    (is (search "VIOLATES" report))))

(test deffeature-rejects-non-list-entry
  "Entries must be lists, not bare atoms"
  (signals-invalid
   (deffeature probe-c3 :purpose "p" :goals (:g1))))

(test deffeature-rejects-dotted-entry
  "A dotted entry is reported as a declaration error, not a raw type error"
  (signals-invalid
   (deffeature probe-c4 :purpose "p" :goals ((:g1 . "a goal")))))

(test deffeature-rejects-dotted-decision
  (signals-invalid
   (deffeature probe-c5 :purpose "p" :decisions ((:id . :d1)))))

;;; Valid declarations must keep working

(test deffeature-accepts-valid-nested-plists
  "Everything the documented shapes allow still expands and registers"
  (finishes
   (eval '(deffeature validation-ok-feature
           :purpose "Valid on every axis"
           :goals ((:g1 "Goal one") (:g2 "Goal two"))
           :constraints ((:c1 "Constraint one"))
           :assumptions ((:a1 "Assumption one"))
           :failure-modes ((:f1 "Failure one" :violates :g1))
           :verification ((:v1 "Verify one"))
           :decisions ((:id :d1 :chose "A" :over ("B" "C") :because "reasons"
                        :date "2026-07-30" :decided-by "Baba")))))
  (let ((intent (feature-intent 'validation-ok-feature)))
    (is (= 2 (length (intent-goals intent))))
    (is (= 1 (length (feature-decisions 'validation-ok-feature))))))

(test deffeature-accepts-id-only-entries
  "An id-only entry is fine — the description is what may be omitted, not its place"
  (finishes
   (eval '(deffeature validation-terse-feature
           :purpose "Terse entries"
           :goals ((:g1))))))

(test deffeature-rejects-keyword-where-description-belongs
  "(:f1 :violates :g1) is ambiguous: an option to us, a description to consumers"
  (signals-invalid
   (deffeature probe-e :purpose "p" :failure-modes ((:f1 :violates :g1)))))

;;; Duplicate keys are the same silent loss, one layer in

(test deffeature-rejects-duplicate-entry-option
  "A repeated option would be dropped by getf — say so instead"
  (signals-invalid
   (deffeature probe-f :purpose "p"
     :failure-modes ((:f1 "d" :violates :a :violates :b)))))

(test deffeature-rejects-duplicate-decision-key
  (signals-invalid
   (deffeature probe-f2 :purpose "p"
     :decisions ((:id :d1 :because "first" :because "second")))))

(test duplicate-key-error-names-the-key
  (let ((report (invalid-declaration-report
                 '(deffeature probe-f3 :purpose "p"
                   :decisions ((:id :d1 :because "a" :because "b"))))))
    (is (not (null report)))
    (is (search "duplicate" report))
    (is (search "BECAUSE" report))))

;;; Whole-field shape errors point at the field, not at a phantom entry

(test deffeature-rejects-non-list-field-value
  (signals-invalid
   (deffeature probe-g :purpose "p" :goals "not a list")))

(test field-shape-error-names-the-field
  (let ((report (invalid-declaration-report
                 '(deffeature probe-g2 :purpose "p" :goals "not a list"))))
    (is (not (null report)))
    (is (search "GOALS" report))
    (is (search "list of entries" report))))

;;; Top-level clauses: the macros that read clauses from a body must be strict too

(test defun-i-rejects-unknown-top-level-clause
  "A typo'd clause used to be swallowed — along with the intent it carried"
  (signals-invalid
   (defun/i validation-probe-fn-2 ()
     (:purpose "p")
     (:failure-mode ((:fm1 "typo in the clause name")))
     nil)))

(test defun-i-does-not-eat-body-forms
  "A keyword-headed body form was consumed as a clause, silently emptying the body"
  (signals-invalid
   (defun/i validation-probe-fn-3 (x)
     (:purpose "p")
     (:some-plist-literal x))))

(test defun-i-clause-error-lists-valid-clauses
  (let ((report (invalid-declaration-report
                 '(defun/i validation-probe-fn-4 ()
                   (:purpose "p")
                   (:failure-mode ((:fm1 "typo")))
                   nil))))
    (is (not (null report)))
    (is (search "FAILURE-MODE;" report))
    (is (search "FAILURE-MODES" report))
    (is (search "VALIDATION-PROBE-FN-4" report))))

(test defstruct-i-rejects-unknown-top-level-clause
  (signals-invalid
   (defstruct/i validation-probe-struct-2
     slot-a
     (:purpose "p")
     (:goal ((:g1 "typo in the clause name"))))))

(test defclass-i-rejects-unknown-top-level-clause
  "Previously forwarded to defclass and surfaced as an initarg error at load time"
  (signals-invalid
   (defclass/i validation-probe-class-2 ()
     ()
     (:purpose "p")
     (:goal ((:g1 "typo"))))))

(test define-condition-i-rejects-unknown-top-level-clause
  (signals-invalid
   (define-condition/i validation-probe-condition-2 (error)
     ()
     (:purpose "p")
     (:goal ((:g1 "typo"))))))

(test defclass-i-still-forwards-standard-options
  "Genuine defclass options are not mistaken for typo'd intent clauses"
  (finishes
   (eval '(defclass/i validation-ok-class ()
           ((a :initarg :a :initform nil))
           (:purpose "Valid class")
           (:documentation "A docstring")
           (:default-initargs :a 1))))
  (is (string= "Valid class" (intent-purpose (class-intent 'validation-ok-class)))))

(test define-condition-i-still-forwards-standard-options
  (finishes
   (eval '(define-condition/i validation-ok-condition (error)
           ()
           (:purpose "Valid condition")
           (:documentation "A docstring")
           (:report (lambda (c s) (declare (ignore c)) (format s "boom")))))))

(test defclass-i-still-rejects-metaclass-with-its-own-message
  "The dedicated :metaclass error is not swallowed by the new clause check"
  (handler-case
      (progn (macroexpand-1 '(defclass/i validation-probe-class-3 ()
                              ()
                              (:metaclass standard-class)))
             (fail "expected an error"))
    (invalid-intent-declaration ()
      (fail "should be the dedicated :METACLASS error, not a clause error"))
    (error (e)
      (is (search "METACLASS" (princ-to-string e))))))

;;; Sibling macros parse the same nested plists

(test defun-i-rejects-unknown-nested-key
  (signals-invalid
   (defun/i validation-probe-fn ()
     (:purpose "p")
     (:failure-modes ((:fm1 "a failure" :bogus-key "x")))
     nil)))

(test defintent-rejects-unknown-nested-key
  (signals-invalid
   (defintent validation-probe-target
     :purpose "p"
     :goals ((:g1 "a goal" :bogus-key "x")))))

(test defclass-i-rejects-unknown-nested-key
  (signals-invalid
   (defclass/i validation-probe-class ()
     ()
     (:purpose "p")
     (:goals ((:g1 "a goal" :bogus-key "x"))))))

(test defstruct-i-rejects-unknown-nested-key
  (signals-invalid
   (defstruct/i validation-probe-struct
     slot-a
     (:purpose "p")
     (:constraints ((:c1 "a constraint" :bogus-key "x"))))))

(test define-condition-i-rejects-unknown-nested-key
  (signals-invalid
   (define-condition/i validation-probe-condition (error)
     ()
     (:purpose "p")
     (:assumptions ((:a1 "an assumption" :bogus-key "x"))))))

;;; A clause carries exactly one value — extra forms used to fall on the floor

(test defun-i-rejects-extra-forms-in-a-clause
  "(:goals (...) (...)) kept the first list and silently dropped the second"
  (signals-invalid
   (defun/i validation-probe-fn-5 ()
     (:goals ((:g1 "kept")) ((:g2 "dropped" :cause "bogus")))
     nil)))

(test defstruct-i-rejects-extra-forms-in-a-clause
  (signals-invalid
   (defstruct/i validation-probe-struct-3
     slot-a
     (:goals ((:g1 "kept")) ((:g2 "dropped"))))))

(test defclass-i-rejects-extra-forms-in-a-clause
  (signals-invalid
   (defclass/i validation-probe-class-4 ()
     ()
     (:goals ((:g1 "kept")) ((:g2 "dropped"))))))

(test clause-arity-error-names-the-clause
  (let ((report (invalid-declaration-report
                 '(defun/i validation-probe-fn-6 ()
                   (:goals ((:g1 "kept")) ((:g2 "dropped")))
                   nil))))
    (is (not (null report)))
    (is (search "GOALS" report))
    (is (search "exactly one value" report))))

(test valueless-clause-reports-a-missing-value-not-a-dropped-one
  "(:purpose) has nothing after it to drop — the value is simply missing"
  (let ((report (invalid-declaration-report
                 '(defun/i validation-probe-fn-7 () (:purpose) nil))))
    (is (not (null report)))
    (is (search "needs a value" report))
    (is (not (search "dropped" report)))))

(test defclass-i-still-allows-multi-value-standard-options
  ":default-initargs legitimately takes many values — the arity rule is intent-only"
  (finishes
   (eval '(defclass/i validation-ok-class-2 ()
           ((a :initarg :a) (b :initarg :b))
           (:purpose "Multi-valued standard option")
           (:default-initargs :a 1 :b 2)))))

;;; Field values are never evaluated — say so instead of blaming an entry named QUOTE

(test deffeature-rejects-quoted-field-value
  "':goals '((:g1 \"d\"))' used to report an entry named QUOTE"
  (signals-invalid
   (deffeature probe-h :purpose "p" :goals '((:g1 "a goal")))))

(test unevaluated-form-error-explains-itself
  (let ((report (invalid-declaration-report
                 '(deffeature probe-h2 :purpose "p" :goals (list '(:g1 "a goal"))))))
    (is (not (null report)))
    (is (search "never evaluated" report))
    (is (not (search "entry LIST" report)))))

(test deffeature-rejects-quoted-decisions-value
  (signals-invalid
   (deffeature probe-h3 :purpose "p" :decisions '((:id :d1 :chose "x")))))

;;; The condition is programmatically inspectable, not just a message

(test invalid-declaration-exposes-key-and-field
  "Handlers can read the offending key, field, and expected keys"
  (handler-case
      (progn (macroexpand-1 '(deffeature probe-d :purpose "p"
                              :failure-modes ((:fm1 "f" :bogus-key "x"))))
             (fail "expected invalid-intent-declaration"))
    (invalid-intent-declaration (e)
      (is (eq :bogus-key (invalid-intent-declaration-key e)))
      (is (eq :failure-modes (invalid-intent-declaration-field e)))
      (is (equal '(:violates) (invalid-intent-declaration-expected e)))
      (is (eq :unknown-key (invalid-intent-declaration-reason e))))))

(test invalid-declaration-is-an-error
  "It is an error, matching the top level's strictness"
  (is (subtypep 'invalid-intent-declaration 'error)))

;;; Decision values are literal data, and their types are checked here rather
;;; than by MAKE-DECISION at load time

(test deffeature-rejects-computed-decision-value
  "A computed :chose used to be evaluated, unlike :over — an asymmetry that made
   :over (list ...) store an unevaluated form"
  (signals-invalid
   (deffeature probe-i :purpose "p"
     :decisions ((:id :d1 :chose (concatenate 'string "signed " "cookies"))))))

(test deffeature-rejects-computed-over
  (signals-invalid
   (deffeature probe-i2 :purpose "p" :decisions ((:id :d1 :over (list "a" "b"))))))

(test decision-value-error-points-at-record-decision
  "The error names the field, the expected type, and where computed values belong"
  (let ((report (invalid-declaration-report
                 '(deffeature probe-i3 :purpose "p"
                   :decisions ((:id :d1 :chose (compute-it)))))))
    (is (not (null report)))
    (is (search "CHOSE" report))
    (is (search "string" report))
    (is (search "RECORD-DECISION" report))))

(test deffeature-rejects-non-keyword-decision-id
  (signals-invalid
   (deffeature probe-i4 :purpose "p" :decisions ((:id "not-a-keyword" :chose "x")))))

(test deffeature-rejects-non-string-in-over
  ":over is a list of the alternatives that were rejected, as strings"
  (signals-invalid
   (deffeature probe-i5 :purpose "p" :decisions ((:id :d1 :over ("a" 42))))))

(test deffeature-rejects-non-list-over
  (signals-invalid
   (deffeature probe-i6 :purpose "p" :decisions ((:id :d1 :over "just-a-string")))))

(test deffeature-stores-decision-values-literally
  "Every decision field is literal data — no field is evaluated"
  (eval '(deffeature validation-literal-decisions
          :purpose "Literal decision values"
          :decisions ((:id :d1 :chose "signed cookies"
                       :over ("server-side sessions" "JWT")
                       :because "Stateless" :date "2026-07-30" :decided-by "Baba"))))
  (let ((d (first (feature-decisions 'validation-literal-decisions))))
    (is (eq :d1 (decision-id d)))
    (is (string= "signed cookies" (decision-chose d)))
    (is (equal '("server-side sessions" "JWT") (decision-over d)))
    (is (string= "Baba" (decision-decided-by d)))))

(test deffeature-accepts-nil-decision-fields
  "Every field but the id is optional"
  (finishes
   (eval '(deffeature validation-terse-decisions
           :purpose "p"
           :decisions ((:id :d1) (:id :d2 :chose "x"))))))

;;; Form detection needs no allowlist: a symbol can never head a list of entries

(test deffeature-detects-any-computed-field-value
  "mapcar/remove/concatenate used to fall through and blame an entry named MAPCAR"
  (let ((report (invalid-declaration-report
                 '(deffeature probe-j :purpose "p" :goals (mapcar #'identity nil)))))
    (is (not (null report)))
    (is (search "never evaluated" report))
    (is (not (search "entry MAPCAR" report)))))

(test deffeature-detects-computed-decisions-collection
  (let ((report (invalid-declaration-report
                 '(deffeature probe-j2 :purpose "p" :decisions (remove nil nil)))))
    (is (not (null report)))
    (is (search "never evaluated" report))))
