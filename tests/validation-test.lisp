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
      ;; The accepted keys, not a fixed list of them — DEFINE-ENTRY-OPTION can widen it.
      (is (member :violates (invalid-intent-declaration-expected e)))
      (is (member :mitigation (invalid-intent-declaration-expected e)))
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

;;; Duplicate entry ids within one field
;;;
;;; Two goals with the same id make :violates ambiguous and hide one description.

(test deffeature-rejects-duplicate-goal-ids
  (signals-invalid
   (deffeature probe-k :purpose "p" :goals ((:g1 "first") (:g1 "second")))))

(test deffeature-rejects-duplicate-failure-mode-ids
  (signals-invalid
   (deffeature probe-k2 :purpose "p"
     :failure-modes ((:fm1 "first" :violates :g) (:fm1 "second" :violates :g)))))

(test duplicate-entry-id-error-names-the-id
  (let ((report (invalid-declaration-report
                 '(deffeature probe-k3 :purpose "p" :goals ((:g1 "first") (:g1 "second"))))))
    (is (not (null report)))
    (is (search "G1" report))
    (is (search "GOALS" report))))

(test deffeature-allows-the-same-id-in-different-fields
  "A goal :g1 and a failure mode :g1 are different things; only within a field is
   a repeat ambiguous"
  (finishes
   (eval '(deffeature validation-same-id-feature
           :purpose "p"
           :goals ((:g1 "A goal"))
           :constraints ((:g1 "A constraint that happens to share the id"))))))

(test defun-i-rejects-duplicate-entry-ids
  (signals-invalid
   (defun/i validation-probe-fn-8 ()
     (:goals ((:g1 "first") (:g1 "second")))
     nil)))

;;; :mitigation on a failure mode
;;;
;;; The vocabulary was too small, not too loose: :mitigation says how to recover
;;; from the failure, which is the field an agent reads when it is the one
;;; recovering. It was rejected because nothing had put it in the table.

(test failure-modes-accept-mitigation
  (finishes
   (eval '(deffeature validation-mitigation-feature
           :purpose "p"
           :failure-modes ((:fm1 "A response arrives late" :mitigation "Check expires-at"))))))

(test failure-modes-accept-violates-and-mitigation-together
  (finishes
   (eval '(deffeature validation-both-options-feature
           :purpose "p"
           :goals ((:g1 "A goal"))
           :failure-modes ((:fm1 "Breaks it" :violates :g1 :mitigation "Retry"))))))

(test mitigation-survives-into-the-stored-intent
  "Accepting the key is worth nothing if the value is not retrievable"
  (eval '(deffeature validation-mitigation-stored
          :purpose "p"
          :failure-modes ((:fm1 "Late response" :mitigation "Check expires-at"))))
  (let ((mode (first (intent-failure-modes (feature-intent 'validation-mitigation-stored)))))
    (is (string= "Check expires-at" (intent-entry-option mode :mitigation)))))

(test misspelled-mitigation-is-still-rejected
  "The point of the vocabulary is that a typo in it is still caught"
  (let ((report (invalid-declaration-report
                 '(deffeature probe-m1 :purpose "p"
                   :failure-modes ((:fm1 "d" :mitigaton "typo"))))))
    (is (not (null report)))
    (is (search "MITIGATON" report))
    (is (search "MITIGATION" report))
    (is (search "VIOLATES" report))))

(test goals-still-accept-no-options
  "Widening :failure-modes must not widen every field"
  (signals-invalid
   (deffeature probe-m2 :purpose "p" :goals ((:g1 "d" :mitigation "nope")))))

;;; Entry accessors
;;;
;;; An entry is (id) or (id description . options). Consumers had to know that
;;; shape and walk the list themselves.

(test entry-accessors-read-a-full-entry
  (let ((entry '(:fm1 "A description" :violates :g1 :mitigation "Retry")))
    (is (eq :fm1 (intent-entry-id entry)))
    (is (string= "A description" (intent-entry-description entry)))
    (is (eq :g1 (intent-entry-option entry :violates)))
    (is (string= "Retry" (intent-entry-option entry :mitigation)))))

(test entry-accessors-tolerate-a-bare-entry
  "(:id) with no description is a legal entry"
  (let ((entry '(:g1)))
    (is (eq :g1 (intent-entry-id entry)))
    (is (null (intent-entry-description entry)))
    (is (null (intent-entry-option entry :violates)))))

(test entry-accessors-tolerate-a-non-entry
  "Accessors are read-side; they report absence rather than signalling"
  (is (null (intent-entry-id nil)))
  (is (null (intent-entry-description nil)))
  (is (null (intent-entry-option nil :violates))))

(test entry-accessors-tolerate-a-malformed-entry
  "The macros reject these shapes, but MAKE-INTENT is exported and validates
   nothing, and CHECK-INTENT-REFERENCES sweeps every intent in the image — one
   bad entry must not take the whole audit down with a GETF type error."
  (dolist (entry '((:fm1 "d" :violates)       ; odd option tail
                   (:fm1 :violates :g1)       ; keyword where the description goes
                   (:a "d" . :b)              ; dotted option tail
                   (:a . :b)                  ; dotted entry
                   :not-a-list))
    (finishes (intent-entry-id entry))
    (finishes (intent-entry-description entry))
    (is (null (intent-entry-option entry :violates)))))

(test entry-option-terminates-on-a-circular-option-tail
  "A guard that loops is worse than the type error it replaced: the audit does
   not fail, it wedges. The timeout is the assertion — without it a regression
   hangs the suite instead of failing it."
  (let* ((tail (list :v 1 :w 2))
         (entry (list* :a "d" tail)))
    (setf (cdr (last tail)) tail)
    #+sbcl
    (finishes
     (sb-ext:with-timeout 5
       (is (null (intent-entry-option entry :v)))))
    #-sbcl
    (is (null (intent-entry-option entry :v)))))

(test audit-survives-a-malformed-entry-from-make-intent
  "The reachable path: MAKE-INTENT takes anything, and the audit walks it"
  (setf (gethash 'validation-malformed-feature telos::*feature-registry*)
        (make-intent :purpose "p" :failure-modes (list (list :fm1 "d" :violates))))
  (unwind-protect
       (finishes (check-intent-references))
    (remhash 'validation-malformed-feature telos::*feature-registry*)))

;;; Extending the vocabulary
;;;
;;; A project with its own constitution should not need a telos release to name
;;; a field telos never thought of.

(defmacro with-clean-vocabulary (&body body)
  "Run BODY with *INTENT-ENTRY-OPTION-KEYS* restored afterwards.
   Without this a test that widens a field leaves every later test — and anyone
   continuing in the same image — with a vocabulary a fresh load would not have."
  (let ((saved (gensym "SAVED-VOCABULARY")))
    `(let ((,saved (mapcar #'copy-list telos::*intent-entry-option-keys*)))
       (unwind-protect (progn ,@body)
         (setf telos::*intent-entry-option-keys* ,saved)))))

(test define-entry-option-widens-a-field
  (with-clean-vocabulary
    (eval '(define-entry-option :goals :validation-owner))
    (finishes
     (eval '(deffeature validation-extended-feature
             :purpose "p"
             :goals ((:g1 "A goal" :validation-owner "quasi")))))
    ;; and the rest of the vocabulary is unchanged
    (signals-invalid
     (deffeature probe-m3 :purpose "p" :goals ((:g1 "d" :still-unknown "no"))))))

(test define-entry-option-does-not-outlive-the-test-that-set-it
  "The fixture itself must work, or every later test runs on a polluted table"
  (with-clean-vocabulary
    (eval '(define-entry-option :goals :validation-scoped)))
  (signals-invalid
   (deffeature probe-m8 :purpose "p" :goals ((:g1 "d" :validation-scoped "gone")))))

(test define-entry-option-is-idempotent
  "Asserted through behaviour and through the message, not the table's internals"
  (with-clean-vocabulary
    (eval '(define-entry-option :goals :validation-owner))
    (eval '(define-entry-option :goals :validation-owner))
    (let ((report (invalid-declaration-report
                   '(deffeature probe-m9 :purpose "p" :goals ((:g1 "d" :nope "x"))))))
      ;; listed once in "expected one of ...", not twice
      (is (= 1 (loop with start = 0
                     for pos = (search "VALIDATION-OWNER" report :start2 start)
                     while pos count 1 do (setf start (1+ pos))))))))

(test define-entry-option-returns-its-keys
  (with-clean-vocabulary
    (is (equal '(:validation-a :validation-b)
               (eval '(define-entry-option :goals :validation-a :validation-b))))))

(test define-entry-option-keeps-built-in-options-first
  "A project's additions come last, so the message reads the same everywhere"
  (with-clean-vocabulary
    (eval '(define-entry-option :failure-modes :validation-late))
    (let ((report (invalid-declaration-report
                   '(deffeature probe-m10 :purpose "p"
                     :failure-modes ((:fm1 "d" :nope "x"))))))
      (is (< (search "VIOLATES" report) (search "VALIDATION-LATE" report))))))

(test define-entry-option-rejects-an-unknown-field
  "A typo'd field would add a key nothing ever consults"
  (signals invalid-intent-declaration
    (eval '(define-entry-option :failure-mode :mitigation))))

(test define-entry-option-rejects-an-unknown-field-with-no-keys
  "The field is a typo whether or not anything follows it"
  (signals invalid-intent-declaration
    (eval '(define-entry-option :failure-mode))))

(test define-entry-option-rejects-a-non-keyword
  (signals invalid-intent-declaration
    (eval '(define-entry-option :goals "owner"))))

(test add-entry-option-is-usable-directly
  "The function under the macro, for a computed field and key"
  (with-clean-vocabulary
    (is (eq :validation-computed (add-entry-option :goals :validation-computed)))
    (finishes
     (eval '(deffeature validation-computed-feature
             :purpose "p"
             :goals ((:g1 "d" :validation-computed "yes")))))))

(defun compile-source-failed-p (text)
  "Compile TEXT as a file and return COMPILE-FILE's FAILURE-P.

   COMPILE-FILE does not let a macroexpansion error escape — it traps it, returns
   a fasl, and reports the failure in its third value. A test that only asserts
   the call FINISHES therefore passes whether or not the code compiles."
  (uiop:with-temporary-file (:pathname source :type "lisp" :direction :output
                             :stream out :keep nil)
    (write-string text out)
    (finish-output out)
    ;; The negative case is *meant* to fail to compile; its diagnostics would
    ;; otherwise print into the test run and read like a broken suite.
    (let* ((sink (make-broadcast-stream))
           (*error-output* sink)
           (*standard-output* sink))
      (multiple-value-bind (fasl warnings-p failure-p)
          ;; :OVERRIDE T keeps the deliberate failure out of the enclosing
          ;; compilation unit, whose summary would otherwise announce a caught
          ;; ERROR at the end of an all-green run.
          (with-compilation-unit (:override t)
            (handler-bind ((warning #'muffle-warning))
              (compile-file source :verbose nil :print nil)))
        (declare (ignore warnings-p))
        (when fasl (ignore-errors (delete-file fasl)))
        failure-p))))

(defparameter +compiled-widening+
  "(in-package :telos/tests)
(define-entry-option :goals :validation-compiled)
(deffeature validation-compiled-feature
  :purpose \"p\"
  :goals ((:g1 \"A goal\" :validation-compiled \"set at compile time\")))
")

(defparameter +compiled-without-widening+
  "(in-package :telos/tests)
(deffeature validation-uncompiled-feature
  :purpose \"p\"
  :goals ((:g1 \"A goal\" :validation-compiled \"never widened\")))
")

(test define-entry-option-takes-effect-at-compile-time
  "The whole point of the EVAL-WHEN: a declaration later in the same file, under
   COMPILE-FILE, must see the widened vocabulary. Every other test here goes
   through EVAL, which only ever reaches the :EXECUTE situation.

   The negative case is half the test. Without it this passes even if
   DEFINE-ENTRY-OPTION does nothing at all, because COMPILE-FILE swallows the
   error either way."
  (with-clean-vocabulary
    (is (null (compile-source-failed-p +compiled-widening+))))
  (with-clean-vocabulary
    (is (compile-source-failed-p +compiled-without-widening+))))
