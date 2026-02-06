(in-package :telos/tests)

(def-suite decision-tests :in :telos-tests)
(in-suite decision-tests)

;;; Struct tests

(test make-decision-all-fields
  "make-decision populates all 6 fields"
  (let ((d (make-decision :id :db-choice
                          :chose "SQLite"
                          :over '("PostgreSQL" "DynamoDB")
                          :because "Embedded, zero-config"
                          :date "2026-02-06"
                          :decided-by "Baba")))
    (is (eq :db-choice (decision-id d)))
    (is (string= "SQLite" (decision-chose d)))
    (is (equal '("PostgreSQL" "DynamoDB") (decision-over d)))
    (is (string= "Embedded, zero-config" (decision-because d)))
    (is (string= "2026-02-06" (decision-date d)))
    (is (string= "Baba" (decision-decided-by d)))))

(test make-decision-minimal
  "make-decision with only :id and :chose, rest nil"
  (let ((d (make-decision :id :minimal :chose "Option A")))
    (is (eq :minimal (decision-id d)))
    (is (string= "Option A" (decision-chose d)))
    (is (null (decision-over d)))
    (is (null (decision-because d)))
    (is (null (decision-date d)))
    (is (null (decision-decided-by d)))))

(test decision-is-a-struct
  "decision is a proper struct type"
  (let ((d (make-decision :id :test)))
    (is (typep d 'decision))))

;;; record-decision tests

(test record-decision-stores
  "record-decision stores a decision retrievable via feature-decisions"
  (remhash 'rd-test-feature telos::*decision-registry*)
  (record-decision 'rd-test-feature
    :id :choice-1 :chose "A" :because "Simplest")
  (let ((decisions (feature-decisions 'rd-test-feature)))
    (is (= 1 (length decisions)))
    (is (eq :choice-1 (decision-id (first decisions))))))

(test record-decision-accumulates
  "multiple record-decision calls accumulate (most recent first)"
  (remhash 'rd-accum-feature telos::*decision-registry*)
  (record-decision 'rd-accum-feature
    :id :first :chose "X")
  (record-decision 'rd-accum-feature
    :id :second :chose "Y")
  (let ((decisions (feature-decisions 'rd-accum-feature)))
    (is (= 2 (length decisions)))
    ;; Most recent first (push semantics)
    (is (eq :second (decision-id (first decisions))))
    (is (eq :first (decision-id (second decisions))))))

(test record-decision-returns-struct
  "record-decision returns the decision struct"
  (let ((result (record-decision 'rd-return-feature
                  :id :ret :chose "Z")))
    (is (typep result 'decision))
    (is (eq :ret (decision-id result)))))

;;; feature-decisions tests

(test feature-decisions-nil-for-unknown
  "feature-decisions returns nil for unknown feature"
  (is (null (feature-decisions 'completely-unknown-feature-xyz))))

;;; deffeature :decisions integration tests

(test deffeature-with-inline-decisions
  "deffeature :decisions clause records decisions and intent still works"
  (remhash 'df-dec-test-1 telos::*decision-registry*)
  (deffeature df-dec-test-1
    :purpose "Feature with a decision"
    :decisions ((:id :lang-choice
                 :chose "Common Lisp"
                 :over ("Python" "Rust")
                 :because "Superior for this domain"
                 :date "2026-02-06"
                 :decided-by "Baba")))
  ;; Intent works
  (let ((intent (feature-intent 'df-dec-test-1)))
    (is (not (null intent)))
    (is (string= "Feature with a decision" (intent-purpose intent))))
  ;; Decision recorded
  (let ((decisions (feature-decisions 'df-dec-test-1)))
    (is (= 1 (length decisions)))
    (is (eq :lang-choice (decision-id (first decisions))))
    (is (string= "Common Lisp" (decision-chose (first decisions))))
    (is (equal '("Python" "Rust") (decision-over (first decisions))))))

(test deffeature-multiple-decisions
  "deffeature with multiple decisions in one form"
  (remhash 'df-dec-test-2 telos::*decision-registry*)
  (deffeature df-dec-test-2
    :purpose "Feature with multiple decisions"
    :decisions ((:id :db :chose "SQLite" :because "Embedded")
                (:id :format :chose "JSON" :because "Universal")))
  (let ((decisions (feature-decisions 'df-dec-test-2)))
    (is (= 2 (length decisions)))))

(test deffeature-without-decisions-unchanged
  "deffeature without :decisions works exactly as before"
  (deffeature df-dec-test-3
    :purpose "No decisions here"
    :goals ((:g1 "A goal")))
  (let ((intent (feature-intent 'df-dec-test-3)))
    (is (not (null intent)))
    (is (string= "No decisions here" (intent-purpose intent)))
    (is (= 1 (length (intent-goals intent)))))
  (is (null (feature-decisions 'df-dec-test-3))))

(test deffeature-returns-name-with-decisions
  "deffeature still returns the feature name symbol when :decisions present"
  (remhash 'df-dec-test-4 telos::*decision-registry*)
  (is (eq 'df-dec-test-4
          (deffeature df-dec-test-4
            :purpose "Return value test"
            :decisions ((:id :x :chose "Y"))))))

;;; list-decisions tests

(test list-decisions-specific-feature
  "list-decisions with feature-name returns that feature's decisions"
  (remhash 'ld-specific telos::*decision-registry*)
  (record-decision 'ld-specific :id :a :chose "A")
  (record-decision 'ld-specific :id :b :chose "B")
  (let ((result (list-decisions 'ld-specific)))
    (is (= 2 (length result)))
    (is (eq :b (decision-id (first result))))))

(test list-decisions-all
  "list-decisions without arg returns alist of all features with decisions"
  (remhash 'ld-all-1 telos::*decision-registry*)
  (remhash 'ld-all-2 telos::*decision-registry*)
  (record-decision 'ld-all-1 :id :x :chose "X")
  (record-decision 'ld-all-2 :id :y :chose "Y")
  (let ((result (list-decisions)))
    ;; result is an alist of (feature-name . decisions)
    (is (listp result))
    (let ((entry-1 (assoc 'ld-all-1 result))
          (entry-2 (assoc 'ld-all-2 result)))
      (is (not (null entry-1)))
      (is (not (null entry-2)))
      (is (= 1 (length (cdr entry-1))))
      (is (= 1 (length (cdr entry-2)))))))
