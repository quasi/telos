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
  (record-decision 'rd-test-feature
    :id :choice-1 :chose "A" :because "Simplest")
  (let ((decisions (feature-decisions 'rd-test-feature)))
    (is (= 1 (length decisions)))
    (is (eq :choice-1 (decision-id (first decisions))))))

(test record-decision-accumulates
  "multiple record-decision calls accumulate (most recent first)"
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
