(in-package :telos/tests)

(def-suite feature-tests :in :telos-tests)
(in-suite feature-tests)

;;; Feature definition and retrieval

(test deffeature-creates-queryable-feature
  "deffeature registers a feature that can be queried"
  (deffeature test-feature-1
    :purpose "A test feature for unit testing")
  (let ((intent (feature-intent 'test-feature-1)))
    (is (not (null intent)))
    (is (string= "A test feature for unit testing" (intent-purpose intent)))))

(test deffeature-with-all-fields
  "deffeature accepts all intent fields"
  (deffeature test-feature-2
    :purpose "Full featured test"
    :goals ((:g1 "Goal one"))
    :constraints ((:c1 "Constraint one"))
    :assumptions ((:a1 "Assumption one"))
    :failure-modes ((:f1 "Failure one" :violates :g1))
    :verification ((:v1 "Verify one"))
    :belongs-to test-parent-feature)
  (let ((intent (feature-intent 'test-feature-2)))
    (is (string= "Full featured test" (intent-purpose intent)))
    (is (= 1 (length (intent-goals intent))))
    (is (= 1 (length (intent-constraints intent))))
    (is (= 1 (length (intent-assumptions intent))))
    (is (= 1 (length (intent-failure-modes intent))))
    (is (= 1 (length (intent-verification intent))))
    (is (eq 'test-parent-feature (intent-belongs-to intent)))))

(test feature-intent-returns-nil-for-unknown
  "feature-intent returns nil for undefined features"
  (is (null (feature-intent 'nonexistent-feature-xyz))))

(test list-features-returns-all-defined
  "list-features returns all defined features"
  (deffeature list-test-feature-a :purpose "Feature A")
  (deffeature list-test-feature-b :purpose "Feature B")
  (let ((features (list-features)))
    (is (member 'list-test-feature-a features))
    (is (member 'list-test-feature-b features))))

(test list-features-with-string-filter
  "list-features filters by name substring"
  (deffeature rate-limiting-test :purpose "Rate limiting")
  (deffeature auth-test :purpose "Authentication")
  (let ((rate-features (list-features "rate")))
    (is (member 'rate-limiting-test rate-features))
    (is (not (member 'auth-test rate-features)))))

(test list-features-matches-purpose-text
  "list-features also matches against purpose text"
  (deffeature abuse-protection :purpose "Protect from abuse")
  (let ((features (list-features "abuse")))
    (is (member 'abuse-protection features))))

(test feature-parent-returns-belongs-to
  "feature-parent returns the parent feature"
  (deffeature parent-feature :purpose "I am the parent")
  (deffeature child-feature :purpose "I am the child" :belongs-to parent-feature)
  (is (eq 'parent-feature (feature-parent 'child-feature)))
  (is (null (feature-parent 'parent-feature))))

(test feature-children-returns-direct-children
  "feature-children returns features that belong to the given feature"
  (deffeature fc-parent :purpose "Parent")
  (deffeature fc-child-1 :purpose "Child 1" :belongs-to fc-parent)
  (deffeature fc-child-2 :purpose "Child 2" :belongs-to fc-parent)
  (deffeature fc-other :purpose "Other" :belongs-to some-other-parent)
  (let ((children (feature-children 'fc-parent)))
    (is (member 'fc-child-1 children))
    (is (member 'fc-child-2 children))
    (is (not (member 'fc-other children)))))
