(in-package :telos/tests)

(def-suite query-tests :in :telos-tests)
(in-suite query-tests)

;;; Setup: define a hierarchy for testing

(deffeature query-test-api
  :purpose "Top-level API feature for testing queries")

(deffeature query-test-rate-limiting
  :purpose "Rate limiting for query tests"
  :belongs-to query-test-api
  :failure-modes ((:user-blocked "Legitimate user blocked")))

(defun/i query-test-check-rate (user-id)
  "Check rate limit"
  (:feature query-test-rate-limiting)
  (:role "Gatekeeper function")
  (declare (ignore user-id))
  t)

(defun/i query-test-track-request (user-id)
  "Track a request"
  (:feature query-test-rate-limiting)
  (:role "Bookkeeper function")
  (declare (ignore user-id))
  nil)

;;; intent-chain tests

(test intent-chain-returns-full-hierarchy
  "intent-chain returns intent from function up to root feature"
  (let ((chain (intent-chain 'query-test-check-rate)))
    ;; Should have at least 2 entries: function + feature
    (is (>= (length chain) 2))
    ;; First should be the function's intent info
    (let ((first (first chain)))
      (is (eq :function (getf first :type)))
      (is (eq 'query-test-check-rate (getf first :name)))
      (is (string= "Gatekeeper function" (getf first :role))))
    ;; Second should be the feature
    (let ((second (second chain)))
      (is (eq :feature (getf second :type)))
      (is (eq 'query-test-rate-limiting (getf second :name)))
      (is (search "Rate limiting" (getf second :purpose))))))

(test intent-chain-includes-parent-features
  "intent-chain includes parent features in the chain"
  (let ((chain (intent-chain 'query-test-check-rate)))
    ;; Should include the parent feature (query-test-api)
    (let ((names (mapcar (lambda (entry) (getf entry :name)) chain)))
      (is (member 'query-test-rate-limiting names))
      (is (member 'query-test-api names)))))

(test intent-chain-returns-nil-for-unknown
  "intent-chain returns nil for symbols without intent"
  (defun query-no-intent-fn () nil)
  (is (null (intent-chain 'query-no-intent-fn))))

(test intent-chain-accepts-explicit-kind
  "intent-chain accepts typed entity specs for ambiguous names"
  (defclass/i query-ambiguous ()
    ()
    (:feature query-test-api)
    (:purpose "Ambiguous class"))
  (defun/i query-ambiguous ()
    (:feature query-test-rate-limiting)
    (:role "Ambiguous function")
    nil)
  (let ((chain (intent-chain '(:function query-ambiguous))))
    (is (eq :function (getf (first chain) :type)))
    (is (eq 'query-ambiguous (getf (first chain) :name)))))

;;; feature-members tests

(test feature-members-returns-functions
  "feature-members returns functions belonging to a feature"
  ;; Define fresh functions for this test
  (defun/i fm-test-fn-1 () (:feature fm-test-feature) nil)
  (defun/i fm-test-fn-2 () (:feature fm-test-feature) nil)
  (deffeature fm-test-feature :purpose "Test feature for members")

  (let ((members (feature-members 'fm-test-feature)))
    (is (member 'fm-test-fn-1 (getf members :functions)))
    (is (member 'fm-test-fn-2 (getf members :functions)))))

(test feature-members-returns-classes
  "feature-members returns classes belonging to a feature"
  (deffeature fm-class-feature :purpose "Feature with classes")
  (defclass/i fm-test-class-1 ()
    ()
    (:feature fm-class-feature))
  (defclass/i fm-test-class-2 ()
    ()
    (:feature fm-class-feature))

  (let ((members (feature-members 'fm-class-feature)))
    (is (member 'fm-test-class-1 (getf members :classes)))
    (is (member 'fm-test-class-2 (getf members :classes)))))

(test feature-members-filter-by-type
  "feature-members can filter by :functions or :classes"
  (deffeature fm-mixed-feature :purpose "Mixed members")
  (defun/i fm-mixed-fn () (:feature fm-mixed-feature) nil)
  (defclass/i fm-mixed-class () () (:feature fm-mixed-feature))

  (let ((fns-only (feature-members 'fm-mixed-feature :functions))
        (classes-only (feature-members 'fm-mixed-feature :classes)))
    (is (listp fns-only))
    (is (member 'fm-mixed-fn fns-only))
    (is (not (member 'fm-mixed-class fns-only)))
    (is (listp classes-only))
    (is (member 'fm-mixed-class classes-only))
    (is (not (member 'fm-mixed-fn classes-only)))))

;;; Cyclic :belongs-to must not take the image down with it

(test intent-chain-survives-a-belongs-to-cycle
  "A cycle used to exhaust the heap: intent-chain pushed an entry per iteration"
  (deffeature cycle-feature-a :purpose "A" :belongs-to cycle-feature-b)
  (deffeature cycle-feature-b :purpose "B" :belongs-to cycle-feature-a)
  (defun/i cycle-test-fn () (:feature cycle-feature-a) (:purpose "In a cycle") nil)
  (let ((chain (intent-chain 'cycle-test-fn)))
    (is (listp chain))
    ;; the function, plus each feature in the cycle exactly once
    (is (= 3 (length chain)))
    (is (equal '(cycle-test-fn cycle-feature-a cycle-feature-b)
               (mapcar (lambda (entry) (getf entry :name)) chain)))))

(test intent-chain-survives-a-self-parent
  (deffeature self-parent-feature :purpose "Its own parent" :belongs-to self-parent-feature)
  (defun/i self-parent-fn () (:feature self-parent-feature) (:purpose "p") nil)
  (let ((chain (intent-chain 'self-parent-fn)))
    (is (= 2 (length chain)))))
