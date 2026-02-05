(in-package :telos/tests)

(def-suite method-tests :in :telos-tests)
(in-suite method-tests)

;;; Method specializer support in defintent

;; Set up test generic functions and classes
(defgeneric consolidate (strategy values)
  (:documentation "Consolidate values using a strategy"))

(defmethod consolidate ((strategy (eql :average)) values)
  (/ (reduce #'+ values) (length values)))

(defmethod consolidate ((strategy (eql :max)) values)
  (reduce #'max values))

(defclass test-backend () ())
(defclass memory-backend (test-backend) ())
(defclass sqlite-backend (test-backend) ())

(defgeneric backend-store (backend key value)
  (:documentation "Store a value in the backend"))

(defmethod backend-store ((backend memory-backend) key value)
  (list :memory key value))

(defmethod backend-store ((backend sqlite-backend) key value)
  (list :sqlite key value))

;;; Tests

(test defintent-accepts-method-specializers-eql
  "defintent accepts (function-name (eql value)) for eql-specialized methods"
  (deffeature consolidation-feature
    :purpose "Data consolidation strategies")
  (defintent (consolidate (eql :average))
    :feature consolidation-feature
    :role "Compute mean for time-averaged metrics"
    :purpose "Average consolidation function")
  (let ((intent (method-intent '(consolidate (eql :average)))))
    (is (not (null intent)))
    (is (eq 'consolidation-feature (intent-belongs-to intent)))
    (is (string= "Compute mean for time-averaged metrics" (intent-role intent)))))

(test defintent-accepts-method-specializers-class
  "defintent accepts (function-name class-name) for class-specialized methods"
  (deffeature backend-feature
    :purpose "Storage backends")
  (defintent (backend-store memory-backend)
    :feature backend-feature
    :role "Store in-memory hash table"
    :purpose "Fast ephemeral storage")
  (let ((intent (method-intent '(backend-store memory-backend))))
    (is (not (null intent)))
    (is (eq 'backend-feature (intent-belongs-to intent)))
    (is (string= "Store in-memory hash table" (intent-role intent)))))

(test method-intent-returns-nil-for-unknown
  "method-intent returns nil for methods without intent"
  (is (null (method-intent '(consolidate (eql :max)))))
  (is (null (method-intent '(nonexistent-fn some-class)))))

(test defintent-registers-method-as-member
  "defintent with method specializers registers as :method member type"
  (deffeature method-member-test-feature
    :purpose "Test feature for method member registration")
  (defintent (backend-store sqlite-backend)
    :feature method-member-test-feature
    :role "Store in SQLite database"
    :purpose "Persistent storage")
  (let ((members (feature-members 'method-member-test-feature)))
    (is (member '(backend-store sqlite-backend) (getf members :methods)
                :test #'equal))))

(test get-intent-works-with-method-specializers
  "get-intent accepts method specializer lists"
  (deffeature get-intent-method-feature
    :purpose "Test get-intent with methods")
  (defintent (consolidate (eql :average))
    :feature get-intent-method-feature
    :purpose "Test method intent via get-intent")
  ;; get-intent should also work with method specs
  (let ((intent (get-intent '(consolidate (eql :average)))))
    (is (not (null intent)))
    (is (eq 'get-intent-method-feature (intent-belongs-to intent)))))
