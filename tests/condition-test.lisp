(in-package :telos/tests)

(def-suite condition-tests :in :telos-tests)
(in-suite condition-tests)

;;; define-condition/i - condition definition with intent

(test define-condition-i-creates-working-condition
  "define-condition/i creates a condition that can be signaled"
  (define-condition/i test-not-found-error (error)
    ((name :initarg :name :reader error-name))
    (:feature error-handling-feature)
    (:purpose "Signal when something is not found"))
  ;; Can create and signal
  (let ((condition (make-condition 'test-not-found-error :name "foo")))
    (is (typep condition 'error))
    (is (string= "foo" (error-name condition)))))

(test define-condition-i-stores-intent
  "define-condition/i stores intent retrievable via get-intent"
  (define-condition/i test-validation-error (error)
    ((field :initarg :field :reader validation-field)
     (message :initarg :message :reader validation-message))
    (:feature validation-feature)
    (:role "Report field validation failures")
    (:purpose "Signal invalid input data"))
  (let ((intent (get-intent 'test-validation-error)))
    (is (not (null intent)))
    (is (eq 'validation-feature (intent-belongs-to intent)))
    (is (string= "Report field validation failures" (intent-role intent)))
    (is (string= "Signal invalid input data" (intent-purpose intent)))))

(test define-condition-i-preserves-standard-options
  "define-condition/i preserves :report and other standard options"
  (define-condition/i test-quota-exceeded (error)
    ((limit :initarg :limit :reader quota-limit)
     (current :initarg :current :reader quota-current))
    (:feature resource-feature)
    (:purpose "Signal resource quota exceeded")
    (:report (lambda (c s)
               (format s "Quota exceeded: ~A/~A"
                       (quota-current c) (quota-limit c)))))
  (let ((condition (make-condition 'test-quota-exceeded :limit 100 :current 150)))
    (is (string= "Quota exceeded: 150/100"
                 (princ-to-string condition)))))

(test define-condition-i-registers-as-condition-member
  "define-condition/i registers condition with feature as :condition member type"
  (deffeature condition-member-test-feature
    :purpose "Test feature for condition member registration")
  (define-condition/i test-timeout-error (error)
    ()
    (:feature condition-member-test-feature)
    (:purpose "Signal operation timeout"))
  (let ((members (feature-members 'condition-member-test-feature)))
    (is (member 'test-timeout-error (getf members :conditions)))))
