(in-package :telos/tests)

(def-suite class-tests :in :telos-tests)
(in-suite class-tests)

;;; defclass/i - class definition with intent via metaclass

(test defclass-i-creates-working-class
  "defclass/i creates a class that can be instantiated"
  (defclass/i test-stack ()
    ((items :initform nil :accessor stack-items))
    (:feature stack-feature)
    (:purpose "A simple stack for testing"))
  (let ((instance (make-instance 'test-stack)))
    (is (not (null instance)))
    (is (null (stack-items instance)))))

(test defclass-i-uses-intentful-metaclass
  "defclass/i creates a class with intentful-class metaclass"
  (defclass/i metaclass-test-class ()
    ()
    (:feature some-feature)
    (:purpose "Testing metaclass"))
  (is (typep (find-class 'metaclass-test-class) 'intentful-class)))

(test defclass-i-stores-intent-on-class
  "defclass/i stores intent retrievable via get-intent or class-intent"
  (defclass/i intent-test-class ()
    ((value :initarg :value :accessor test-value))
    (:feature data-feature)
    (:purpose "Hold a test value")
    (:role "Container for testing"))
  (let ((intent (get-intent 'intent-test-class)))
    (is (not (null intent)))
    (is (eq 'data-feature (intent-belongs-to intent)))
    (is (string= "Hold a test value" (intent-purpose intent)))
    (is (string= "Container for testing" (intent-role intent)))))

(test class-intent-accessor-works
  "class-intent accessor retrieves intent from metaclass"
  (defclass/i accessor-test-class ()
    ()
    (:feature accessor-feature)
    (:purpose "Test class-intent accessor"))
  (let* ((class (find-class 'accessor-test-class))
         (intent (class-intent class)))
    (is (not (null intent)))
    (is (string= "Test class-intent accessor" (intent-purpose intent)))))

;;; defintent for classes (retrofit)

(test defintent-works-for-existing-class
  "defintent can add intent to an existing class via registry"
  ;; Define a plain class
  (defclass existing-class ()
    ((data :initarg :data)))
  ;; Retrofit intent
  (defintent existing-class
    :feature retrofitted-class-feature
    :purpose "A retrofitted class"
    :role "Storage for data")
  (let ((intent (get-intent 'existing-class)))
    (is (not (null intent)))
    (is (eq 'retrofitted-class-feature (intent-belongs-to intent)))
    (is (string= "A retrofitted class" (intent-purpose intent))))
  ;; Class still works
  (let ((instance (make-instance 'existing-class :data 42)))
    (is (= 42 (slot-value instance 'data)))))

;;; intent-feature works for classes too

(test intent-feature-works-for-classes
  "intent-feature returns feature for classes"
  (defclass/i featured-class ()
    ()
    (:feature my-class-feature))
  (is (eq 'my-class-feature (intent-feature 'featured-class))))
