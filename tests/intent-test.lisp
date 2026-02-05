(in-package :telos/tests)

(def-suite intent-tests :in :telos-tests)
(in-suite intent-tests)

;;; Intent struct creation

(test make-intent-with-purpose-only
  "Intent can be created with just a purpose (the only required field)"
  (let ((intent (make-intent :purpose "Test the system")))
    (is (string= "Test the system" (intent-purpose intent)))
    (is (null (intent-failure-modes intent)))
    (is (null (intent-goals intent)))
    (is (null (intent-constraints intent)))
    (is (null (intent-assumptions intent)))
    (is (null (intent-verification intent)))
    (is (null (intent-belongs-to intent)))
    (is (null (intent-role intent)))
    (is (null (intent-members intent)))))

(test make-intent-with-all-fields
  "Intent can be created with all optional fields populated"
  (let ((intent (make-intent
                 :purpose "Protect system from abuse"
                 :goals '((:block-abuse "No actor overwhelms system")
                          (:allow-legitimate "Auth users never blocked"))
                 :constraints '((:paid-priority "Paid users get higher limits"))
                 :assumptions '((:mostly-legit "99% traffic is legitimate"))
                 :failure-modes '((:user-blocked "Legit user gets 429" :violates :allow-legitimate))
                 :verification '((:false-positive-rate "< 0.1%"))
                 :belongs-to 'api-layer
                 :role "Rate limiting subsystem"
                 :members '(check-rate track-request))))
    (is (string= "Protect system from abuse" (intent-purpose intent)))
    (is (= 2 (length (intent-goals intent))))
    (is (= 1 (length (intent-constraints intent))))
    (is (= 1 (length (intent-assumptions intent))))
    (is (= 1 (length (intent-failure-modes intent))))
    (is (= 1 (length (intent-verification intent))))
    (is (eq 'api-layer (intent-belongs-to intent)))
    (is (string= "Rate limiting subsystem" (intent-role intent)))
    (is (= 2 (length (intent-members intent))))))

(test intent-is-a-struct
  "Intent should be a proper struct type"
  (let ((intent (make-intent :purpose "Test")))
    (is (typep intent 'intent))))
