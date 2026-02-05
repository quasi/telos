(defsystem "telos"
  :version "0.1.0"
  :author "quasi / quasiLabs"
  :license "MIT"
  :description "Intent introspection for Common Lisp — make the WHY queryable"
  :depends-on ("closer-mop")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "intent")
     (:file "storage")
     (:file "feature")
     (:file "function")
     (:file "class")
     (:file "struct")
     (:file "condition")
     (:file "query"))))
  :in-order-to ((test-op (test-op "telos/tests"))))

(defsystem "telos/tests"
  :depends-on ("telos" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "package")
     (:file "intent-test")
     (:file "feature-test")
     (:file "function-test")
     (:file "class-test")
     (:file "struct-test")
     (:file "condition-test")
     (:file "query-test"))))
  :perform (test-op (o s)
             (uiop:symbol-call :fiveam :run! :telos-tests)))
