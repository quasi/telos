(defpackage :telos/tests
  (:use :cl :fiveam :telos))

(in-package :telos/tests)

(def-suite :telos-tests
  :description "Test suite for Telos intent introspection library")

(in-suite :telos-tests)
