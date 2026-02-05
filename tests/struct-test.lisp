(in-package :telos/tests)

(def-suite struct-tests :in :telos-tests)
(in-suite struct-tests)

;;; defstruct/i - struct definition with intent

(test defstruct-i-creates-working-struct
  "defstruct/i creates a struct that can be instantiated"
  (defstruct/i test-point
    (x 0 :type fixnum)
    (y 0 :type fixnum)
    (:feature geometry-feature)
    (:purpose "Represent a 2D point"))
  (let ((p (make-test-point :x 10 :y 20)))
    (is (= 10 (test-point-x p)))
    (is (= 20 (test-point-y p)))))

(test defstruct-i-stores-intent
  "defstruct/i stores intent retrievable via get-intent"
  (defstruct/i test-rectangle
    (width 0 :type fixnum)
    (height 0 :type fixnum)
    (:feature geometry-feature)
    (:role "Axis-aligned rectangle")
    (:purpose "Represent a rectangle for area calculations"))
  (let ((intent (get-intent 'test-rectangle)))
    (is (not (null intent)))
    (is (eq 'geometry-feature (intent-belongs-to intent)))
    (is (string= "Axis-aligned rectangle" (intent-role intent)))
    (is (string= "Represent a rectangle for area calculations" (intent-purpose intent)))))

(test defstruct-i-with-name-and-options
  "defstruct/i works with (name options...) form"
  (defstruct/i (test-circle (:conc-name circle-))
    (radius 1.0 :type single-float)
    (:feature geometry-feature)
    (:purpose "Represent a circle"))
  (let ((c (make-test-circle :radius 5.0)))
    (is (= 5.0 (circle-radius c))))
  (let ((intent (get-intent 'test-circle)))
    (is (not (null intent)))
    (is (string= "Represent a circle" (intent-purpose intent)))))

(test defstruct-i-registers-as-struct-member
  "defstruct/i registers struct with feature as :struct member type"
  ;; Clear any existing members
  (deffeature struct-member-test-feature
    :purpose "Test feature for struct member registration")
  (defstruct/i test-vec3
    (x 0.0) (y 0.0) (z 0.0)
    (:feature struct-member-test-feature)
    (:purpose "3D vector"))
  (let ((members (feature-members 'struct-member-test-feature)))
    (is (member 'test-vec3 (getf members :structs)))))
