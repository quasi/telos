(in-package :telos/tests)

(def-suite function-tests :in :telos-tests)
(in-suite function-tests)

;;; defun/i - function definition with intent

(test defun-i-creates-working-function
  "defun/i creates a function that actually works"
  (defun/i test-add (a b)
    "Add two numbers"
    (:feature some-math-feature)
    (:role "Basic addition")
    (+ a b))
  (is (= 5 (test-add 2 3))))

(test defun-i-stores-intent-on-symbol
  "defun/i stores intent retrievable via get-intent"
  (defun/i test-multiply (a b)
    "Multiply two numbers"
    (:feature math-feature)
    (:role "Basic multiplication")
    (:failure-modes ((:overflow "Numbers too large")))
    (* a b))
  (let ((intent (get-intent 'test-multiply)))
    (is (not (null intent)))
    (is (eq 'math-feature (intent-belongs-to intent)))
    (is (string= "Basic multiplication" (intent-role intent)))
    (is (= 1 (length (intent-failure-modes intent))))))

(test defun-i-without-intent-clauses
  "defun/i works with just docstring, no intent clauses"
  (defun/i test-subtract (a b)
    "Subtract two numbers"
    (- a b))
  (is (= 2 (test-subtract 5 3)))
  ;; Should still have some intent (possibly minimal)
  (let ((intent (get-intent 'test-subtract)))
    ;; Intent may be nil or minimal - that's ok
    (is (or (null intent)
            (typep intent 'intent)))))

(test defun-i-preserves-docstring
  "defun/i preserves the docstring on the function"
  (defun/i test-divide (a b)
    "Divide A by B"
    (:feature math-feature)
    (/ a b))
  (is (string= "Divide A by B" (documentation 'test-divide 'function))))

;;; defintent - retrofit intent onto existing function

(test defintent-adds-intent-to-existing-function
  "defintent adds intent to a pre-existing function"
  ;; First define a plain function
  (defun existing-fn (x) (* x x))
  ;; Then add intent
  (defintent existing-fn
    :feature retrofitted-feature
    :role "Square a number"
    :purpose "Calculate squares for area computation")
  (let ((intent (get-intent 'existing-fn)))
    (is (not (null intent)))
    (is (eq 'retrofitted-feature (intent-belongs-to intent)))
    (is (string= "Square a number" (intent-role intent)))
    (is (string= "Calculate squares for area computation" (intent-purpose intent))))
  ;; Function still works
  (is (= 16 (existing-fn 4))))

;;; get-intent - query function intent

(test get-intent-returns-nil-for-unknown
  "get-intent returns nil for functions without intent"
  (defun no-intent-fn () nil)
  (is (null (get-intent 'no-intent-fn))))

;;; intent-feature - quick lookup

(test intent-feature-returns-feature-name
  "intent-feature returns which feature a function belongs to"
  (defun/i featured-fn ()
    (:feature my-cool-feature)
    nil)
  (is (eq 'my-cool-feature (intent-feature 'featured-fn))))

(test intent-feature-returns-nil-for-unknown
  "intent-feature returns nil for functions without intent"
  (defun unfeatured-fn () nil)
  (is (null (intent-feature 'unfeatured-fn))))
