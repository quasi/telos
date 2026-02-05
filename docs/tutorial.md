# Tutorial: Building a Rate Limiter with Intent

This tutorial teaches Telos by building a rate limiter with queryable intent. By the end, you'll understand how to define features, attach intent to code, and query the system at runtime.

**Time**: 20 minutes
**Prerequisites**: Basic Common Lisp knowledge, REPL access

---

## Step 1: Load Telos

Start your Lisp REPL and load Telos:

```lisp
(ql:quickload :telos)
(use-package :telos)
```

You now have access to `deffeature`, `defun/i`, `defclass/i`, `defstruct/i`, `define-condition/i`, `defintent`, and the query API.

---

## Step 2: Define the Top-Level Feature

Features organize intent hierarchically. Start with a root feature:

```lisp
(deffeature rate-limiting
  :purpose "Prevent API abuse by limiting request frequency"
  :goals ((:availability "Legitimate users can always make requests")
          (:protection "Abusive users are blocked"))
  :constraints ((:performance "Decision must take <1ms"))
  :assumptions ((:user-identity "User IDs are stable and authenticated"))
  :failure-modes ((:false-positive "Legitimate user blocked" :violates :availability)
                  (:false-negative "Abusive user not blocked" :violates :protection)))
```

**What this does**: Registers a feature with structured intent. The system can now answer "Why does rate-limiting exist?" and "What can go wrong?"

**Verify it worked**:

```lisp
(feature-intent 'rate-limiting)
;; => #S(INTENT :PURPOSE "Prevent API abuse..." :GOALS ((:availability ...) ...) ...)
```

---

## Step 3: Define a Sub-Feature

Rate limiting has moving parts. Create a sub-feature for token bucket logic:

```lisp
(deffeature token-bucket
  :purpose "Track request allowance using token bucket algorithm"
  :belongs-to rate-limiting
  :goals ((:smooth-rate "Allow bursts up to bucket capacity")
          (:fair "Replenish rate is constant per user"))
  :constraints ((:atomic "Bucket operations must be thread-safe")))
```

**What this does**: Declares `token-bucket` as a child of `rate-limiting`. The `belongs-to` link creates a hierarchy.

**Verify the hierarchy**:

```lisp
(feature-parent 'token-bucket)
;; => RATE-LIMITING

(feature-children 'rate-limiting)
;; => (TOKEN-BUCKET)
```

---

## Step 4: Define a Function with Intent

Functions belong to features and declare their role:

```lisp
(defun/i check-rate-limit (user-id)
  "Return T if request is allowed, NIL if blocked"
  (:feature token-bucket)
  (:role "Decide if request should proceed")
  (:failure-modes ((:race-condition "Concurrent requests bypass limit")))
  ;; Simplified implementation
  (let ((bucket (get-bucket user-id)))
    (when (> (bucket-tokens bucket) 0)
      (decf (bucket-tokens bucket))
      t)))
```

**What this does**: Defines a working function with embedded intent. The intent lives on the symbol's plist, queryable at runtime.

**Verify the function works**:

```lisp
;; First define a minimal bucket for the example
(defstruct bucket (tokens 10))
(defparameter *buckets* (make-hash-table :test 'equal))
(defun get-bucket (user-id)
  (or (gethash user-id *buckets*)
      (setf (gethash user-id *buckets*) (make-bucket))))

;; Now test the function
(check-rate-limit "user-123")
;; => T (allowed)
```

**Query the intent**:

```lisp
(get-intent 'check-rate-limit)
;; => #S(INTENT :BELONGS-TO TOKEN-BUCKET :ROLE "Decide if request should proceed" ...)

(intent-feature 'check-rate-limit)
;; => TOKEN-BUCKET
```

---

## Step 5: Add Another Function

Rate limiters refill tokens over time. Add a replenishment function:

```lisp
(defun/i replenish-tokens (user-id)
  "Add tokens based on elapsed time"
  (:feature token-bucket)
  (:role "Restore request capacity over time")
  (:constraints ((:monotonic "Tokens never exceed bucket capacity")))
  (let ((bucket (get-bucket user-id)))
    (setf (bucket-tokens bucket)
          (min 10 (1+ (bucket-tokens bucket))))))
```

**What this does**: Adds a second function to the `token-bucket` feature. Now the feature has multiple members.

**Query feature members**:

```lisp
(feature-members 'token-bucket)
;; => (:FUNCTIONS (CHECK-RATE-LIMIT REPLENISH-TOKENS) :CLASSES NIL :FEATURES NIL)

;; Get only functions
(feature-members 'token-bucket :functions)
;; => (CHECK-RATE-LIMIT REPLENISH-TOKENS)
```

---

## Step 6: Define a Class with Intent

Store bucket state in an intentful class:

```lisp
(defclass/i rate-limit-bucket ()
  ((user-id :initarg :user-id :reader bucket-user-id)
   (tokens :initform 10 :accessor bucket-tokens)
   (last-refill :initform (get-universal-time) :accessor bucket-last-refill))
  (:feature token-bucket)
  (:purpose "Store per-user token bucket state")
  (:role "State container for rate limiting"))
```

**What this does**: Defines a class with the `intentful-class` metaclass. Intent is stored on the class object itself.

**Verify the metaclass**:

```lisp
(typep (find-class 'rate-limit-bucket) 'intentful-class)
;; => T

(class-intent (find-class 'rate-limit-bucket))
;; => #S(INTENT :PURPOSE "Store per-user token bucket state" ...)
```

**Use the class**:

```lisp
(defparameter *my-bucket* (make-instance 'rate-limit-bucket :user-id "user-456"))
(bucket-tokens *my-bucket*)
;; => 10
```

---

## Step 7: Query the Full Intent Chain

Trace intent from function to root feature:

```lisp
(intent-chain 'check-rate-limit)
;; => ((:TYPE :FUNCTION
;;      :NAME CHECK-RATE-LIMIT
;;      :ROLE "Decide if request should proceed"
;;      :FAILURE-MODES ((:RACE-CONDITION "Concurrent requests bypass limit")))
;;     (:TYPE :FEATURE
;;      :NAME TOKEN-BUCKET
;;      :PURPOSE "Track request allowance using token bucket algorithm"
;;      :FAILURE-MODES NIL)
;;     (:TYPE :FEATURE
;;      :NAME RATE-LIMITING
;;      :PURPOSE "Prevent API abuse by limiting request frequency"
;;      :FAILURE-MODES ((:FALSE-POSITIVE "Legitimate user blocked" :VIOLATES :AVAILABILITY) ...)))
```

**What this shows**: The full context of why `check-rate-limit` exists, from its specific role up to the root purpose.

---

## Step 8: Retrofit Intent to Existing Code

You don't need to rewrite code to add intent. Use `defintent` for existing functions:

```lisp
;; Existing function without intent
(defun log-rate-limit-event (user-id action)
  (format t "~&RATE-LIMIT: ~A ~A~%" user-id action))

;; Add intent retroactively
(defintent log-rate-limit-event
  :feature rate-limiting
  :role "Audit trail for rate limit decisions"
  :purpose "Provide visibility into rate limiter behavior")
```

**Verify it worked**:

```lisp
(get-intent 'log-rate-limit-event)
;; => #S(INTENT :PURPOSE "Provide visibility into rate limiter behavior" ...)

;; Function still works
(log-rate-limit-event "user-789" "BLOCKED")
;; => RATE-LIMIT: user-789 BLOCKED
```

---

## Step 9: List and Filter Features

Find features by name or purpose:

```lisp
;; List all features
(list-features)
;; => (RATE-LIMITING TOKEN-BUCKET ...)

;; Filter by substring (matches name or purpose)
(list-features "bucket")
;; => (TOKEN-BUCKET)

(list-features "abuse")
;; => (RATE-LIMITING)

;; List only children of a feature
(list-features nil :parent 'rate-limiting)
;; => (TOKEN-BUCKET)
```

---

## Recap

You learned how to:

1. **Define features** with `deffeature` and link them hierarchically
2. **Define functions with intent** using `defun/i`
3. **Define classes with intent** using `defclass/i`
4. **Retrofit intent** onto existing code with `defintent`
5. **Query intent** using `get-intent`, `intent-chain`, and `feature-members`
6. **Navigate the hierarchy** with `feature-parent` and `feature-children`
7. **Search features** with `list-features`

**Additional Capabilities** (see [API Reference](reference.md)):

- **Structs with intent**: Use `defstruct/i` to define structs with embedded intent
- **Conditions with intent**: Use `define-condition/i` to define conditions with embedded intent
- **Method intent**: Use `defintent` with a list like `(generic-name specializer)` to add intent to specific method specializations
- **Symbol convenience**: `class-intent` accepts symbols directly: `(class-intent 'my-class)`

---

## Next Steps

- Read the [API Reference](reference.md) for complete function signatures
- Read the [Explanation](explanation.md) to understand design choices
- Apply Telos to your own codebase

---

**Navigation**: [← README](../README.md) | [API Reference →](reference.md)
