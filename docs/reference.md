# API Reference

Complete reference for all Telos functions, macros, and data structures.

---

## Table of Contents

- [Data Structures](#data-structures)
  - [intent](#intent-struct)
- [Definition Macros](#definition-macros)
  - [deffeature](#deffeature)
  - [defun/i](#defuni)
  - [defclass/i](#defclassi)
  - [defstruct/i](#defstructi)
  - [define-condition/i](#define-conditioni)
  - [defintent](#defintent)
- [Query API](#query-api)
  - [get-intent](#get-intent)
  - [method-intent](#method-intent)
  - [intent-feature](#intent-feature)
  - [intent-chain](#intent-chain)
  - [feature-intent](#feature-intent)
  - [feature-members](#feature-members)
  - [feature-parent](#feature-parent)
  - [feature-children](#feature-children)
  - [list-features](#list-features)
- [Metaclass](#metaclass)
  - [intentful-class](#intentful-class)
  - [class-intent](#class-intent)

---

## Data Structures

### `intent` (struct)

Core data structure representing intent at any level (feature, function, class).

**Exported Accessors**:

- `intent-purpose` → `(or null string)` — Why this exists
- `intent-failure-modes` → `list` — What can go wrong
- `intent-goals` → `list` — Success criteria
- `intent-constraints` → `list` — Boundaries and limits
- `intent-assumptions` → `list` — World assumptions
- `intent-verification` → `list` — How to verify correctness
- `intent-belongs-to` → `(or null symbol)` — Parent feature
- `intent-role` → `(or null string)` — Role within parent feature
- `intent-members` → `list` — Child functions/classes (internal use)

**Constructor**:

```lisp
(make-intent &key purpose failure-modes goals constraints
                  assumptions verification belongs-to role members)
```

**Example**:

```lisp
(make-intent :purpose "Validate input"
             :failure-modes '((:injection "SQL injection risk"))
             :belongs-to 'data-validation)
```

---

## Definition Macros

### `deffeature`

**Signature**:

```lisp
(deffeature name &key purpose goals constraints assumptions
                      failure-modes verification belongs-to)
```

**Purpose**: Define a feature with structured intent.

**Parameters**:

- `name` — Symbol naming the feature
- `purpose` — String describing why this feature exists (recommended)
- `goals` — List of `(:id "description")` for success criteria
- `constraints` — List of `(:id "description")` for boundaries
- `assumptions` — List of `(:id "description")` for world assumptions
- `failure-modes` — List of `(:id "description" :violates :goal-id)` for failure scenarios
- `verification` — List of `(:id "description")` for verification methods
- `belongs-to` — Parent feature symbol (optional, creates hierarchy)

**Returns**: `name`

**Example**:

```lisp
(deffeature user-authentication
  :purpose "Verify user identity before access"
  :goals ((:secure "No unauthorized access")
          (:usable "Login completes in <2 seconds"))
  :failure-modes ((:lockout "User locked out" :violates :usable))
  :belongs-to security-features)
```

**See Also**: `feature-intent`, `feature-parent`, `feature-children`

---

### `defun/i`

**Signature**:

```lisp
(defun/i name lambda-list &body body)
```

**Purpose**: Define a function with embedded intent.

**Syntax**: Like `defun`, but supports intent clauses after the optional docstring:

```lisp
(defun/i function-name (arg1 arg2)
  "Optional docstring"
  (declare ...) ; Optional declarations
  (:feature feature-name)
  (:role "Role description")
  (:purpose "Why this function exists")
  (:failure-modes ((:id "description") ...))
  (:goals ((:id "description") ...))
  (:constraints ((:id "description") ...))
  (:assumptions ((:id "description") ...))
  (:verification ((:id "description") ...))
  ;; Function body follows
  (+ arg1 arg2))
```

**Intent Clauses** (all optional):

- `(:feature feature-name)` — Which feature this belongs to
- `(:role "description")` — Role within the feature
- `(:purpose "description")` — Why this function exists
- `(:failure-modes list)` — What can go wrong
- `(:goals list)`, `(:constraints list)`, `(:assumptions list)`, `(:verification list)` — Same as `deffeature`

**Returns**: `name`

**Behavior**:
1. Defines the function (works like `defun`)
2. Stores intent on symbol's plist under `'telos:intent`
3. Registers function as member of feature (if `:feature` specified)

**Example**:

```lisp
(defun/i validate-email (email)
  "Check if email format is valid"
  (:feature input-validation)
  (:role "Email format checker")
  (:failure-modes ((:bypass "Regex bypass via unicode")))
  (and (stringp email)
       (search "@" email)))
```

**See Also**: `get-intent`, `intent-feature`, `defintent`

---

### `defclass/i`

**Signature**:

```lisp
(defclass/i name superclasses slots &rest options)
```

**Purpose**: Define a class with intent via metaclass.

**Syntax**: Like `defclass`, but uses `intentful-class` metaclass and supports intent options:

```lisp
(defclass/i class-name (superclasses)
  ((slot-1 :initarg :slot-1 :accessor slot-1-accessor)
   (slot-2 :initform default-value))
  (:feature feature-name)
  (:role "Role description")
  (:purpose "Why this class exists")
  (:failure-modes ...)
  ;; Other standard defclass options
  (:documentation "Class docstring"))
```

**Intent Options** (same as `defun/i`):

- `(:feature feature-name)`
- `(:role "description")`
- `(:purpose "description")`
- `(:failure-modes list)`, `(:goals list)`, etc.

**Returns**: The class object

**Behavior**:
1. Defines the class with `intentful-class` as metaclass
2. Stores intent on the class object (accessible via `class-intent`)
3. Registers class as member of feature (if `:feature` specified)

**Example**:

```lisp
(defclass/i user ()
  ((username :initarg :username :reader user-username)
   (email :initarg :email :accessor user-email))
  (:feature user-management)
  (:purpose "Represent a user account")
  (:role "User account model"))
```

**See Also**: `intentful-class`, `class-intent`, `get-intent`

---

### `defstruct/i`

**Signature**:

```lisp
(defstruct/i name-and-options &body slots-and-intent)
```

**Purpose**: Define a struct with embedded intent.

**Syntax**: Like `defstruct`, but supports intent clauses after slots:

```lisp
(defstruct/i struct-name
  (slot-1 default-value)
  (slot-2 nil :type string)
  (:feature feature-name)
  (:role "Role description")
  (:purpose "Why this struct exists")
  (:failure-modes ...)
  (:goals ...)
  (:constraints ...)
  (:assumptions ...)
  (:verification ...))
```

**Intent Clauses** (same as `defun/i`):

- `(:feature feature-name)` — Which feature this belongs to
- `(:role "description")` — Role within the feature
- `(:purpose "description")` — Why this struct exists
- `(:failure-modes list)`, `(:goals list)`, etc.

**Returns**: `name`

**Behavior**:
1. Defines the struct (works like `defstruct`)
2. Stores intent on symbol's plist under `'telos:intent`
3. Registers struct as member of feature (if `:feature` specified)

**Example**:

```lisp
(defstruct/i token-bucket
  (tokens 10 :type integer)
  (capacity 10 :type integer)
  (last-refill (get-universal-time) :type integer)
  (:feature rate-limiting)
  (:purpose "Store per-user rate limit state")
  (:role "State container for token bucket algorithm"))

;; Query intent
(get-intent 'token-bucket)
;; => #S(INTENT :PURPOSE "Store per-user rate limit state" ...)
```

**Note**: Also supports `defstruct` options like `(:conc-name ...)`:

```lisp
(defstruct/i (bucket (:conc-name bucket-))
  (tokens 10)
  (:feature rate-limiting)
  (:purpose "Rate limit state"))
```

**See Also**: `get-intent`, `feature-members`

---

### `define-condition/i`

**Signature**:

```lisp
(define-condition/i name parent-types slots &rest options)
```

**Purpose**: Define a condition with embedded intent.

**Syntax**: Like `define-condition`, but supports intent clauses in options:

```lisp
(define-condition/i condition-name (parent-condition)
  ((slot-1 :initarg :slot-1 :reader slot-1-reader))
  (:feature feature-name)
  (:role "Role description")
  (:purpose "Why this condition exists")
  (:failure-modes ...)
  ;; Other standard define-condition options
  (:report (lambda (c s) (format s "..."))))
```

**Intent Options** (same as `defclass/i`):

- `(:feature feature-name)`
- `(:role "description")`
- `(:purpose "description")`
- `(:failure-modes list)`, `(:goals list)`, etc.

**Returns**: `name`

**Behavior**:
1. Defines the condition (works like `define-condition`)
2. Stores intent on symbol's plist under `'telos:intent`
3. Registers condition as member of feature (if `:feature` specified)

**Example**:

```lisp
(define-condition/i rate-limit-exceeded (error)
  ((user-id :initarg :user-id :reader exceeded-user-id)
   (limit :initarg :limit :reader exceeded-limit))
  (:feature rate-limiting)
  (:purpose "Signal when request rate exceeds allowed limit")
  (:role "Error condition for rate limit violations")
  (:report (lambda (c s)
             (format s "Rate limit exceeded for user ~A (limit: ~A)"
                     (exceeded-user-id c) (exceeded-limit c)))))

;; Query intent
(get-intent 'rate-limit-exceeded)
;; => #S(INTENT :PURPOSE "Signal when request rate exceeds allowed limit" ...)
```

**See Also**: `get-intent`, `feature-members`

---

### `defintent`

**Signature**:

```lisp
(defintent name &key feature role purpose failure-modes
                     goals constraints assumptions verification)
```

**Purpose**: Add intent to an existing function, class, or method without rewriting it.

**Parameters**:

- `name` — Symbol naming the function or class, OR list for method specializers:
  - `(generic-name specializer1 specializer2 ...)` where specializers are class names or `(eql value)`
- `feature` — Which feature this belongs to
- `role` — Role within the feature
- `purpose` — Why this exists
- Other fields same as `deffeature`

**Returns**: `name`

**Behavior**:
1. For symbols: stores intent on symbol's plist (functions) or class registry (classes)
2. For method specs: stores intent in method registry (keyed by specializer list)
3. Registers as member of feature (if `feature` specified)
4. Does not modify the function, class, or method definition itself

**Example (function)**:

```lisp
;; Existing function
(defun legacy-hash (password)
  (md5-hash password))

;; Add intent retroactively
(defintent legacy-hash
  :feature authentication
  :role "Password hashing"
  :purpose "Hash passwords for storage"
  :failure-modes ((:weak-algo "MD5 is cryptographically broken")))
```

**Example (method with EQL specializer)**:

```lisp
;; Existing generic function and method
(defgeneric consolidate (strategy values))

(defmethod consolidate ((strategy (eql :average)) values)
  (/ (reduce #'+ values) (length values)))

;; Add intent to specific method
(defintent (consolidate (eql :average))
  :feature metrics
  :role "Compute arithmetic mean for time-averaged metrics"
  :purpose "Provide smoothed value over time window")
```

**Example (method with class specializer)**:

```lisp
;; Existing method
(defmethod store-data ((backend memory-backend) key value)
  (setf (gethash key (backend-storage backend)) value))

;; Add intent
(defintent (store-data memory-backend)
  :feature storage
  :role "Store data in memory hash table"
  :purpose "Fast in-memory storage for development/testing"
  :constraints ((:volatile "Data lost on restart")))
```

**Use Case**: Retrofit intent onto third-party code, CL built-ins, legacy functions, or specific method specializations you don't want to rewrite.

**See Also**: `get-intent`, `method-intent`

---

## Query API

### `get-intent`

**Signature**:

```lisp
(get-intent name) → (or intent null)
```

**Purpose**: Get intent for a function, class, struct, condition, or method.

**Parameters**:

- `name` — Symbol naming the function/class/struct/condition, OR list for method specializers:
  - `(generic-name specializer1 ...)` for method intent

**Returns**: `intent` struct or `nil` if not found

**Behavior**: Checks multiple locations based on name type:

For lists (method specializers):
1. Method registry

For symbols:
1. Symbol plist (for `defun/i`, `defstruct/i`, `define-condition/i`, or `defintent`)
2. Class metaclass (for `defclass/i` classes)
3. Class registry (for `defintent` on classes)

**Example (function)**:

```lisp
(get-intent 'verify-credentials)
;; => #S(INTENT :BELONGS-TO USER-AUTHENTICATION :ROLE "Validate credentials" ...)
```

**Example (struct)**:

```lisp
(get-intent 'token-bucket)
;; => #S(INTENT :PURPOSE "Store per-user rate limit state" ...)
```

**Example (method)**:

```lisp
(get-intent '(consolidate (eql :average)))
;; => #S(INTENT :ROLE "Compute arithmetic mean" ...)
```

```lisp
(get-intent 'nonexistent-function)
;; => NIL
```

**See Also**: `method-intent`, `intent-feature`, `intent-chain`

---

### `method-intent`

**Signature**:

```lisp
(method-intent method-spec) → (or intent null)
```

**Purpose**: Get intent for a specific method specialization.

**Parameters**:

- `method-spec` — List of form `(generic-name specializer1 specializer2 ...)` where specializers are class names or `(eql value)`

**Returns**: `intent` struct or `nil` if not found

**Example**:

```lisp
(method-intent '(consolidate (eql :average)))
;; => #S(INTENT :ROLE "Compute arithmetic mean" ...)

(method-intent '(store-data memory-backend))
;; => #S(INTENT :ROLE "Store data in memory hash table" ...)
```

**Note**: This is a lower-level function. For most use cases, `get-intent` with a list argument is equivalent and preferred.

**See Also**: `get-intent`, `defintent`

---

### `intent-feature`

**Signature**:

```lisp
(intent-feature name) → (or symbol null)
```

**Purpose**: Quick lookup for which feature a function or class belongs to.

**Parameters**:

- `name` — Symbol naming the function or class

**Returns**: Feature name (symbol) or `nil`

**Example**:

```lisp
(intent-feature 'check-rate-limit)
;; => TOKEN-BUCKET
```

**Equivalent to**: `(intent-belongs-to (get-intent name))`

---

### `intent-chain`

**Signature**:

```lisp
(intent-chain name) → list
```

**Purpose**: Get full intent chain from function/class up to root feature.

**Parameters**:

- `name` — Symbol naming the function or class

**Returns**: List of plists, each representing one level of the hierarchy. Returns `nil` if `name` has no intent.

**Plist Format**:

```lisp
(:type <:function | :class | :feature>
 :name <symbol>
 :role <string or nil>
 :purpose <string or nil>
 :failure-modes <list>)
```

**Example**:

```lisp
(intent-chain 'verify-password)
;; => ((:TYPE :FUNCTION
;;      :NAME VERIFY-PASSWORD
;;      :ROLE "Check password hash"
;;      :PURPOSE NIL
;;      :FAILURE-MODES ((:timing "Timing attack risk")))
;;     (:TYPE :FEATURE
;;      :NAME USER-AUTHENTICATION
;;      :PURPOSE "Verify user identity"
;;      :FAILURE-MODES ((:lockout "User blocked" :VIOLATES :USABLE)))
;;     (:TYPE :FEATURE
;;      :NAME SECURITY
;;      :PURPOSE "Protect system from threats"
;;      :FAILURE-MODES NIL))
```

**Use Case**: Trace the full context of why code exists, from specific implementation to high-level purpose.

---

### `feature-intent`

**Signature**:

```lisp
(feature-intent name) → (or intent null)
```

**Purpose**: Get the intent struct for a feature.

**Parameters**:

- `name` — Feature name (symbol)

**Returns**: `intent` struct or `nil` if feature not defined

**Example**:

```lisp
(feature-intent 'rate-limiting)
;; => #S(INTENT :PURPOSE "Prevent API abuse" :GOALS ((:availability ...) ...) ...)
```

---

### `feature-members`

**Signature**:

```lisp
(feature-members feature-name &optional type-filter) → list or plist
```

**Purpose**: Get members (functions, classes, structs, conditions, methods, sub-features) of a feature.

**Parameters**:

- `feature-name` — Feature name (symbol)
- `type-filter` — Optional filter:
  - `nil` (default): Return plist with all types
  - `:functions`: Return list of functions only
  - `:classes`: Return list of classes only
  - `:structs`: Return list of structs only
  - `:conditions`: Return list of conditions only
  - `:methods`: Return list of method specializers only
  - `:features`: Return list of sub-features only

**Returns**:
- Without filter: `(:functions <list> :classes <list> :structs <list> :conditions <list> :methods <list> :features <list>)`
- With filter: list of symbols (or lists for methods)

**Example**:

```lisp
(feature-members 'token-bucket)
;; => (:FUNCTIONS (CHECK-RATE-LIMIT REPLENISH-TOKENS)
;;     :CLASSES (RATE-LIMIT-BUCKET)
;;     :STRUCTS (TOKEN-BUCKET)
;;     :CONDITIONS (RATE-LIMIT-EXCEEDED)
;;     :METHODS ((STORE-DATA MEMORY-BACKEND))
;;     :FEATURES NIL)

(feature-members 'token-bucket :functions)
;; => (CHECK-RATE-LIMIT REPLENISH-TOKENS)

(feature-members 'token-bucket :methods)
;; => ((STORE-DATA MEMORY-BACKEND) (CONSOLIDATE (EQL :AVERAGE)))
```

---

### `feature-parent`

**Signature**:

```lisp
(feature-parent name) → (or symbol null)
```

**Purpose**: Get the parent feature of a feature.

**Parameters**:

- `name` — Feature name (symbol)

**Returns**: Parent feature name (symbol) or `nil` if no parent

**Example**:

```lisp
(feature-parent 'token-bucket)
;; => RATE-LIMITING

(feature-parent 'rate-limiting)
;; => NIL
```

**Equivalent to**: `(intent-belongs-to (feature-intent name))`

---

### `feature-children`

**Signature**:

```lisp
(feature-children name) → list
```

**Purpose**: Get all features that have `name` as their parent.

**Parameters**:

- `name` — Feature name (symbol)

**Returns**: List of feature names (symbols)

**Example**:

```lisp
(feature-children 'rate-limiting)
;; => (TOKEN-BUCKET SLIDING-WINDOW)
```

---

### `list-features`

**Signature**:

```lisp
(list-features &optional filter &key parent under) → list
```

**Purpose**: List features, optionally filtered.

**Parameters**:

- `filter` — Substring to match against name or purpose (case-insensitive)
- `parent` — Only return direct children of this feature
- `under` — Return all descendants of this feature (not yet implemented)

**Returns**: List of feature names (symbols)

**Examples**:

```lisp
;; List all features
(list-features)
;; => (RATE-LIMITING TOKEN-BUCKET USER-AUTHENTICATION ...)

;; Filter by name substring
(list-features "auth")
;; => (USER-AUTHENTICATION TOKEN-AUTHENTICATION)

;; Filter by purpose substring
(list-features "abuse")
;; => (RATE-LIMITING) ; if purpose contains "abuse"

;; List direct children
(list-features nil :parent 'security)
;; => (USER-AUTHENTICATION ENCRYPTION ...)
```

---

## Metaclass

### `intentful-class`

**Type**: Metaclass (subclass of `standard-class`)

**Purpose**: Metaclass for classes that carry intent information.

**Usage**: Applied automatically by `defclass/i`. Can be used directly with `defclass`:

```lisp
(defclass my-class ()
  ((slot-1 :accessor slot-1))
  (:metaclass intentful-class))

;; Set intent after class creation
(setf (class-intent (find-class 'my-class))
      (make-intent :purpose "Example class"))
```

**Slot**: `intent` — Stores the intent struct

**Compatibility**: Works with classes that have `standard-class` superclasses (validated via `c2mop:validate-superclass`).

---

### `class-intent`

**Signature**:

```lisp
(class-intent class-designator) → (or intent null)
(setf (class-intent class-object) intent)
```

**Purpose**: Accessor for intent stored on a class with `intentful-class` metaclass.

**Parameters**:

- `class-designator` — Either:
  - A class object (from `find-class`)
  - A symbol naming the class (convenience method)

**Returns**: `intent` struct or `nil`

**Example**:

```lisp
;; Using symbol directly (recommended)
(class-intent 'rate-limit-bucket)
;; => #S(INTENT :PURPOSE "Store per-user token state" ...)

;; Using class object (also works)
(class-intent (find-class 'rate-limit-bucket))
;; => #S(INTENT :PURPOSE "Store per-user token state" ...)
```

**Note**: For most use cases, prefer `get-intent` which works with all entity types.

---

## Navigation

[← Tutorial](tutorial.md) | [README](../README.md) | [Explanation →](explanation.md) | [Use Cases →](use-cases.md)
