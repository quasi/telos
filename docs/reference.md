# API Reference

Complete reference for all Telos functions, macros, and data structures.

---

## Table of Contents

- [Data Structures](#data-structures)
  - [intent](#intent-struct)
  - [Entry accessors](#entry-accessors)
- [Definition Macros](#definition-macros)
  - [deffeature](#deffeature)
  - [defun/i](#defuni)
  - [defclass/i](#defclassi)
  - [defstruct/i](#defstructi)
  - [define-condition/i](#define-conditioni)
  - [defintent](#defintent)
  - [define-entry-option](#define-entry-option)
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
- [Audit](#audit)
  - [check-intent-references](#check-intent-references)
  - [assert-intent-references](#assert-intent-references)
  - [all-intentful-classes](#all-intentful-classes)
- [Conditions](#conditions)
  - [invalid-intent-declaration](#invalid-intent-declaration)
  - [intent-reference-error](#intent-reference-error)

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

### Entry accessors

The entries inside `intent-goals`, `intent-failure-modes` and the rest are literal lists of
the shape `(:id)` or `(:id "description" . options)`. These read that shape so you do not have
to know it.

```lisp
(intent-entry-id entry)          → keyword or nil
(intent-entry-description entry) → string or nil     ; nil for the bare (:id) shape
(intent-entry-option entry key)  → value or nil      ; e.g. :violates, :mitigation
```

All three are read-side and total. Something that is not an entry — `nil`, a bare keyword, a
dotted, circular, or odd-length option tail — simply has no id, description, or options; none
of them signals the way a bare `getf` would, and none of them loops. That matters because
`make-intent` is exported and
validates nothing, and because `check-intent-references` sweeps every intent in the image,
where one malformed entry must not take the whole audit down.

The `intent-entry-` prefix is deliberate: `(defstruct entry id description ...)` is an
ordinary thing for a downstream project to write, and under the shorter names it would clobber
these with nothing but a warning.

```lisp
(let ((mode (first (intent-failure-modes (feature-intent 'recovery-api)))))
  (list (intent-entry-id mode)
        (intent-entry-description mode)
        (intent-entry-option mode :mitigation)))
;; => (:STALE-RESPONSE "A response arrives after the option has expired"
;;     "%execute-recovery-response checks expires-at")
```

---

## Definition Macros

### `deffeature`

**Signature**:

```lisp
(deffeature name &key purpose goals constraints assumptions
                      failure-modes verification belongs-to decisions)
```

**Purpose**: Define a feature with structured intent.

**Parameters**:

- `name` — Symbol naming the feature
- `purpose` — String describing why this feature exists (recommended)
- `goals` — List of `(:id "description")` for success criteria
- `constraints` — List of `(:id "description")` for boundaries
- `assumptions` — List of `(:id "description")` for world assumptions
- `failure-modes` — List of `(:id "description" :violates :goal-id :mitigation "how to recover")`
  for failure scenarios. `:violates` names a **goal** id — goals only, not constraints —
  declared either here or on any ancestor feature. It is not resolved at macroexpansion time:
  the ancestor may not be defined yet, and a member legitimately violates a goal declared on
  its parent feature. `:mitigation` describes how to recover from the failure; read it with
  `intent-entry-option`. Both are optional and independent.
- `verification` — List of `(:id "description")` for verification methods
- `belongs-to` — Parent feature symbol (optional, creates hierarchy)
- `decisions` — List of decision plists inline: `:id` (keyword), `:chose` (string), `:over`
  (list of strings), `:because` (string), `:date` (string), `:decided-by` (string). All values
  are literal — use `record-decision` for a computed decision.

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

**Errors**: Unknown top-level keywords are rejected by the macro's `&key` lambda list.
Unknown keys *inside* the nested entries — goals, constraints, assumptions, failure modes,
verification, decisions — signal `invalid-intent-declaration` at macroexpansion time, as do
duplicate keys and malformed entry shapes. Goal, constraint, assumption and verification
entries accept no keyword options; failure modes accept `:violates` and `:mitigation`;
decisions accept `:id`, `:chose`, `:over`, `:because`, `:date`, `:decided-by`. Nothing you
write into a declaration is silently discarded. Use `define-entry-option` to add a keyword
option your own project needs.

**See Also**: `feature-intent`, `feature-parent`, `feature-children`,
`invalid-intent-declaration`

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

**Errors**: An unrecognized clause — `(:purpsoe "...")`, `(:failure-mode ...)` — signals
`invalid-intent-declaration` at macroexpansion time, as do unknown keys inside the nested
entries. This matters more here than elsewhere: clauses are popped off the front of the body,
so a swallowed clause used to take a body form with it and leave the function returning `nil`.
A keyword-headed form is never a legal Common Lisp body form, so nothing valid is rejected.

**See Also**: `get-intent`, `intent-feature`, `defintent`, `invalid-intent-declaration`

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

**Errors**: An option that is neither an intent clause nor a standard `defclass` option
(`:documentation`, `:default-initargs`, `:metaclass`) signals `invalid-intent-declaration` at
macroexpansion time, as do unknown keys inside the nested entries. `:metaclass` gets its own
dedicated error, since `defclass/i` always uses `intentful-class`.

**See Also**: `intentful-class`, `class-intent`, `get-intent`, `invalid-intent-declaration`

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

**Errors**: A keyword-headed clause that is not a recognized intent clause signals
`invalid-intent-declaration` at macroexpansion time, as do unknown keys inside the nested
entries. Struct slots are never keyword-headed, so a keyword-headed clause is always intent —
which is why a misspelled one used to vanish from both the intent and the slot list.

**See Also**: `get-intent`, `feature-members`, `invalid-intent-declaration`

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

**Errors**: An option that is neither an intent clause nor a standard `define-condition`
option (`:documentation`, `:default-initargs`, `:report`) signals
`invalid-intent-declaration` at macroexpansion time, as do unknown keys inside the nested
entries.

**See Also**: `get-intent`, `feature-members`, `invalid-intent-declaration`

---

### `defintent`

**Signature**:

```lisp
(defintent name &key feature role purpose failure-modes
                     goals constraints assumptions verification)
```

**Purpose**: Add intent to an existing function, class, or method without rewriting it.

**Parameters**:

- `name` — One of:
  - Symbol naming an existing function or class when the name is unambiguous
  - Typed entity spec `(:function symbol)` or `(:class symbol)` when a name exists in multiple namespaces
  - List for method specializers:
  - `(generic-name specializer1 specializer2 ...)` where specializers are class names or `(eql value)`
- `feature` — Which feature this belongs to
- `role` — Role within the feature
- `purpose` — Why this exists
- Other fields same as `deffeature`

**Returns**: `name`

**Behavior**:
1. For unambiguous symbols: stores intent on the appropriate registry automatically
2. For typed entity specs: stores intent on the registry for that entity kind
3. For method specs: stores intent in method registry (keyed by specializer list)
4. Registers as member of feature (if `feature` specified)
5. Does not modify the function, class, or method definition itself

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

**Errors**: Unknown top-level keywords are rejected by the macro's `&key` lambda list;
unknown keys inside the nested entries signal `invalid-intent-declaration` at macroexpansion
time.

**See Also**: `get-intent`, `method-intent`, `invalid-intent-declaration`

---

### `define-entry-option`

**Signature**:

```lisp
(define-entry-option field &rest keys) → keys
```

**Purpose**: Widen the keyword options an intent field accepts, so a project can name a field
Telos never thought of without waiting for a Telos release.

**Parameters**:

- `field` — One of `:goals`, `:constraints`, `:assumptions`, `:verification`,
  `:failure-modes`. Anything else is an error: a key added to a field that does not exist
  would never be consulted.
- `keys` — Keywords to accept as entry options on that field.

**Example**:

```lisp
(define-entry-option :failure-modes :detected-by :severity)

(deffeature ingest
  :purpose "Load the day's files"
  :failure-modes ((:partial "Half the rows landed"
                   :severity :high
                   :detected-by "row-count check")))
```

Read the values back with `intent-entry-option`.

**Returns**: `keys` — the literal list from the macroexpansion, so treat it as read-only.

**Timing**: declarations are validated as they are macroexpanded, so this takes effect for
everything compiled *after* it. Put it in a file that loads before the declarations that use
it — the macro wraps itself in `eval-when`, so it also applies to the rest of its own file and
survives into a fasl compiled against it.

**Reloading**: the table is a `defparameter`, so `(asdf:load-system :telos :force t)` resets
it to the built-in vocabulary and your extensions are gone until the file declaring them is
reloaded too. This bites in the interactive loop, not in a fresh image, where load order does
the right thing on its own.

**Note**: extending is deliberate, and strictness is unchanged. An unknown key is still an
error, so a typo in your own vocabulary is caught exactly like a typo in Telos's. A typo in
the *field* is caught too, even with no keys after it. `add-entry-option` and
`add-entry-options` are the functions underneath, for when the field and keys are computed;
neither they nor the macro take a lock, so extend at load time rather than from several
threads of a running system.

**See Also**: `intent-entry-option`, `invalid-intent-declaration`

---

## Query API

### `get-intent`

**Signature**:

```lisp
(get-intent name) → (or intent null)
```

**Purpose**: Get intent for a function, class, struct, condition, or method.

**Parameters**:

- `name` — One of:
  - Symbol naming a function/class/struct/condition when the name is unambiguous
  - Typed entity spec `(:function symbol)`, `(:class symbol)`, `(:struct symbol)`, or `(:condition symbol)`
  - List for method specializers:
  - `(generic-name specializer1 ...)` for method intent

**Returns**: `intent` struct or `nil` if not found

**Behavior**: Checks multiple locations based on name type:

For lists (method specializers):
1. Method registry

For typed entity specs:
1. Entity registry for functions, structs, and conditions
2. Class metaclass or class registry for classes

For plain symbols:
1. Resolves the symbol to exactly one intent-bearing entity kind
2. Signals an error if the symbol is ambiguous across namespaces

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

**Example (explicit class)**:

```lisp
(get-intent '(:class token-bucket))
;; => #S(INTENT :PURPOSE "Class-level intent" ...)
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

- `name` — Symbol, typed entity spec, or method spec accepted by `get-intent`

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

**Purpose**: Get full intent chain from an entity up to root feature.

**Parameters**:

- `name` — Symbol, typed entity spec, or method spec accepted by `get-intent`

**Returns**: List of plists, each representing one level of the hierarchy. Returns `nil` if `name` has no intent.

**Plist Format**:

```lisp
(:type <:function | :class | :struct | :condition | :method | :feature>
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

**Note**: Membership is verified on read. Registration only ever adds, so re-declaring an
entity under a second feature would otherwise leave it listed under both; each member's own
intent is consulted, and a member that has moved away is not reported here.

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
(list-features [&optional filter] &key filter parent under) → list
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
(list-features :parent 'security)
;; => (USER-AUTHENTICATION ENCRYPTION ...)

;; Keyword filter also works
(list-features :filter "auth" :parent 'security)
;; => (USER-AUTHENTICATION)
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

## Audit

Local shape is strict at macroexpansion time. Cross-declaration topology — does this
`:violates` name a real goal, does this `:belongs-to` exist — cannot be judged there, because
the feature referred to may not be defined yet and load order is arbitrary. That is what the
audit is for.

### `check-intent-references`

**Signature**:

```lisp
(check-intent-references) → list of findings, or nil
```

**Purpose**: Report cross-declaration mistakes in the intent graph, over the finished image.

**Returns**: A deterministically ordered list of plists, or `nil` when everything resolves.
Registry iteration is hash-ordered; the report is sorted so a human or a CI diff sees a stable
result.

**Finding keys**:

| Key | Value |
|-----|-------|
| `:severity` | `:error` |
| `:code` | `:dangling-violates`, `:undefined-parent`, or `:cyclic-hierarchy` |
| `:entity` | The feature, function, class, struct, condition, or method spec |
| `:entity-type` | `:feature`, `:function`, `:class`, `:struct`, `:condition`, `:method` |
| `:reference` | The unresolved goal id or feature name |
| `:message` | A sentence for a human or an agent |

The codes are API — dispatch on them rather than on the message.

**Never signals.** A partial or in-progress load legitimately shows dangling references; that
means the rest has not loaded yet, not that anything is wrong. Use `assert-intent-references`
when you want failure.

**Coverage**: features, functions, structs, conditions, methods, retrofitted classes, and
classes defined with `defclass/i` — including a `defclass/i` with no `:feature`, which appears
in no registry and is enumerated via the metaclass instead.

**Example**:

```lisp
(deffeature security :purpose "Top" :goals ((:secure "No unauthorized access")))
(deffeature login :purpose "Log in" :belongs-to security
  :goals ((:fast "Under 2s"))
  :failure-modes ((:slow "Takes ages" :violates :fast)
                  (:leak "Password leaked" :violates :secrue)))   ; typo

(check-intent-references)
;; => ((:severity :error :code :dangling-violates :entity login :entity-type :feature
;;      :reference :secrue
;;      :message "Failure mode :LEAK of LOGIN violates :SECRUE, which is not a goal of
;;                LOGIN or of any feature it belongs to."))
```

`:slow` is not reported, and neither is a failure mode that violates a goal declared on a
parent or grandparent feature — that is the documented pattern, not an exception.

**See Also**: `assert-intent-references`, `intent-reference-error`

---

### `assert-intent-references`

**Signature**:

```lisp
(assert-intent-references) → nil
```

**Purpose**: Signal `intent-reference-error` if `check-intent-references` finds anything. One
line for a test or a CI step.

**Example**:

```lisp
(test intent-graph-resolves
  (finishes (assert-intent-references)))
```

**See Also**: `check-intent-references`

---

### `all-intentful-classes`

**Signature**:

```lisp
(all-intentful-classes) → list of symbols
```

**Purpose**: Names of classes currently defined with the `intentful-class` metaclass.

**Note**: `defclass/i` stores intent on the class metaobject, so such classes appear in no
registry. The index behind this function is a candidate list, never truth: every name is
re-derived through `find-class`, so a name that no longer denotes an intentful class is
dropped here rather than reported as a phantom.

---

## Conditions

### `invalid-intent-declaration`

**Type**: Condition (subclass of `program-error`, hence of `error`)

**Purpose**: Signalled at macroexpansion time when a nested plist in an intent declaration
is malformed or carries a key the declaration does not understand. Every definition macro
(`deffeature`, `defun/i`, `defclass/i`, `defstruct/i`, `define-condition/i`, `defintent`)
validates its nested entries this way, so an unrecognized key is never silently dropped.

**Readers**:

| Reader | Value |
|--------|-------|
| `invalid-intent-declaration-context` | String describing the declaration, e.g. `"DEFFEATURE USER-AUTH"` |
| `invalid-intent-declaration-field` | The field the bad entry came from, e.g. `:goals`, `:decisions` |
| `invalid-intent-declaration-entry` | The offending entry, as written |
| `invalid-intent-declaration-key` | The offending keyword (when `reason` is `:unknown-key` or `:non-keyword-key`) |
| `invalid-intent-declaration-expected` | The keywords this kind of entry accepts |
| `invalid-intent-declaration-reason` | `:unknown-key`, `:unknown-clause`, `:clause-arity`, `:duplicate-key`, `:non-keyword-key`, `:options-before-description`, `:odd-plist`, `:not-a-list`, `:field-not-a-list`, `:unevaluated-form`, `:value-type`, `:duplicate-entry-id`, `:unknown-entry-field`, `:invalid-option-key` |

**Accepted keys per field**:

| Field | Entry shape | Keyword options |
|-------|-------------|-----------------|
| `goals`, `constraints`, `assumptions`, `verification` | `(:id "description")` | none |
| `failure-modes` | `(:id "description" :violates :goal-id :mitigation "...")` | `:violates`, `:mitigation` |
| `decisions` | plist | `:id`, `:chose`, `:over`, `:because`, `:date`, `:decided-by` |

Widen this table for your own project with [`define-entry-option`](#define-entry-option).

**Entry shape**: `(:id)` or `(:id "description" . options)`. The description may be omitted,
but a keyword in its place is rejected rather than guessed at — `(:f1 :violates :g1)` would
read as an option here and as a description to any consumer taking `(second entry)`.

**Nested structured fields are literal data; scalar top-level fields may be computed.** That
is the boundary: `:purpose` and `:role` are evaluated, while `:goals`, `:constraints`,
`:assumptions`, `:verification`, `:failure-modes` and `:decisions` are taken exactly as
written. A form where data belongs — `:over (list "a" "b")`, `:goals (mapcar #'f xs)` — is
rejected rather than stored unevaluated. `record-decision` is the computed path.

**Decision value types** are checked at macroexpansion time against the `decision` struct's
slot types, so the message names the field and the declaration instead of surfacing as a
`make-decision` type error at load time. `:over` must be a list of *strings*.

**Also rejected**: two entries in one field sharing an id — `:violates` could not say
which one it meant, and one description would be hidden. Ids are compared within a field, so a
goal and a constraint may share one.

**Also rejected**: a key given twice (the second value would be dropped by `getf`), a
dangling key with no value, a dotted or non-list entry, a field value that is not a list of
entries, a clause carrying more than one value (`(:goals (...) (...))` — the second list would
be dropped), and a field value that looks like a form to evaluate (`:goals '((:g1 "d"))`) —
field values are taken literally, never evaluated.

**Example**:

```lisp
(deffeature probe
  :purpose "Demonstrate strictness"
  :failure-modes ((:fm1 "a failure" :cause "swallowed")))
;; => In DEFFEATURE PROBE, :FAILURE-MODES entry (:FM1 "a failure" :CAUSE "swallowed"):
;;    unknown keyword: :CAUSE; expected one of :VIOLATES, :MITIGATION

(defun/i f (x)
  (:purpose "p")
  (:failure-mode ((:fm1 "typo in the clause name")))
  x)
;; => In DEFUN/I F, clause (:FAILURE-MODE ((:FM1 "typo in the clause name"))):
;;    unknown intent clause: :FAILURE-MODE; expected one of :FEATURE, :ROLE, :PURPOSE,
;;    :FAILURE-MODES, :GOALS, :CONSTRAINTS, :ASSUMPTIONS, :VERIFICATION
```

**Note**: Rationale for a decision goes in `:because`; constraints belong at feature level
(`intent-constraints`), not on a decision.

---

### `intent-reference-error`

**Type**: Condition (subclass of `error`)

**Purpose**: Signalled by `assert-intent-references` when the intent graph has unresolved
references.

**Readers**: `intent-reference-error-findings` — the findings list from
`check-intent-references`, so a handler can report or filter them.

---

## Navigation

[← Tutorial](tutorial.md) | [README](../README.md) | [Explanation →](explanation.md) | [Use Cases →](use-cases.md)
