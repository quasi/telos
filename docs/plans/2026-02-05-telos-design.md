# Telos: Intent Introspection for Common Lisp

**Date:** 2026-02-05
**Status:** Design
**Author:** quasi + Claude

## Overview

Telos is a Common Lisp library for declaring and querying **intent** — the "why" behind code, not just the "what" or "how."

Documentation tells humans what code does. Telos tells machines (LLMs) why code exists, enabling reasoning about purpose, detecting when intent is violated, and supporting self-healing systems.

## Motivation

Systems can describe their behavior. They cannot explain their purpose.

**The chain:**
```
Intent (WHY) → Design Decisions → Implementation → Behavior (observable)
```

We record the bottom two layers well. Code IS implementation. Tests document behavior. But intent evaporates the moment fingers leave keyboard.

This matters because **behavior can be correct while intent is violated**. A rate limiter blocking a legitimate power user is behaving correctly (following rules) but violating intent (allow legitimate heavy use). Without access to intent, debugging systems — human or AI — cannot distinguish these cases.

**Telos makes intent queryable.**

## Core Concepts

### Three-Level Hierarchy

```
FEATURE (semantic unit — where primary intent lives)
    ↓ contains
FUNCTION/CLASS (implementation — has role within feature)
    ↓ can nest
SUB-FEATURE (features can contain features)
```

**Key insight:** Individual functions have *role* (what they do within feature). Features have *purpose* (why the group exists). An LLM queries function → gets routed to feature → understands the why.

### Intent Structure

```lisp
(defstruct intent
  ;; Required
  purpose          ; string — "why does this exist?"

  ;; Optional but valuable
  failure-modes    ; list — "what does broken look like?"
                   ;   ((:id "description" :violates goal/constraint))

  ;; Optional
  goals            ; list — "what does success look like?"
                   ;   ((:id "description"))
  constraints      ; list — "boundaries that must be respected"
                   ;   ((:id "description"))
  assumptions      ; list — "what this believes about the world"
                   ;   ((:id "description"))
  verification     ; list — "how to check if intent is met"
                   ;   ((:id "description" :metric "..." :threshold "..."))

  ;; Structural (for hierarchy)
  belongs-to       ; symbol — parent feature (nil for top-level)
  role             ; string — "what part do I play in parent's purpose?"
  members)         ; list — functions/classes/sub-features (features only)
```

**Validation:** Only `purpose` is required. Everything else is optional.

**Usage patterns:**
- Quick annotation: just `purpose`
- Debugging-focused: `purpose` + `failure-modes`
- Full specification: all fields

## API

### Defining Features

```lisp
(deffeature rate-limiting
  :purpose "Protect system from abuse while allowing legitimate heavy use"
  :goals ((:block-abuse "No single actor can overwhelm system")
          (:allow-legitimate "Authenticated users never blocked for legitimate work"))
  :constraints ((:paid-priority "Paid users get higher limits"))
  :assumptions ((:mostly-legitimate "99%+ of traffic is legitimate"))
  :failure-modes ((:legitimate-blocked "Paid user gets 429" :violates :allow-legitimate))
  :belongs-to api-layer)  ; optional parent feature
```

### Defining Functions (New Code)

```lisp
(defun/i check-rate (user-id)
  "Check if USER-ID is within rate limits."  ; regular docstring
  (:feature rate-limiting)
  (:role "Gatekeeper — first check before any rate-limited operation")
  (:failure-modes ((:false-positive "Returns exceeded for legitimate use")))
  ;; body
  (let ((count (get-request-count user-id)))
    (< count (get-limit user-id))))
```

### Defining Classes (New Code)

```lisp
(defclass/i rate-limiter ()
  ((limits :initarg :limits :accessor limits))
  (:metaclass intentful-class)
  (:feature rate-limiting)
  (:purpose "Configuration and state for rate limiting")
  (:role "Holds per-tier limits and current request counts"))
```

### Retrofitting Existing Code

```lisp
;; Works for both functions and classes
(defintent check-rate
  :feature rate-limiting
  :role "Gatekeeper — first check before any rate-limited operation")

(defintent rate-limiter
  :feature rate-limiting
  :purpose "Configuration and state for rate limiting"
  :role "Holds per-tier limits and current request counts")
```

Uses registry storage — does not modify original definitions.

### Querying Features

```lisp
;; List features (with optional filtering)
(list-features)                        ; all features
(list-features "rate")                 ; substring match on name
(list-features "abuse")                ; also matches purpose text
(list-features :parent 'api-layer)     ; direct children
(list-features :under 'api-layer)      ; all descendants
(list-features "rate" :under 'api-layer) ; combined

;; Get feature intent
(feature-intent 'rate-limiting)
→ #S(INTENT :PURPOSE "Protect system from abuse..."
            :FAILURE-MODES ((:LEGITIMATE-BLOCKED ...))
            :BELONGS-TO API-LAYER
            ...)

;; Get members
(feature-members 'rate-limiting)           ; all
(feature-members 'rate-limiting :functions) ; just functions
(feature-members 'rate-limiting :classes)   ; just classes
(feature-members 'rate-limiting "check")    ; name matching

;; Navigate hierarchy
(feature-parent 'rate-limiting)   → api-layer
(feature-children 'api-layer)     → (rate-limiting authentication ...)
```

### Querying Functions/Classes

```lisp
;; Get intent
(get-intent 'check-rate)
→ #S(INTENT :PURPOSE NIL
            :ROLE "Gatekeeper..."
            :BELONGS-TO RATE-LIMITING
            :FAILURE-MODES ...)

;; Quick lookup
(intent-feature 'check-rate) → rate-limiting

;; Full context chain (what LLM typically wants)
(intent-chain 'check-rate)
→ ((:function check-rate :role "Gatekeeper..." :failure-modes ...)
   (:feature rate-limiting :purpose "Protect from abuse..." ...)
   (:feature api-layer :purpose "Public HTTP interface" ...))
```

## Storage

### Feature Registry

```lisp
(defvar *feature-registry* (make-hash-table :test 'eq)
  "Maps feature-name (symbol) → intent struct")
```

### Function Intent (Symbol Plist)

```lisp
;; Storage
(setf (get 'check-rate 'telos:intent) <intent-struct>)

;; Retrieval
(get 'check-rate 'telos:intent)
```

### Class Intent (New Classes via Metaclass)

```lisp
(defclass intentful-class (standard-class)
  ((intent :accessor class-intent :initarg :intent :initform nil)))

(defmethod validate-superclass ((class intentful-class) (super standard-class))
  t)
```

### Class Intent (Retrofit via Registry)

```lisp
(defvar *class-intent-registry* (make-hash-table :test 'eq)
  "Maps class-name (symbol) → intent struct. Used for retrofitted classes.")
```

### Unified Getter

```lisp
(defun get-intent (name)
  "Get intent for function or class NAME."
  (cond
    ;; Class with intentful metaclass
    ((and (find-class name nil)
          (typep (find-class name) 'intentful-class))
     (class-intent (find-class name)))
    ;; Class registry (retrofit)
    ((gethash name *class-intent-registry*))
    ;; Symbol plist (function)
    ((get name 'telos:intent))
    ;; Not found
    (t nil)))
```

## Project Structure

```
telos/
├── telos.asd              ; ASDF system definition
├── src/
│   ├── package.lisp       ; package definition, exports
│   ├── intent.lisp        ; intent struct, accessors
│   ├── storage.lisp       ; registries, metaclass, getters
│   ├── feature.lisp       ; deffeature, feature queries
│   ├── function.lisp      ; defun/i, defintent (for functions)
│   ├── class.lisp         ; defclass/i, defintent (for classes)
│   └── query.lisp         ; list-features, intent-chain, search
└── tests/
    ├── package.lisp
    ├── intent-test.lisp
    ├── feature-test.lisp
    └── query-test.lisp
```

## Package Exports

```lisp
(defpackage :telos
  (:use :cl)
  (:export
   ;; Struct
   #:intent #:make-intent
   #:intent-purpose #:intent-failure-modes #:intent-goals
   #:intent-constraints #:intent-assumptions #:intent-verification
   #:intent-belongs-to #:intent-role #:intent-members

   ;; Definition macros
   #:deffeature
   #:defun/i
   #:defclass/i
   #:defintent

   ;; Query API
   #:get-intent
   #:feature-intent
   #:feature-members
   #:feature-parent
   #:feature-children
   #:intent-feature
   #:intent-chain
   #:list-features

   ;; Metaclass
   #:intentful-class
   #:class-intent))
```

## Dependencies

- `closer-mop` — portable metaclass operations

No other dependencies.

## Design Decisions

### Why symbol plist for functions (not MOP)?

Functions in CL are not easily extensible objects. Using MOP for functions requires funcallable instances and a custom `defun` replacement. Symbol plists are:
- Two lines of code
- Built into CL
- Intent travels with the symbol
- No wrapper objects

### Why metaclass for classes?

Unlike functions, classes naturally support metaclasses. Intent as a slot on the metaclass:
- Keeps intent with the class definition
- Discoverable via standard introspection
- Natural fit for CLOS

### Why registry for retrofit?

Changing an existing class's metaclass is invasive and may break things. A separate registry for retrofitted classes:
- Does not modify original definitions
- Satisfies "must not break non-LLM flows"
- Same query API works for both

### Why features instead of just functions?

Intent at function level is little different from docstrings. The value emerges at the **semantic group** level — where multiple functions together serve a purpose greater than their individual roles.

Features are the semantic unit. Functions are entry points that route to features.

## Future Directions (Not in Prototype)

1. **Condition system integration** — automatically include intent chain when errors are signaled
2. **Export to external formats** — dump intent graph to JSON/YAML for external tools
3. **Semantic search** — embed purpose strings for similarity search
4. **Canon integration** — generate intent from Canon specs or vice versa
5. **Intent validation** — warn when function claims feature membership but feature doesn't list it

## Example: Complete Rate Limiting

```lisp
;;; Feature definition
(deffeature rate-limiting
  :purpose "Protect system from abuse while allowing legitimate heavy use"
  :belongs-to api-layer
  :goals
  ((:block-abuse "No single actor can overwhelm system resources")
   (:allow-legitimate "Authenticated users performing legitimate work are never blocked"))
  :constraints
  ((:paid-priority "Paid users receive higher limits than free tier")
   (:graceful-degradation "When limits hit, inform user of wait time"))
  :assumptions
  ((:mostly-legitimate "99%+ of traffic is legitimate")
   (:abuse-is-automated "Abuse comes from bots, not humans manually"))
  :failure-modes
  ((:legitimate-blocked "Paid user receives 429" :violates :allow-legitimate)
   (:abuse-succeeds "System overloaded despite limits" :violates :block-abuse))
  :verification
  ((:false-positive-rate "< 0.1% of paid user requests rejected")
   (:abuse-blocked-rate "> 99% of detected abuse patterns blocked")))

;;; Functions with intent
(defun/i check-rate (user-id)
  "Check if USER-ID is within rate limits."
  (:feature rate-limiting)
  (:role "Gatekeeper — first check before any rate-limited operation")
  (let ((count (get-request-count user-id))
        (limit (get-limit user-id)))
    (< count limit)))

(defun/i track-request (user-id)
  "Record a request for USER-ID."
  (:feature rate-limiting)
  (:role "Bookkeeper — maintains accurate request counts")
  (incf (get-request-count user-id)))

(defun/i get-limit (user-id)
  "Get rate limit for USER-ID based on their tier."
  (:feature rate-limiting)
  (:role "Policy — determines limits based on user tier")
  (let ((tier (user-tier user-id)))
    (ecase tier
      (:free 100)
      (:paid 10000)
      (:enterprise nil))))  ; unlimited

;;; LLM debugging session
(intent-chain 'check-rate)
→ ((:function check-rate
    :role "Gatekeeper — first check before any rate-limited operation")
   (:feature rate-limiting
    :purpose "Protect system from abuse while allowing legitimate heavy use"
    :failure-modes ((:legitimate-blocked "Paid user receives 429"
                     :violates :allow-legitimate) ...))
   (:feature api-layer
    :purpose "Public HTTP interface for external clients"))

;; LLM can now reason:
;; "Error was rate-limit-exceeded for a paid user.
;;  This matches failure-mode :legitimate-blocked.
;;  Intent is VIOLATED — this is a bug, not expected behavior."
```
