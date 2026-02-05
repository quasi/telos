# Telos API Enhancements

**Date:** 2026-02-05
**Status:** Approved
**Origin:** Field testing with trivial-rrd library (Opus feedback)

## Problem

Four gaps identified when using Telos with real CLOS code:

1. **Method specializers not supported** — `defintent` only accepts symbols, but each method specialization has distinct intent
2. **No defstruct/i** — Structs require `defstruct` + `defintent` separately
3. **No define-condition/i** — Same issue for conditions
4. **class-intent requires class object** — `(class-intent 'name)` fails, must use `(class-intent (find-class 'name))`

## Solution

### 1. class-intent symbol convenience

Add method specializing on `symbol`:

```lisp
(defmethod class-intent ((name symbol))
  (let ((class (find-class name nil)))
    (when (and class (typep class 'intentful-class))
      (slot-value class 'intent))))
```

### 2. defstruct/i

New macro following defun/i pattern:

```lisp
(defstruct/i rrd-archive
  (cf :type keyword)
  (xff :type single-float)
  (steps :type fixnum)
  (rows :type fixnum)
  (data :type (vector single-float))
  (:feature rrd-storage)
  (:role "Circular buffer for consolidated data points"))
```

- Storage: symbol plist (like functions)
- Member type: `:struct`
- Reuse `get-intent` — already checks symbol plist

### 3. define-condition/i

New macro following defclass/i pattern:

```lisp
(define-condition/i metric-not-found (error)
  ((name :initarg :name :reader metric-name))
  (:feature rrd-errors)
  (:role "Signal when requested metric doesn't exist")
  (:report (lambda (c s) (format s "Metric ~A not found" (metric-name c)))))
```

- Storage: symbol plist
- Member type: `:condition`
- Reuse `parse-class-intent-options` for option parsing

### 4. Method specializers in defintent

Extend `defintent` to accept specializer lists:

```lisp
(defintent (consolidate (eql :average))
  :feature rrd-consolidation
  :role "Compute mean for time-averaged metrics")

(defintent (rrd-create memory-backend)
  :feature rrd-memory-backend
  :role "Allocate circular buffer for new metric")
```

- Storage: `*method-intent-registry*` hash table keyed by `(generic-name . specializers)`
- New function: `method-intent` for explicit lookup
- Update `get-intent` to handle lists
- Member type: `:method`

## New Exports

- `defstruct/i`
- `define-condition/i`
- `method-intent`

## Updated Behavior

- `class-intent` — accepts symbols
- `defintent` — accepts method specializer lists
- `feature-members` — returns `:structs`, `:conditions`, `:methods` in addition to existing keys

## Implementation Order (TDD)

1. class-intent symbol method (quick win)
2. defstruct/i macro
3. define-condition/i macro
4. Method specializers in defintent + method-intent query

Each cycle: failing test → minimal implementation → passing test → commit.
