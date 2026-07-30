---
name: telos-dev
description: Development guide for working on the telos intent introspection library
version: 1.0.0
author: quasiLabs
type: dev
---

# Telos Dev Skill

## What is Telos

Intent introspection for Common Lisp. Captures the *why* behind code and makes it queryable at runtime. Developers embed intent metadata into functions, classes, structs, conditions, and features, then query it programmatically or via MCP tools.

**Dependencies**: `closer-mop` (runtime), `fiveam` (tests only)

## Quick Reference

```lisp
;; Load
(asdf:load-system :telos)

;; Test (all)
(asdf:test-system :telos)

;; Test (specific suite) via Lisp MCP
(5am:run! :intent-tests)
(5am:run! :feature-tests)
(5am:run! :function-tests)
(5am:run! :class-tests)
(5am:run! :struct-tests)
(5am:run! :condition-tests)
(5am:run! :method-tests)
(5am:run! :query-tests)
(5am:run! :decision-tests)
(5am:run! :validation-tests)
```

## Architecture

### Core Data Type: Intent Struct

```lisp
(defstruct intent
  purpose        ; STRING: why this code exists
  failure-modes  ; LIST: what can go wrong, linked to goals
  goals          ; LIST: success criteria
  constraints    ; LIST: boundaries/non-negotiables
  assumptions    ; LIST: world assumptions
  verification   ; LIST: how to verify correctness
  belongs-to     ; SYMBOL: parent feature
  role           ; STRING: role within parent feature
  members)       ; LIST: child functions/classes
```

### Storage Mechanisms

| Entity | Storage |
|--------|---------|
| Features | `*feature-registry*` hash table |
| Function intent | Symbol plist: `(get 'fn-name 'telos:intent)` |
| Struct intent | Symbol plist: `(get 'struct-name 'telos:intent)` |
| Condition intent | Symbol plist: `(get 'condition-name 'telos:intent)` |
| Class intent (defclass/i) | Metaclass slot via `class-intent` accessor |
| Class intent (retrofitted) | `*class-intent-registry*` hash table |
| Method intent | `*method-intent-registry*` hash table |
| Feature members | `*feature-members*` hash table |

### Module Load Order

```
package → validation → intent → storage → decision → feature → function → class → struct → condition → query
```

## Key Macros

| Macro | Purpose |
|-------|---------|
| `deffeature` | Define a feature with intent |
| `defun/i` | Define a function with embedded intent |
| `defclass/i` | Define a class with intent via metaclass |
| `defstruct/i` | Define a struct with embedded intent |
| `define-condition/i` | Define a condition with embedded intent |
| `defintent` | Retrofit intent onto existing definitions |
| `record-decision` | Record a design decision for a feature |

All definition macros validate what they were given at macroexpansion time, via
`src/validation.lisp`, signalling `invalid-intent-declaration` rather than dropping anything:

| Layer | Checked by | Table to extend |
|-------|-----------|-----------------|
| Nested entries (goals, failure modes, …) | `validate-intent-fields` | `*intent-entry-option-keys*` |
| Decision plists | `validate-decision-entries` | `*decision-keys*` |
| Top-level clauses in `defun/i`, `defstruct/i`, `defclass/i`, `define-condition/i` | `invalid-intent-clause` from each parser's `case` `otherwise` | `*intent-clause-keys*` |
| Forwarded `defclass` / `define-condition` options | `parse-class-intent-options` | `*defclass-passthrough-options*`, `*define-condition-passthrough-options*` |

`deffeature` and `defintent` get top-level strictness free from their `&key` lambda lists.
If you add a nested field, add it to `*intent-entry-option-keys*` — an unlisted field is not
validated at all. If you add a clause, add it to both the parser's `case` and
`*intent-clause-keys*` (the latter only feeds the error message).

## Query API

| Function | Purpose |
|----------|---------|
| `intent-chain` | Trace from function/class up to root feature |
| `feature-members` | Get all code belonging to a feature |
| `get-intent` | Get intent for function, class, or method |
| `feature-intent` | Get intent for a feature |
| `intent-feature` | Quick lookup: which feature owns this? |
| `class-intent` | Get intent for a class |
| `feature-decisions` | Get decisions for a feature |
| `list-decisions` | All decisions across features |

## Conventions

### Intent Clause Syntax

After docstring/declarations, before body:
```lisp
(defun/i my-function (args)
  "Optional docstring"
  (:feature parent-feature)
  (:role "What this function does in the feature")
  (:failure-modes ((:id "description" :violates :goal-id)))
  (actual-body))
```

### Feature Hierarchy

Features form a tree via `:belongs-to`:
```lisp
(deffeature security :purpose "Top-level security")
(deffeature user-auth :purpose "Verify identity" :belongs-to security)
```

### Naming

- Features: kebab-case symbols
- Failure mode IDs: keywords (`:timing-attack`)
- Goal IDs: keywords (`:secure`, `:usable`)

## Test Structure

Test package: `:telos/tests` | Root suite: `:telos-tests`

```
tests/
  package.lisp, intent-test.lisp, feature-test.lisp,
  function-test.lisp, class-test.lisp, struct-test.lisp,
  condition-test.lisp, method-test.lisp, query-test.lisp,
  decision-test.lisp, validation-test.lisp
```

## TDD Workflow

1. Write failing test in `tests/*-test.lisp`
2. Run `(5am:run! :telos-tests)` via Lisp MCP — confirm failure
3. Implement minimal code in `src/*.lisp`
4. Run tests — confirm pass
5. Commit

## Documentation

Human docs in `docs/` (not agent docs):
- `docs/tutorial.md` — hands-on rate limiter guide
- `docs/explanation.md` — design rationale
- `docs/reference.md` — complete API reference
- `docs/use-cases.md` — real-world scenarios
