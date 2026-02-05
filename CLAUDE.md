# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Telos?

Telos is a Common Lisp library for **intent introspection**—capturing the *why* behind code and making it queryable at runtime. Instead of scattering rationale across commit messages and comments, developers embed queryable intent metadata directly into functions, classes, and features.

## Build & Test Commands

```lisp
;; Load the system
(asdf:load-system :telos)

;; Run all tests (89 tests across 5 suites)
(asdf:test-system :telos)

;; Or from shell
sbcl --eval "(asdf:test-system :telos)" --quit

;; Run specific test suite in REPL
(5am:run! :intent-tests)     ; Intent struct tests
(5am:run! :feature-tests)    ; Feature definition/hierarchy tests
(5am:run! :function-tests)   ; defun/i, defintent tests
(5am:run! :class-tests)      ; defclass/i, metaclass tests
(5am:run! :query-tests)      ; intent-chain, feature-members tests
```

**Dependencies**: `closer-mop` (runtime), `fiveam` (tests only)

## Architecture Overview

### Core Abstraction: Intent Struct

The `intent` struct is the foundational data type:

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

| Entity | Storage Location |
|--------|-----------------|
| Features | `*feature-registry*` hash table |
| Function intent | Symbol plist: `(get 'fn-name 'telos:intent)` |
| Class intent (defclass/i) | Metaclass slot via `class-intent` accessor |
| Class intent (retrofitted) | `*class-intent-registry*` hash table |
| Feature members | `*feature-members*` hash table |

### Module Dependencies

```
package.lisp → intent.lisp → storage.lisp → feature.lisp → function.lisp → class.lisp → query.lisp
```

All modules load serially via ASDF.

### Key Macros

- `deffeature` — Define a feature with intent (registers in `*feature-registry*`)
- `defun/i` — Define a function with embedded intent clauses
- `defclass/i` — Define a class with intent via `intentful-class` metaclass
- `defintent` — Retrofit intent onto existing functions/classes

### Query API

- `intent-chain(name)` — Trace from function/class up to root feature
- `feature-members(name &optional type)` — Get functions, classes, sub-features
- `get-intent(name)` — Get intent for any function or class
- `feature-intent(name)` — Get intent for a feature
- `intent-feature(name)` — Quick lookup of which feature something belongs to

## Conventions

### Intent Clause Syntax in defun/i

Intent clauses come after docstring/declarations, before function body:

```lisp
(defun/i my-function (args)
  "Optional docstring"
  (:feature parent-feature)
  (:role "What this function does in the feature")
  (:failure-modes ((:id "description" :violates :goal-id)))
  (actual-function-body))
```

### Feature Hierarchy

Features form a tree via `:belongs-to`:

```lisp
(deffeature security :purpose "Top-level security")
(deffeature user-authentication
  :purpose "Verify identity"
  :belongs-to security)
```

### Naming Conventions

- Features: kebab-case symbols (`user-authentication`, `token-bucket`)
- Failure mode IDs: keywords (`:timing-attack`, `:lockout`)
- Goal IDs: keywords (`:secure`, `:usable`)

## Test Structure

Test package: `:telos/tests`
Root suite: `:telos-tests`

```
tests/
├── package.lisp      (defines test package, root suite)
├── intent-test.lisp  (intent struct creation/access)
├── feature-test.lisp (deffeature, hierarchy, list-features)
├── function-test.lisp (defun/i, defintent, intent retrieval)
├── class-test.lisp   (defclass/i, intentful-class metaclass)
└── query-test.lisp   (intent-chain, feature-members)
```

## TDD Workflow

This project follows strict TDD. When adding functionality:

1. Write failing test in appropriate `tests/*-test.lisp`
2. Run `(5am:run! :telos-tests)` — confirm failure
3. Implement minimal code in `src/*.lisp`
4. Run tests — confirm pass
5. Commit with clear message

## Documentation

- `docs/tutorial.md` — Hands-on guide building a rate limiter
- `docs/explanation.md` — Design rationale and mental models
- `docs/reference.md` — Complete API reference
- `docs/use-cases.md` — Real-world scenarios
