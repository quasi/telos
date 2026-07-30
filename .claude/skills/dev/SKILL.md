---
name: telos-dev
description: Development guide for working on the telos intent introspection library
version: 1.2.0
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
(5am:run! :audit-tests)
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
| Features | `*feature-registry*` hash table, keyed on the feature name |
| Function / struct / condition intent | `*entity-intent-registry*` hash table, keyed on `(kind name)`, via `register-entity-intent` / `entity-intent` |
| Class intent (defclass/i) | Metaclass slot via `class-intent` accessor |
| Class intent (retrofitted) | `*class-intent-registry*` hash table |
| Method intent | `*method-intent-registry*` hash table, keyed on `(name specializers)` |
| Decisions | `*decision-registry*` hash table, feature name → list of `decision` structs |
| Feature members | `*feature-members*` hash table (candidate index — see below) |

All live in `src/storage.lisp`. Nothing is stored on a symbol plist.

### Module Load Order

```
package → validation → intent → storage → decision → feature → function → class → struct → condition → query → audit
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
| `define-entry-option` | Widen a field's entry-option vocabulary at compile time |

All definition macros validate what they were given at macroexpansion time, via
`src/validation.lisp`, signalling `invalid-intent-declaration` rather than dropping anything:

| Layer | Checked by | Table to extend |
|-------|-----------|-----------------|
| Nested entries (goals, failure modes, …) | `validate-intent-fields` | `*intent-entry-option-keys*` |
| Decision plists | `validate-decision-entries` | `*decision-keys*` |
| Top-level clauses in `defun/i`, `defstruct/i`, `defclass/i`, `define-condition/i` | `invalid-intent-clause` from each parser's `case` `otherwise` | `*intent-clause-keys*` |
| Forwarded `defclass` / `define-condition` options | `parse-class-intent-options` | `*defclass-passthrough-options*`, `*define-condition-passthrough-options*` |
| Decision value types | `validate-decision-values` | `*decision-value-types*` |
| Duplicate entry ids | `validate-intent-entries` | — (within a field only) |

Registries are candidate indexes, never truth — `all-intentful-classes` and `feature-members`
both re-derive from the entity itself on read, so stale entries drop out rather than becoming
phantom answers. Apply that rule to any new index.

Nested structured fields are literal data; scalar top-level fields (`:purpose`, `:role`) are
evaluated. `form-valued-p` catches a form where data belongs — it needs no list of known heads,
since a list of entries always has conses for elements, so any symbol in the head position is
already wrong.

`deffeature` and `defintent` get top-level strictness free from their `&key` lambda lists.
If you add a nested field, add it to `*intent-entry-option-keys*` — an unlisted field is not
validated at all. If you add a clause, add it to both the parser's `case` and
`*intent-clause-keys*` (the latter only feeds the error message).

### Extending the entry vocabulary

`define-entry-option` wraps `add-entry-options` in an `eval-when` (so it carries into a fasl and
into the rest of its own file) and returns `keys`. Rules that hold in the implementation:

- `*intent-entry-option-keys*` is built with `list`, not quoted — `add-entry-option` mutates
  those cells, and destructively modifying literal source data is undefined.
- New keys are **appended**, so built-ins stay first and "expected one of …" reads the same
  everywhere.
- `add-entry-options` checks every key before installing any: a form rejected halfway must not
  leave the vocabulary half-widened. The field is checked even when `keys` is empty.
- Two error reasons come from here: `:unknown-entry-field` (typo'd field) and
  `:invalid-option-key` (non-keyword key).
- Not thread-safe and not locked, by design; a forced reload of Telos resets the table to the
  built-in vocabulary. Both facts say the same thing — extend at load time, from one thread, in
  a file that loads before the declarations it affects.

## Query API

| Function | Purpose |
|----------|---------|
| `intent-chain` | Trace from function/class up to root feature |
| `feature-members` | Get all code belonging to a feature |
| `get-intent` | Get intent for function, class, or method |
| `method-intent` | Intent for one method specialization |
| `feature-intent` | Get intent for a feature |
| `intent-feature` | Quick lookup: which feature owns this? |
| `class-intent` | Get intent for a class |
| `feature-parent` / `feature-children` | Walk the feature tree one step |
| `list-features` | Every feature in the image |
| `feature-decisions` | Get decisions for a feature |
| `check-intent-references` | Audit the graph: `:dangling-violates`, `:undefined-parent`, `:cyclic-hierarchy`, `:malformed-field` |
| `assert-intent-references` | Same, but signals `intent-reference-error` |
| `all-intentful-classes` | Enumerate `defclass/i` classes (metaclass-recorded, re-derived on read) |
| `list-decisions` | All decisions across features |
| `intent-entry-id` / `intent-entry-description` / `intent-entry-option` | Read one entry of `:goals`, `:failure-modes`, … |
| `intent-entry-list` | The field as a proper list, plus a second value: was it walkable? |
| `define-entry-option` | Widen the keyword options a field accepts |

### The read side is total, and must stay that way

`make-intent` is exported and validates nothing, so anything reaching the accessors may be
malformed. `intent-entry-*` therefore return `nil` rather than signalling, and use `list-length`
rather than `proper-list-p` — the latter spins forever on a circular tail. The validator may
assume declarations come from source and cannot be circular; the read side may not. A wedged
audit is worse than a failed one.

`check-intent-references` sweeps the whole image and never signals, so one malformed field is a
`:malformed-field` finding (its `:reference` names the field) and the entity's other fields are
still audited. It reads `:violates` through `intent-entry-option`, not a private accessor —
keep it that way.

The `intent-entry-` prefix is load-bearing, not verbose: `entry-id` and `entry-description` are
exactly what `(defstruct entry id description …)` generates, and a downstream package that
`:use`s Telos would have clobbered them with only a warning.

## Conventions

### Intent Clause Syntax

After docstring/declarations, before body:
```lisp
(defun/i my-function (args)
  "Optional docstring"
  (:feature parent-feature)
  (:role "What this function does in the feature")
  (:failure-modes ((:id "description" :violates :goal-id :mitigation "how to recover")))
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
  decision-test.lisp, validation-test.lisp, audit-test.lisp
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
- `docs/plans/` — implementation plans for in-flight work
- `CHANGELOG.md` — the reasoning behind each release, not just the diff; write the *why* there
- `examples/csv-validator.lisp` — a worked example, including a member that legitimately
  violates a goal declared on its parent

Both shipped skills (`.claude/skills/dev`, `.claude/skills/integration`) are part of the
release: a behaviour change that reaches users updates the skill in the same commit, and their
`version:` tracks the system version in `telos.asd`.
