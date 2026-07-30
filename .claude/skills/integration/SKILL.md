---
name: telos-integration
description: How to use telos intent introspection in your Common Lisp project
version: 1.2.0
author: quasiLabs
type: integration
---

# Telos Integration Skill

## What is Telos

Telos captures WHY code exists and makes it queryable at runtime. Three-level hierarchy: **Feature** → **Function/Class/Struct/Condition** → **Sub-feature**. Intent metadata (purpose, goals, constraints, failure modes) is embedded directly in code and introspectable programmatically or via MCP tools.

## Quick Start

```lisp
(asdf:load-system :telos)
(use-package :telos)

;; Define a feature
(deffeature user-authentication
  :purpose "Verify user identity before granting access"
  :goals ((:secure "No unauthorized access")
          (:usable "Users can log in quickly"))
  :failure-modes ((:lockout "Legitimate user blocked" :violates :usable)
                  (:breach "Attacker gains access" :violates :secure)))

;; Define a function with intent
(defun/i verify-credentials (username password)
  "Check if credentials are valid"
  (:feature user-authentication)
  (:role "Validate username/password pair")
  (:failure-modes ((:timing-attack "Password comparison leaks timing")))
  (secure-compare (lookup-password username) password))

;; Query intent
(intent-chain 'verify-credentials)      ; trace up to root feature
(feature-members 'user-authentication)   ; all code in feature
(intent-feature 'verify-credentials)     ; which feature owns this?
```

## Core Concepts

### Features (`deffeature`)

High-level groupings with purpose, goals, constraints, failure modes. Hierarchies via `:belongs-to`:
```lisp
(deffeature security :purpose "Top-level security")
(deffeature rate-limiting :purpose "Prevent exhaustion" :belongs-to security)
```

### Intent-Decorated Definitions

| Macro | Use For |
|-------|---------|
| `defun/i` | Functions |
| `defclass/i` | Classes (uses `intentful-class` metaclass) |
| `defstruct/i` | Structs |
| `define-condition/i` | Conditions |
| `defintent` | Retrofit intent onto existing definitions |

All accept `:feature`, `:role`, `:failure-modes`, and other intent clauses.

### Declarations Are Strict

Both the top-level keywords and the keys inside nested entries are checked at
macroexpansion time. An unrecognized key signals `invalid-intent-declaration` — nothing you
write into a declaration is silently dropped, because a declaration that quietly loses a
field answers queries with a lie.

| Field | Entry shape | Keyword options |
|-------|-------------|-----------------|
| `goals`, `constraints`, `assumptions`, `verification` | `(:id "description")` | none |
| `failure-modes` | `(:id "description" :violates :goal-id :mitigation "...")` | `:violates`, `:mitigation` |
| `decisions` | plist | `:id`, `:chose`, `:over`, `:because`, `:date`, `:decided-by` |

```lisp
(deffeature probe :purpose "p" :failure-modes ((:fm1 "a failure" :cause "x")))
;; => In DEFFEATURE PROBE, :FAILURE-MODES entry (:FM1 "a failure" :CAUSE "x"):
;;    unknown keyword: :CAUSE; expected one of :VIOLATES, :MITIGATION
```

Read the options back with `intent-entry-option`; `intent-entry-id` and `intent-entry-description` read the rest of
the entry shape, so you never walk the list yourself:

```lisp
(intent-entry-option mode :mitigation)   ; => "how to recover"
```

All four accessors are read-side and total. A non-entry — `nil`, a bare keyword, a dotted,
circular or odd-length option tail — has no id, description, or options, rather than signalling
the way `getf` would or looping the way a naive proper-list check would. Walk the *collection*
through `intent-entry-list`, whose second value says whether the field could be walked at all,
so an unwalkable field is something you report instead of dying on:

```lisp
(multiple-value-bind (entries walkable) (intent-entry-list (intent-failure-modes i))
  (if walkable (mapcar #'intent-entry-id entries) (warn "unwalkable :failure-modes")))
```

They report what the entry holds, not what it ought to: ids are keywords and descriptions
strings by convention, but `make-intent` is exported and validates nothing, so the accessors do
not pretend otherwise.

If your project needs an option Telos does not have, add it rather than working around the
validator. Extending is explicit, and strictness is unchanged — a typo in your own vocabulary
is still an error:

```lisp
(define-entry-option :failure-modes :detected-by :severity)
```

Three things to know:

- Validation happens at macroexpansion time, so this must be **compiled before** the
  declarations that use it — put it in a file that loads first. Returns `keys`.
- A **forced reload of Telos resets** the table to the built-in vocabulary; reload the file
  holding your extensions too.
- Neither the macro nor the underlying `add-entry-option` / `add-entry-options` takes a lock —
  extend at load time, from one thread, not in a running system.

A typo'd *field* is an error too (`:unknown-entry-field`), with or without keys after it, as is
a non-keyword option key (`:invalid-option-key`). `add-entry-options` is all-or-nothing: every
key is checked before any is installed.

Also rejected, for the same reason: a key given twice, a dangling key, a clause carrying two
values (`(:goals (...) (...))` — the second would be dropped), and a field value that looks
like a form to evaluate (`:goals '((:g1 "d"))`) — field values are read literally, never
evaluated. An entry may omit its description (`(:g1)`), but a keyword may not sit where the
description belongs: `(:f1 :violates :g1)` reads as an option here and as a description to
anything taking `(second entry)`, so write `(:f1 "description" :violates :g1)`.

A decision's rationale goes in `:because`. Constraints live at feature level
(`intent-constraints`), never on a decision.

### Query API

| Function | Purpose |
|----------|---------|
| `get-intent` | Get intent for any symbol |
| `method-intent` | Intent for one method specialization |
| `intent-chain` | Trace from symbol up to root feature |
| `feature-members` | List all code belonging to a feature |
| `feature-intent` | Full intent for a feature |
| `intent-feature` | Quick lookup: which feature owns this? |
| `feature-decisions` | Design decisions for a feature |
| `list-features` / `list-decisions` | Everything in the image |
| `feature-parent` / `feature-children` | Walk the feature tree one step |
| `all-intentful-classes` | Every `defclass/i` class, including one with no `:feature` |
| `check-intent-references` | Audit the intent graph for unresolved references |
| `assert-intent-references` | Same, signalling `intent-reference-error` |
| `intent-entry-list` / `-id` / `-description` / `-option` | Read entries without walking them |
| `define-entry-option` | Widen the options a field accepts |

### Decision Tracking

```lisp
(record-decision 'user-authentication
  :id :session-store
  :chose "signed cookies"
  :over '("server-side sessions" "JWT")
  :because "Stateless, no shared storage needed")
```

### Auditing Your Intent Graph

Strictness at declaration time cannot catch a `:violates` that names a goal on a feature
defined in another file, so check the finished image:

```lisp
(check-intent-references)
;; => nil when everything resolves, else findings with :code :dangling-violates,
;;    :undefined-parent, :cyclic-hierarchy or :malformed-field

(assert-intent-references)   ; signals intent-reference-error — one line for a test
```

The codes are API. `:malformed-field` names, in `:reference`, a field the audit could not walk
(a dotted or circular cons reached a slot via `make-intent`); the rest of that entity's fields
are still audited, because one malformed entry must not take the whole sweep down. Findings are
sorted, so a CI diff is stable despite hash-ordered registries, and `check-intent-references`
never signals — a mid-load image legitimately shows dangling references.

Worth a test in your suite: a renamed goal leaves failure modes pointing at nothing, and
nothing else will tell you.

## MCP Tools

When telos is loaded in a Lisp MCP session, Claude Code gets 7 tools:

| Tool | Purpose |
|------|---------|
| `telos-list-features` | List all features with purpose and hierarchy |
| `telos-feature-intent` | Full intent for a feature |
| `telos-get-intent` | Intent for a specific function, class, or condition |
| `telos-intent-chain` | Trace intent from symbol up to root feature |
| `telos-feature-members` | All code belonging to a feature |
| `telos-feature-decisions` | Design decisions recorded for a feature |
| `telos-list-decisions` | All decisions across all features |

## Common Patterns

1. **Before modifying code**: read feature intent to understand goals and constraints to preserve
2. **Understanding why a function exists**: `intent-chain` traces up through feature hierarchy
3. **Finding related code**: `feature-members` discovers all code in a feature
4. **Reviewing design decisions**: `feature-decisions` shows what was chosen and rejected

## Pitfalls

- **Don't add intent to trivial helpers** — intent is for code with meaningful purpose
- **Feature hierarchy should mirror real architecture** — not organizational convenience
- **Intent describes WHY, not WHAT** — code shows what; intent captures purpose and rationale
- **Link failure modes to goals** — every failure mode should violate a specific goal via `:violates`
- **Don't invent keys inside entries** — the macros reject unknown nested keys with `invalid-intent-declaration`; if a field seems missing, it probably belongs at feature level

## Deep Dives

- `docs/reference.md` — complete API reference with all parameters
- `docs/tutorial.md` — hands-on guide building a rate limiter with intent
- `docs/explanation.md` — design rationale and mental models
