---
name: telos-integration
description: How to use telos intent introspection in your Common Lisp project
version: 0.1.0
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

### Query API

| Function | Purpose |
|----------|---------|
| `get-intent` | Get intent for any symbol |
| `intent-chain` | Trace from symbol up to root feature |
| `feature-members` | List all code belonging to a feature |
| `feature-intent` | Full intent for a feature |
| `intent-feature` | Quick lookup: which feature owns this? |
| `feature-decisions` | Design decisions for a feature |

### Decision Tracking

```lisp
(record-decision 'user-authentication
  :id :session-store
  :chose "signed cookies"
  :over ("server-side sessions" "JWT")
  :because "Stateless, no shared storage needed")
```

## MCP Tools

When telos is loaded in a Lisp MCP session, Claude Code gets 5 tools:

| Tool | Purpose |
|------|---------|
| `telos-list-features` | List all features with purpose and hierarchy |
| `telos-feature-intent` | Full intent for a feature |
| `telos-get-intent` | Intent for a specific function, class, or condition |
| `telos-intent-chain` | Trace intent from symbol up to root feature |
| `telos-feature-members` | All code belonging to a feature |

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

## Deep Dives

- `docs/reference.md` — complete API reference with all parameters
- `docs/tutorial.md` — hands-on guide building a rate limiter with intent
- `docs/explanation.md` — design rationale and mental models
