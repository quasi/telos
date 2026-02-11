# Telos

Intent introspection for Common Lisp — make the WHY queryable.

## What is Telos?

Telos captures the purpose, goals, and failure modes of your code, then lets you query that intent at runtime. Instead of scattering rationale across commit messages and comments, embed it in the code itself and make it discoverable.

## Why Telos?

Code answers WHAT and HOW. Telos answers WHY.

Systems can introspect their own behavior—function signatures, stack traces, runtime state. Ask a system *why it exists*, and you get silence. This missing layer is **intent**.

Consider a rate limiter that blocks a legitimate power user. The code behaves correctly (following its rules), but violates its intent (protect system *while allowing* legitimate use). Without queryable intent, no debugging tool—human or AI—can distinguish these cases.

Telos makes intent introspectable:

- **Maintainability**: Understand why code exists when you read it months later
- **Onboarding**: New developers query intent instead of reverse-engineering decisions
- **Debugging**: Trace failure modes up feature hierarchies to find root causes
- **LLM-assisted reasoning**: Give AI agents the context to reason about purpose, not just behavior
- **Self-documenting code**: Intent lives with code, not in stale external docs

## Quickstart

### Requirements
- Any ANSI compliant Lisp implementation (I recommend SBCL, get it at sbcl.org)
- Having quicklisp installed (Follow the instructions at quicklisp.org/beta/)

### Install

```lisp

;; Load via Quicklisp (when available)
(ql:quickload :telos)
;; Install the dependency
(ql:quickload "closer-mop")

;; To load from local directory (Insert in your REPL)
;; 1. Load Quicklisp (if you have it installed)
(load "~/quicklisp/setup.lisp")

;; 2. ASDF is usually already loaded in modern Common Lisp
;;    but we ensure it's available
(require 'asdf)

;; 3. Define the path to telos - ADJUST THIS TO YOUR ACTUAL PATH
;;    For Windows users: Use forward slashes or double backslashes
(defparameter *telos-dir* (truename "C:/your/actual/path/to/telos/"))

;; 4. Add the directory to your central registry
(pushnew *telos-dir* asdf:*central-registry*)

;; 5. Install telos dependency (closer-mop)
(ql:quickload "closer-mop")

;; 6. Finally load the system
(asdf:load-system "telos")
```

### Define a Feature

```lisp
(use-package :telos)

(deffeature user-authentication
  :purpose "Verify user identity before granting access"
  :goals ((:secure "No unauthorized access")
          (:usable "Users can log in quickly"))
  :failure-modes ((:lockout "Legitimate user blocked" :violates :usable)
                  (:breach "Attacker gains access" :violates :secure)))
```

### Define Functions with Intent

```lisp
(defun/i verify-credentials (username password)
  "Check if credentials are valid"
  (:feature user-authentication)
  (:role "Validate username/password pair")
  (:failure-modes ((:timing-attack "Password comparison leaks timing")))
  (secure-compare (lookup-password username) password))
```

### Query Intent

```lisp
;; Get full intent chain from function to root feature
(intent-chain 'verify-credentials)
;; => ((:type :function :name verify-credentials :role "Validate username/password pair" ...)
;;     (:type :feature :name user-authentication :purpose "Verify user identity..." ...))

;; Get all members of a feature
(feature-members 'user-authentication)
;; => (:functions (verify-credentials check-session ...)
;;     :classes (user session ...)
;;     :structs (...)
;;     :conditions (auth-failure ...)
;;     :methods (...)
;;     :features ())

;; Quick lookup: which feature does this belong to?
(intent-feature 'verify-credentials)
;; => user-authentication
```

### Track Decisions

```lisp
;; Inline with feature definition
(deffeature user-authentication
  :purpose "Verify user identity before granting access"
  :decisions ((:id :auth-method
               :chose "bcrypt"
               :over ("argon2" "scrypt")
               :because "Widest library support, proven in production"
               :date "2026-02-06"
               :decided-by "quasi")))

;; Or record decisions later as they happen
(record-decision 'user-authentication
  :id :session-store
  :chose "signed cookies"
  :over ("server-side sessions" "JWT")
  :because "Stateless, no shared storage needed")

;; Query decisions
(feature-decisions 'user-authentication)
;; => (#S(DECISION :ID :SESSION-STORE :CHOSE "signed cookies" ...)
;;     #S(DECISION :ID :AUTH-METHOD :CHOSE "bcrypt" ...))

;; List all decisions across features
(list-decisions)
;; => ((USER-AUTHENTICATION . (#S(DECISION ...) #S(DECISION ...)))
;;     (RATE-LIMITING . (#S(DECISION ...))))
```

## What You Get

- `deffeature` — Define features with purpose, goals, constraints, and failure modes
- `defun/i` — Define functions with embedded intent
- `defclass/i` — Define classes with intent via metaclass
- `defstruct/i` — Define structs with embedded intent
- `define-condition/i` — Define conditions with embedded intent
- `defintent` — Retrofit intent onto existing functions, classes, or methods
- Decision tracking — `record-decision`, `feature-decisions`, `list-decisions`
- Query API — `intent-chain`, `feature-members`, `get-intent`, `method-intent`, and more
- **MCP Integration** — Query intent directly from Claude Code (see below)

## MCP Integration: Query Intent from Claude Code

Telos integrates with [cl-mcp-server](https://github.com/quasi/cl-mcp-server) to make intent introspection available directly in Claude Code sessions.

### Setup

Install and configure cl-mcp-server following its [installation instructions](https://github.com/quasi/cl-mcp-server#installation). Once configured, load telos in your REPL session:

```lisp
(ql:quickload :telos)
```

### Available Tools

Claude Code gets 5 telos-specific tools:

| Tool | Purpose |
|------|---------|
| `telos-list-features` | List all features with their purpose and hierarchy |
| `telos-feature-intent` | Get complete intent for a feature (goals, constraints, failure modes) |
| `telos-get-intent` | Get intent for a specific function, class, or condition |
| `telos-intent-chain` | Trace intent from symbol up to root feature |
| `telos-feature-members` | List all code belonging to a feature |

### Example Workflow

```
User: What features are defined in this codebase?
Claude: [uses telos-list-features]

        Features (3):

        user-authentication
          Purpose: Verify user identity before granting access
          Parent: security

        rate-limiting
          Purpose: Prevent resource exhaustion
          Parent: security

User: Why does the verify-credentials function exist?
Claude: [uses telos-intent-chain]

        Intent Chain (2 levels):

        1. [function] verify-credentials
           Role: Validate username/password pair
           Failure modes: timing-attack

        2. [feature] user-authentication
           Purpose: Verify user identity before granting access
           Failure modes: lockout, breach
```

This integration enables Claude to reason about **why** code exists, not just **what** it does—making intent a first-class part of AI-assisted development.

## Documentation

- [Tutorial](docs/tutorial.md) — Learn by building a rate limiter with intent
- [Use Cases](docs/use-cases.md) — Real-world scenarios with examples
- [API Reference](docs/reference.md) — Complete function and macro documentation
- [Explanation](docs/explanation.md) — Design rationale and mental models

## Requirements

- Common Lisp implementation (tested on SBCL)
- `closer-mop` for metaclass support
- `fiveam` for running tests (development only)

## Running Tests

```lisp
(asdf:test-system :telos)
;; Or via shell
sbcl --eval "(asdf:test-system :telos)" --quit
```

All tests should pass. If not, please report an issue.

## Project Status

Telos is in active development. The core API is stable, but expect refinements based on real-world usage.

## License

MIT License — see LICENSE file for details.

## Author

quasi / quasiLabs

---

**Next Steps**: Start with the [Tutorial](docs/tutorial.md) to build your first intentful feature.
