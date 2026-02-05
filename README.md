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

### Install

```lisp
;; Load via Quicklisp (when available)
(ql:quickload :telos)

;; Or load from local directory
(asdf:load-system :telos)
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

## What You Get

- `deffeature` — Define features with purpose, goals, constraints, and failure modes
- `defun/i` — Define functions with embedded intent
- `defclass/i` — Define classes with intent via metaclass
- `defstruct/i` — Define structs with embedded intent
- `define-condition/i` — Define conditions with embedded intent
- `defintent` — Retrofit intent onto existing functions, classes, or methods
- Query API — `intent-chain`, `feature-members`, `get-intent`, `method-intent`, and more

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
