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

## Telos in the Age of Agents

Agents read code well. They infer purpose badly.

An agent asked to "fix the rate limiter" can read every line, trace every caller, and still not know whether blocking that power user was the bug or the feature. So it guesses. It reconstructs intent from variable names and commit archaeology, burns context doing it, and lands on an answer that sounds right. Sometimes it is.

The guessing has a second cost. Intent recovered by inference dies with the session. The next agent — after a context compaction, a model swap, a fresh checkout — starts the archaeology over. Intent written into the code survives all three.

### Conditions and telos: recovery that knows what it protects

Common Lisp's condition system already splits *detecting* a problem from *deciding* what to do about it. A function signals and offers restarts; a handler further up the stack, holding more context, picks one. The stack stays alive while the decision is made.

That split is precisely the shape an agent needs. The agent belongs in the handler — not in the code that failed.

But the condition system hands the handler only two things: a condition object and a list of restarts. `retry-file`, `continue-next-row`, `continue-next-field` — all three "work". Which one is *right* depends on what the system is trying to achieve, and that is exactly what the runtime does not carry.

Telos supplies the missing half. Declare the condition with `define-condition/i`, attach it to its feature, and say which goal each failure mode violates:

```lisp
(deffeature rate-limiting
  :purpose "Protect shared capacity without turning away legitimate users"
  :goals ((:protect "No client can exhaust shared capacity")
          (:allow-legitimate "Real users are never blocked")))

(define-condition/i rate-limit-exceeded (error)
  ((client :initarg :client :reader offending-client))
  (:feature rate-limiting)
  (:role "Report that a client crossed its quota, before deciding what to do about it")
  (:failure-modes ((:false-positive "The blocked client is legitimate"
                    :violates :allow-legitimate))))

(defun/i admit-request (client)
  "Admit a request, signalling when the client is over quota."
  (:feature rate-limiting)
  (:role "Enforce the quota at the entry point and leave recovery to the caller")
  (restart-case (if (over-quota-p client)
                    (error 'rate-limit-exceeded :client client)
                    :admitted)
    (deny () :report "Reject the request." :denied)
    (admit-anyway () :report "Let this request through." :admitted)))
```

Now a handler can ask the condition what a wrong answer would cost:

```lisp
(defun agent-handler (condition)
  (let* ((mode (first (intent-failure-modes (get-intent 'rate-limit-exceeded))))
         (goal (intent-entry-option mode :violates)))
    (when (and (eq goal :allow-legitimate)
               (trusted-p (offending-client condition)))
      (invoke-restart 'admit-anyway))))

(handler-bind ((rate-limit-exceeded #'agent-handler))
  (admit-request "batch-importer"))
;; => :ADMITTED
```

The handler matched no error strings and hard-coded no policy. It read the failure mode the condition declares, learned which goal a false block would violate, and took the restart that preserves it. Rename the goal or retarget the `:violates` and the handler's reasoning follows — because the goal lives in the code, not in a prompt.

### Metadata that stays honest

Agent-readable metadata that drifts out of sync is worse than none: it teaches confident wrong answers. `check-intent-references` walks the whole intent graph and reports failure modes pointing at goals that no longer exist, `:belongs-to` naming a feature that was never defined, and loops in the hierarchy:

```lisp
(check-intent-references)
;; => ((:SEVERITY :ERROR :CODE :DANGLING-VIOLATES :ENTITY TYPO-CONDITION
;;      :ENTITY-TYPE :CONDITION :REFERENCE :ALLOW-LEGITMATE :MESSAGE
;;      "Failure mode :OOPS of TYPO-CONDITION violates :ALLOW-LEGITMATE, which is
;;       not a goal of TYPO-CONDITION or of any feature it belongs to."))
```

Wire `assert-intent-references` into your test suite and a stale goal reference fails the build.

For a full worked example — a CSV validator with three levels of restarts and an agent-facing recovery API — see [examples/csv-validator.lisp](examples/csv-validator.lisp).

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
  :over '("server-side sessions" "JWT")
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

Claude Code gets 7 telos-specific tools:

| Tool | Purpose |
|------|---------|
| `telos-list-features` | List all features with their purpose and hierarchy |
| `telos-feature-intent` | Get complete intent for a feature (goals, constraints, failure modes) |
| `telos-get-intent` | Get intent for a specific function, class, or condition |
| `telos-intent-chain` | Trace intent from symbol up to root feature |
| `telos-feature-members` | List all code belonging to a feature |
| `telos-feature-decisions` | Get all decisions recorded for a feature |
| `telos-list-decisions` | List all decisions across all features |

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
- [Example: CSV validator](examples/csv-validator.lisp) — Conditions, restarts, and an agent-facing recovery API

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
