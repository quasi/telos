# Use Cases and Examples

Real-world scenarios where intent introspection provides value.

---

## The Origin Story

Telos emerged from a question: **Can systems understand themselves?**

Systems can describe their behavior through introspection—function signatures, stack traces, runtime state. Ask a system *why it exists*, and you get silence. This missing layer is **intent**.

Documentation was the first attempt to capture intent. But documentation was designed for humans reading code *before* they work—not for machines reasoning *during* operation. With LLMs, we now have reasoning capability that could use intent information—if only it were queryable.

### The Chain: Intent → Behavior

Every system artifact exists because someone wanted something:

```
Intent (WHY)
    ↓
Design Decisions (HOW to achieve)
    ↓
Implementation (WHAT we built)
    ↓
Behavior (observable output)
```

Code captures the bottom two layers. Intent—the top of the chain—evaporates the moment fingers leave the keyboard.

This matters because **behavior can be correct while intent is violated**, and vice versa. A rate limiter that blocks a legitimate power user is behaving correctly (following its rules) but violating its intent (protect system *while allowing* legitimate use).

Without access to intent, a debugging system—human or AI—cannot distinguish these cases.

---

## Use Case 1: The Rate Limiter Dilemma

### The Problem

A rate limiter blocks requests exceeding 100/minute from the same IP.

| Layer | Content |
|-------|---------|
| **Behavior** | Rejects requests exceeding 100/minute |
| **Implementation** | Token bucket algorithm, Redis counter |
| **Intent** | Protect system from abuse while allowing legitimate heavy use |

A legitimate power user hits the limit. The system behaves correctly but violates intent. The system cannot distinguish abuse (bot scraping) from legitimate heavy use (power user on deadline).

### With Telos

```lisp
(deffeature rate-limiting
  :purpose "Protect system from abuse while allowing legitimate heavy use"
  :goals ((:block-abuse "No single actor can overwhelm system resources")
          (:allow-legitimate "Authenticated users performing legitimate work are never blocked"))
  :assumptions ((:mostly-legitimate "99%+ of traffic is legitimate")
                (:abuse-is-automated "Abuse comes from bots/scripts, not humans"))
  :failure-modes ((:false-positive "Legitimate user blocked" :violates :allow-legitimate)
                  (:false-negative "Abusive user not blocked" :violates :block-abuse)))

(defun/i check-rate-limit (user-id)
  "Decide if request should proceed"
  (:feature rate-limiting)
  (:role "Gatekeeper: allow or block requests")
  (:failure-modes ((:race-condition "Concurrent requests bypass limit")))
  ;; implementation
  ...)
```

### What an Intent-Aware System Can Reason

> "User exceeded rate limit. User is authenticated, paid tier, 2-year history. Intent is 'protect from abuse while allowing legitimate heavy use.' This appears to be legitimate heavy use, not abuse. Action: temporarily increase limit, log for review."

The behavior says "block." The intent says "allow legitimate use." Only with both can you make the right call.

---

## Use Case 2: The Retry Mechanism

### The Problem

A retry mechanism attempts failed HTTP calls 3 times with exponential backoff.

| Layer | Content |
|-------|---------|
| **Behavior** | Retries failed HTTP calls 3 times |
| **Implementation** | Loop with sleep, catch specific exceptions |
| **Intent** | Handle transient failures without burdening the user |

The key word is *transient*. If the downstream service is permanently down, retrying makes things worse—adds latency, wastes resources, delays the inevitable error.

### With Telos

```lisp
(deffeature retry-mechanism
  :purpose "Handle transient failures without burdening the user"
  :goals ((:resilience "Recover automatically from temporary blips")
          (:responsiveness "Surface permanent failures quickly"))
  :assumptions ((:transient-common "Most failures are transient (network hiccups)")
                (:permanent-rare "Permanent failures are outliers"))
  :failure-modes ((:retry-permanent "Retrying a permanent failure wastes time"
                   :violates :responsiveness)
                  (:give-up-transient "Giving up on a transient failure loses work"
                   :violates :resilience)))

(defun/i with-retry (fn &key (max-attempts 3))
  "Execute FN with automatic retry on transient failures"
  (:feature retry-mechanism)
  (:role "Orchestrate retry attempts")
  (:constraints ((:exponential-backoff "Wait increases between attempts")))
  ;; implementation
  ...)
```

### What an Intent-Aware System Can Reason

> "Three retries failed with identical error (connection refused). Intent is to handle *transient* failures. Connection refused on all attempts suggests permanent failure. Action: abort retry loop, surface error immediately."

The behavior says "retry." The intent says "*transient* failures only." The reasoning fills the gap.

---

## Use Case 3: The Stale Database Index

### The Problem

A database has an index on `user_id`:

| Layer | Content |
|-------|---------|
| **Behavior** | Maintains B-tree structure on `user_id` column |
| **Implementation** | `CREATE INDEX user_id_idx ON orders(user_id)` |
| **Intent** | Speed up the "orders by user" dashboard query |

Indexes have maintenance cost. Without intent, we cannot know if an index is still earning its keep. The dashboard might have been removed. The query pattern might have changed.

### With Telos

```lisp
(deffeature orders-dashboard
  :purpose "Show user's order history quickly"
  :goals ((:fast-load "Dashboard loads in <200ms"))
  :constraints ((:no-full-scan "Never scan entire orders table")))

(defintent user-id-idx
  :feature orders-dashboard
  :role "Accelerate user lookup in orders table"
  :purpose "Speed up the orders-by-user dashboard query"
  :assumptions ((:dashboard-active "Dashboard feature is in use")))
```

### What an Intent-Aware System Can Reason

> "Index `user-id-idx` exists. Intent was 'speed up orders dashboard query.' Query logs show this pattern has not executed in 90 days. Dashboard was removed in commit abc123. Intent is obsolete. Recommendation: drop index."

Behavior is silent about purpose. Intent reveals whether something should still exist.

---

## Use Case 4: The Validation Function

### The Problem

An email validation function:

| Layer | Content |
|-------|---------|
| **Behavior** | Returns true if email matches regex |
| **Implementation** | Pattern match |
| **Intent** | Ensure user can be contacted |

The regex is an approximation. `a@b.c` passes validation but is not contactable. Without intent, we cannot evaluate whether validation is *good enough*.

### With Telos

```lisp
(deffeature user-contact
  :purpose "Ensure we can reach users when needed"
  :goals ((:deliverability "All stored emails can receive messages"))
  :failure-modes ((:undeliverable "User provides email we cannot reach"
                   :violates :deliverability)))

(defun/i validate-email (email)
  "Check if email format is valid"
  (:feature user-contact)
  (:role "First-pass filter for obviously invalid emails")
  (:purpose "Catch typos and invalid formats before storage")
  (:constraints ((:permissive "Accept edge cases rather than reject valid emails")))
  (:assumptions ((:format-not-deliverability "This checks format, not deliverability")))
  ;; implementation
  ...)
```

### What an Intent-Aware System Can Reason

> "Email `test@example.com` passed validation but delivery failed. Intent is 'ensure user can be contacted.' Validation only checks syntax. Gap between intent and implementation. Recommend: add verification step (confirmation email)."

The behavior says "valid." The intent says "contactable." These are different things.

---

## Use Case 5: The Thermostat

### The Problem

A thermostat turns on the heater when temperature drops below 68°F.

| Layer | Content |
|-------|---------|
| **Behavior** | Turns on heater when temp < 68°F |
| **Implementation** | Sensor + relay + threshold |
| **Intent** | Maintain comfortable temperature for occupants |

Without intent, 68°F appears to be a sacred constant. Knowing intent reveals that 68 is merely a proxy for "comfort"—it could be adjusted or replaced.

### With Telos

```lisp
(deffeature temperature-control
  :purpose "Maintain comfortable temperature for occupants"
  :goals ((:comfort "Occupants feel neither too hot nor too cold")
          (:efficiency "Minimize energy use when comfort not needed"))
  :assumptions ((:occupants-present "Someone is home to benefit from heating")
                (:68-is-comfort "68°F is the comfort target"))
  :failure-modes ((:wasteful-heating "Heating empty house" :violates :efficiency)
                  (:uncomfortable "Occupants too cold/hot" :violates :comfort)))

(defun/i should-activate-heater-p (current-temp)
  "Decide if heater should run"
  (:feature temperature-control)
  (:role "Compare current temperature to comfort threshold")
  (:assumptions ((:threshold-68 "68°F is the configured comfort point")))
  (< current-temp 68))
```

### What an Intent-Aware System Can Reason

> "Temperature is 67°F but occupants are away. Intent is comfort *for occupants*. No occupants present. Intent is not violated. Action: do not activate heater."

The behavior sees threshold breached and acts. Intent enables contextual reasoning.

---

## Use Case 6: LLM-Assisted Debugging

### The Scenario

An LLM debugging agent investigates why a user was blocked by the rate limiter.

### Without Intent

The agent sees:
- Function: `check-rate-limit`
- Input: `user-id = "user-456"`
- Output: `NIL` (blocked)
- Logs: `request count = 101, limit = 100`

The agent reports: "User was blocked because request count (101) exceeded limit (100). Working as intended."

But was it? The agent cannot know.

### With Intent

The agent queries:

```lisp
(intent-chain 'check-rate-limit)
;; => Shows purpose: "Protect from abuse while allowing legitimate heavy use"
;;    Shows failure mode: "Legitimate user blocked" violates "allow-legitimate"

(get-intent 'check-rate-limit)
;; => Shows assumption: "Abuse comes from bots/scripts, not humans"
```

The agent reasons:

> "Function blocked user. Checking intent: purpose is 'protect from abuse while allowing legitimate heavy use.' Checking failure modes: 'legitimate user blocked' violates 'allow-legitimate' goal. Checking assumptions: 'abuse comes from bots/scripts.'
>
> Query: Is this user a bot? Examining: user has 2-year history, paid account, human usage patterns.
>
> Conclusion: This is a false positive. The rate limiter is violating its own intent. Recommend: increase limit for this user tier."

Intent transforms debugging from "is the code working?" to "is the code achieving its purpose?"

---

## Use Case 7: Onboarding New Developers

### The Scenario

A new developer joins the team and asks: "What's going on with authentication?"

### Without Intent

They read `auth.lisp`. They see functions: `verify-password`, `create-session`, `check-token`. They understand WHAT the code does but not WHY certain decisions were made.

Why does session timeout after 30 minutes? Why is password hashing done this way? What can go wrong?

### With Intent

```lisp
(list-features "auth")
;; => (USER-AUTHENTICATION SESSION-MANAGEMENT TOKEN-VERIFICATION)

(feature-intent 'session-management)
;; => Purpose: "Maintain user identity across requests without repeated login"
;;    Goals: (:seamless "User never notices session machinery")
;;           (:secure "Stolen session cannot be used indefinitely")
;;    Failure-modes: (:session-hijack "Attacker uses stolen session" :violates :secure)
;;                   (:frequent-logout "User forced to re-login" :violates :seamless)

(feature-members 'session-management)
;; => Functions: (create-session extend-session invalidate-session)
;;    Classes: (user-session)
```

The developer immediately understands:
- Sessions exist for seamlessness and security (why)
- 30-minute timeout balances `:seamless` vs `:secure`
- Session hijacking is the main threat
- Which functions implement this feature

---

## Use Case 8: Audit and Compliance

### The Scenario

Auditor asks: "Show me all code that handles user data."

### Without Intent

Grep for "user" in the codebase. Get hundreds of results. Manually triage.

### With Intent

```lisp
(deffeature user-data-handling
  :purpose "Process user information with privacy guarantees"
  :constraints ((:gdpr "Must support right-to-deletion")
                (:encryption "PII must be encrypted at rest"))
  :failure-modes ((:data-leak "User data exposed to unauthorized party")))

;; Query
(feature-members 'user-data-handling :functions)
;; => (store-user-data retrieve-user-data delete-user-data export-user-data)

(feature-members 'user-data-handling :classes)
;; => (user-profile encrypted-field)
```

The auditor gets:
- Precisely which code handles user data
- What constraints must be met (GDPR, encryption)
- What failure modes to test for

---

## Use Case 9: Retrofitting a Legacy Codebase

### The Scenario

You inherit a codebase with no documentation. You want to add intent incrementally.

### The Approach

```lisp
;; Start with high-level features (what does this system do?)
(deffeature order-processing
  :purpose "Convert customer orders into fulfilled shipments")

(deffeature payment-handling
  :purpose "Charge customers and track payment status"
  :belongs-to order-processing)

;; Then retrofit functions you understand
(defintent process-payment
  :feature payment-handling
  :purpose "Charge customer's payment method"
  :failure-modes ((:double-charge "Customer charged twice")
                  (:charge-failed "Payment rejected but order proceeds")))

;; Eventually, your understanding is queryable
(intent-chain 'process-payment)
;; Shows the function in context of the payment system
```

You don't need to understand everything at once. Add intent as you learn. Your understanding becomes permanent, queryable knowledge.

---

## The Bigger Picture

Telos addresses a fundamental asymmetry: **we have introspection for behavior but not for intent**.

Systems can tell you:
- What functions exist
- What types they accept
- What state they hold
- What exceptions they throw

Systems cannot tell you:
- Why they exist
- What success looks like
- What can go wrong (semantically, not just exceptions)
- What assumptions they make about the world

Telos closes this gap. It makes intent as introspectable as behavior.

This enables:
- **Humans** to understand code faster
- **LLMs** to reason about code contextually
- **Systems** to explain and potentially heal themselves

The test: Can an agent understand your code WITHOUT reading separate documentation? With Telos, yes.

---

**Navigation**: [← Explanation](explanation.md) | [README](../README.md)
