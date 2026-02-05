# Explanation: Understanding Telos

This document explains the design rationale, mental models, and trade-offs behind Telos.

---

## Table of Contents

1. [The Problem](#the-problem)
2. [Core Concepts](#core-concepts)
3. [Design Decisions](#design-decisions)
4. [Mental Models](#mental-models)
5. [Limitations and Trade-offs](#limitations-and-trade-offs)
6. [When to Use Telos](#when-to-use-telos)
7. [Comparison to Alternatives](#comparison-to-alternatives)

---

## The Problem

### Code Documents WHAT and HOW, Not WHY

Traditional code captures:
- **WHAT**: The data structures and outputs
- **HOW**: The algorithms and control flow

But not:
- **WHY**: Why this code exists
- **WHAT IF**: What can go wrong and why it matters
- **CONTEXT**: How this fits into larger goals

Comments and documentation help, but they:
- Drift from code over time
- Live in separate files or commits
- Lack structure for querying
- Are hard to maintain

### The Cost of Missing Intent

When intent is implicit:
- **Maintenance**: "Why does this exist?" requires archaeology
- **Refactoring**: "Can I delete this?" needs reverse-engineering
- **Debugging**: "Why did this fail?" obscures root causes
- **Onboarding**: New developers guess instead of query

Telos makes intent explicit, structured, and queryable.

---

## Core Concepts

### Intent is Data

Telos treats intent as first-class data:

```lisp
(defstruct intent
  purpose         ; String: why this exists
  failure-modes   ; List: what can go wrong
  goals           ; List: success criteria
  constraints     ; List: boundaries
  assumptions     ; List: what we assume about the world
  verification    ; List: how to verify correctness
  belongs-to      ; Symbol: parent feature
  role            ; String: role within parent
  members)        ; List: child functions/classes
```

This structure captures:
1. **Purpose**: The primary reason for existence
2. **Failure Modes**: Anticipated ways it can fail (linked to goals via `:violates`)
3. **Goals/Constraints/Assumptions**: Context for understanding behavior
4. **Hierarchy**: Relationship to larger features

### Features are Organizing Units

A **feature** is a cohesive unit of functionality with its own intent. Features form hierarchies:

```
security (root)
  ├─ user-authentication
  │    ├─ password-hashing
  │    └─ session-management
  └─ encryption
       ├─ at-rest
       └─ in-transit
```

Functions and classes belong to features, creating a navigable intent graph.

### Intent is Queryable

Once defined, intent can be queried at runtime:

- `get-intent` — Retrieve intent for a function, class, struct, condition, or method
- `method-intent` — Retrieve intent for a specific method specialization
- `intent-chain` — Trace intent up the feature hierarchy
- `feature-members` — List all functions, classes, structs, conditions, methods in a feature
- `list-features` — Search features by name or purpose

This enables tools to answer questions like:
- "Show me all failure modes for this subsystem"
- "Which functions implement user-authentication?"
- "Why does this function exist?"
- "What is the intent of this specific method specialization?"

---

## Design Decisions

### Why Macros, Not Annotations?

Telos uses macros (`defun/i`, `defclass/i`, `defstruct/i`, `define-condition/i`) instead of annotations or metadata.

**Rationale**:
- Macros integrate with `defun`/`defclass`/`defstruct`/`define-condition` syntax naturally
- Intent clauses live with the code, not separate files
- Macro expansion can enforce structure and register members
- No external tooling required for parsing

**Trade-off**: Macros add syntax. But the syntax mirrors standard definition forms, minimizing learning curve.

### Why Store Intent on Symbols and Classes?

Intent is stored in different locations depending on the entity type:
- **Functions** (`defun/i`, `defintent`): On the symbol's plist under `'telos:intent`
- **Structs** (`defstruct/i`): On the symbol's plist under `'telos:intent`
- **Conditions** (`define-condition/i`): On the symbol's plist under `'telos:intent`
- **Classes** (`defclass/i`): On the class object via `intentful-class` metaclass
- **Classes** (`defintent`): In `*class-intent-registry*` hash table
- **Methods** (`defintent` with specializers): In `*method-intent-registry*` hash table

**Rationale**:
- Symbol plists are fast and idiomatic in Lisp
- Metaclasses provide clean integration with CLOS
- Method registry uses `equal` test to match specializer lists
- No external database required
- Works with Lisp image persistence

**Trade-off**: Intent does not survive separate compilation without image. For long-term persistence, serialize the registries.

### Why Not Docstrings?

Docstrings are great for describing HOW to use code. Telos captures WHY code exists.

**Comparison**:

| Aspect | Docstrings | Telos Intent |
|--------|------------|--------------|
| **Purpose** | How to use this | Why this exists |
| **Structure** | Freeform text | Structured data (plist) |
| **Queryable** | No (just string search) | Yes (programmatic access) |
| **Hierarchy** | None | Feature hierarchy |
| **Failure Modes** | Implicit | Explicit |

Telos complements docstrings. Use both:
- **Docstring**: "Returns T if user is authenticated"
- **Intent**: `:purpose "Prevent unauthorized access" :failure-modes ((:bypass ...)) :belongs-to user-authentication`

### Why Separate `defintent` for Retrofitting?

`defintent` adds intent to existing code without modifying definitions.

**Rationale**:
- Legacy code may use `defun`/`defclass`/`defstruct`/`define-condition` without rewriting
- Third-party libraries can be annotated
- CL built-ins can receive intent (e.g., `(defintent hash-table :purpose ...)`)
- Method specializations can receive intent via list syntax: `(defintent (generic-name specializer) ...)`

**Use Case**: Build an intent layer over an existing codebase incrementally, including specific method specializations.

### Why `intent-chain` Returns a List, Not a Tree?

`intent-chain` returns a flat list of plists, not nested data.

**Rationale**:
- Simple to consume (map, filter, reduce)
- Matches common use case: "trace from this function to root"
- Easy to print and display
- Avoids deep recursion in query code

**Trade-off**: For complex graph queries (siblings, cousins), use `feature-members` and `feature-children` in combination.

---

## Mental Models

### Model 1: Intent is Context

Think of intent as the **context** needed to understand code:

```
Code (WHAT/HOW) + Intent (WHY/WHAT-IF) = Understanding
```

Without intent, you read the code and infer context from variable names, comments, and surrounding code. With intent, the context is explicit.

### Model 2: Features are Documentation Units

A **feature** is a self-contained documentation unit:

```
Feature (Purpose, Goals, Failure Modes)
  ├─ Functions (Roles, Specific Failure Modes)
  └─ Classes (Roles, State Assumptions)
```

Features organize intent hierarchically. When you ask "Why does this code exist?", you trace the feature hierarchy to get progressively broader context.

### Model 3: Intent is a Graph

Telos creates an **intent graph**:

- **Nodes**: Features, functions, classes
- **Edges**: `belongs-to` relationships
- **Attributes**: Purpose, failure modes, goals

Query the graph to navigate the codebase by intent instead of syntax.

---

## Limitations and Trade-offs

### Not a Replacement for Tests

Telos documents **intent**, not **correctness**. Tests verify behavior; Telos explains why that behavior matters.

Use both:
- **Tests**: "This function returns correct results"
- **Telos**: "This function exists to prevent XSS attacks"

### Intent Can Drift

Like comments, intent can become stale if not maintained. Mitigation strategies:
- Keep intent near code (via `defun/i`, `defclass/i`)
- Link failure modes to goals (`:violates` clauses)
- Review intent during code review
- Query intent in CI to detect missing or incomplete entries

### Overhead for Small Projects

For small scripts or throwaway code, Telos is overkill. Use it for:
- Long-lived codebases
- Multi-developer projects
- Security-critical systems
- Complex domains

### No Cross-File Search Without Tooling

Telos provides runtime queries, but not cross-file search. For "Show me all functions with failure mode X", write custom tooling that loads the system and queries the registries.

Future work: Static analysis tools that parse intent without loading code.

---

## When to Use Telos

### Use Telos When:

- **Rationale matters**: Security, compliance, or complex domains require understanding WHY
- **Onboarding is frequent**: New developers need to understand code quickly
- **Maintenance is long-term**: Code lives for years and original authors leave
- **Failure modes are critical**: You need to audit what can go wrong
- **Features evolve**: You need to track which code belongs to which feature

### Skip Telos When:

- Throwaway scripts or prototypes
- Code is self-explanatory and trivial
- Single-developer projects with no handoff
- Performance-critical paths where metadata overhead matters (though intent is only stored at definition time, not runtime)

---

## Comparison to Alternatives

### Telos vs. Comments

| Aspect | Comments | Telos |
|--------|----------|-------|
| **Structure** | Freeform | Structured (plist) |
| **Queryable** | No | Yes (programmatic) |
| **Maintenance** | Manual, prone to drift | Structured, easier to audit |
| **Tooling** | None | Query API, potential for linters |

**Verdict**: Telos complements comments. Use comments for implementation notes, Telos for high-level intent.

### Telos vs. Docstrings

| Aspect | Docstrings | Telos |
|--------|------------|-------|
| **Focus** | HOW to use | WHY it exists |
| **Audience** | API users | Maintainers, auditors |
| **Structure** | String | Structured (plist) |
| **Hierarchy** | None | Feature-based |

**Verdict**: Use both. Docstrings describe usage, Telos describes rationale.

### Telos vs. ADRs (Architecture Decision Records)

| Aspect | ADRs | Telos |
|--------|------|-------|
| **Scope** | High-level decisions | Code-level intent |
| **Storage** | Separate files | Embedded in code |
| **Queryable** | Via file search | Via runtime queries |
| **Granularity** | Coarse (system/module) | Fine (function/class) |

**Verdict**: Use both. ADRs document major decisions, Telos documents implementation rationale.

### Telos vs. Design by Contract (Eiffel, Racket)

| Aspect | Contracts | Telos |
|--------|-----------|-------|
| **Focus** | Preconditions/postconditions | Purpose, failure modes |
| **Enforced** | At runtime (assertions) | No (documentation only) |
| **Goal** | Correctness | Understanding |

**Verdict**: Telos is not a contract system. It documents intent, not invariants. Use contracts for correctness, Telos for context.

---

## Future Directions

Potential enhancements (not yet implemented):

1. **Static Analysis**: Tools to parse intent without loading code
2. **Visualization**: Graph diagrams of feature hierarchies
3. **Linting**: Detect missing or incomplete intent
4. **Export**: Generate documentation from intent (Markdown, HTML)
5. **Link to Tests**: Associate tests with failure modes
6. **Recursive Queries**: `list-features :under 'root-feature` (descendants, not just children)

---

## Summary

Telos makes the WHY of code explicit, structured, and queryable. It organizes intent around features, attaches intent to functions and classes, and provides a query API for runtime introspection.

Use Telos when understanding rationale, failure modes, and feature relationships is critical. Combine it with tests, docstrings, and ADRs for comprehensive documentation.

---

**Navigation**: [← Reference](reference.md) | [README](../README.md) | [Use Cases →](use-cases.md)
