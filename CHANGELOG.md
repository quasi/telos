# Changelog

All notable changes to telos. Format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
versioning follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] — 2026-07-30

First stable release. The API has been in real use and has settled; this release makes
declarations strict so a declaration can be trusted to mean what it says.

### Changed (breaking)

Every intent macro now validates what it was given at macroexpansion time and signals
`invalid-intent-declaration` instead of silently discarding it. Previously an unrecognized key
was dropped without a word, and the declaration then answered queries as though the field had
never been written — a silently lossy declaration is worse than no declaration at all.

Declarations that carry stray keys will now fail to compile. That is the point: they were
already lossy, they just did not say so. The error names the offending key, the accepted keys,
and the declaration it came from.

Rejected, in `deffeature`, `defun/i`, `defclass/i`, `defstruct/i`, `define-condition/i`,
and `defintent`:

- **Unknown keys inside nested entries** — goals, constraints, assumptions, verification,
  failure modes, decisions. Goal/constraint/assumption/verification entries accept no keyword
  options; failure modes accept `:violates`; decisions accept `:id`, `:chose`, `:over`,
  `:because`, `:date`, `:decided-by`.
- **Unknown top-level clauses** in the macros that read clauses from a body or option list
  (`defun/i`, `defstruct/i`, `defclass/i`, `define-condition/i`). In `defun/i` this also fixes a
  correctness bug: because clauses are popped off the front of the body, a misspelled clause
  consumed the following body form too, leaving the function returning `nil`. A keyword-headed
  form is never a legal Common Lisp body form, so no valid code is rejected.
- **A clause carrying more than one value** — `(:goals (...) (...))` kept the first list and
  dropped the second. Standard `defclass`/`define-condition` options such as
  `(:default-initargs :a 1 :b 2)` are unaffected.
- **Duplicate keys** — `(:f1 "d" :violates :a :violates :b)` silently kept only the first.
- **Field values that look like forms** — `:goals '((:g1 "d"))`. Field values are taken
  literally, never evaluated; the quoted form used to be stored verbatim.
- **A keyword where an entry's description belongs** — `(:f1 :violates :g1)` reads as an option
  to the validator and as a description to anything taking `(second entry)`. Write
  `(:f1 "description" :violates :g1)`. An entry may still omit its description entirely: `(:g1)`.
- **Malformed shapes** — dangling keys, dotted entries, non-list field values.

`defclass/i` and `define-condition/i` also reject options that are neither intent clauses nor
standard options for the underlying macro, at macroexpansion time rather than as an unrelated
initarg error at load time.

### Added

- `invalid-intent-declaration`, a distinct condition (subtype of `program-error`) so this class
  of mistake can be grepped, handled, and escalated rather than scrolling past in a build log.
  Readers: `invalid-intent-declaration-context`, `-field`, `-entry`, `-key`, `-expected`,
  `-reason`.
- `src/validation.lisp`, `tests/validation-test.lisp` (74 new checks; 246 total).
- Documentation of the strictness rules in `docs/reference.md` and both shipped skills.

### Fixed

- `src/storage.lisp` referenced `*decision-registry*` before `src/decision.lisp` defined it,
  producing a compile warning on every build. The library now builds warning-free — which
  matters, since a warning lost in build noise is how the swallowed-key bug went unnoticed.
- `record-decision` examples in `README.md` and the integration skill passed `:over` an
  unquoted list, which is an illegal function call. `record-decision` is a function, so its
  arguments are evaluated: `:over '("a" "b")`.

### Migration

Compile your declarations. Each error names the key and the accepted set; fix or delete the
key. If a field seems to be missing, it usually belongs at a different level — rationale for a
decision goes in `:because`, and constraints belong on the feature (`intent-constraints`), not
on a decision.

## [0.1.0] — 2026-02-05

Initial implementation: `intent` struct; `deffeature`; `defun/i`, `defclass/i` (via the
`intentful-class` metaclass), `defstruct/i`, `define-condition/i`, `defintent`; decision
tracking (`record-decision`, `feature-decisions`, `list-decisions`, inline `:decisions`); query
API (`get-intent`, `intent-chain`, `feature-members`, `feature-intent`, `intent-feature`,
`list-features`); method specializer support; library-shipped dev and integration skills.
