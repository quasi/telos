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

#### Decision values are literal data

`deffeature`'s `:decisions` clause quoted `:over` but evaluated `:id`, `:chose`, `:because`,
`:date` and `:decided-by`. Nothing announced the asymmetry, so `:over (list "a" "b")` stored
the unevaluated form `(LIST "a" "b")` and queries then reported an alternative literally named
`LIST`, while `:chose (f)` evaluated normally.

All six fields are now literal, matching every other nested field in the library. The boundary
is: **nested structured fields are literal data; scalar top-level fields may be computed.**
`record-decision` remains the path for a computed decision.

Decision values are also type-checked at macroexpansion time against the `decision` struct's
slot types — `:id` a keyword, the rest strings, `:over` a list of strings. This moves an
existing constraint earlier with a better message: previously a wrong type either signalled a
`make-decision` type error at load time or, for `:over`, was stored silently. The one genuinely
new rule is that `:over`'s elements must be strings; the struct only required a list.

An inline `:id` given as a symbolic constant (`+decision-id+`) no longer resolves; use
`record-decision`.

#### Cyclic `:belongs-to` no longer kills the image

`(deffeature a :belongs-to b)` with `(deffeature b :belongs-to a)` made `intent-chain` push an
entry per iteration until the heap was exhausted — `Heap exhausted, game over`. The parent walk
now stops on a repeated feature.

A cycle is not rejected at declaration time, deliberately. Reloading a file that re-points a
parent can be transiently cyclic mid-load, and rejecting it would break a legitimate reload —
the same reason `:violates` is not resolved at macroexpansion time. The principle: **local
shape is strict at macroexpansion; cross-declaration topology is reported by an audit, not
enforced by the macro.** The audit is not in this release.

### Added

- `invalid-intent-declaration`, a distinct condition (subtype of `program-error`) so this class
  of mistake can be grepped, handled, and escalated rather than scrolling past in a build log.
  Readers: `invalid-intent-declaration-context`, `-field`, `-entry`, `-key`, `-expected`,
  `-reason`.
- `src/validation.lisp`, `tests/validation-test.lisp`, and cycle tests in
  `tests/query-test.lisp` (97 new checks; 269 total).
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
