# Changelog

All notable changes to telos. Format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
versioning follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.2.0] — 2026-07-30

### Added

#### `:mitigation` on failure modes

A failure mode may now say how to recover from it, not only which goal it breaks:

```lisp
:failure-modes ((:stale-response "A response arrives after the option expired"
                 :violates :timely
                 :mitigation "Check expires-at before executing"))
```

The vocabulary was too small, not the validator too strict: 1.0.0 accepted only `:violates`,
so a declaration carrying the field a reader most wants — what to *do* about the failure — was
rejected outright. The entry was stored verbatim all along; what was missing was permission to
write it and a way to read it back.

#### Entry accessors — `intent-entry-id`, `intent-entry-description`, `intent-entry-option`

Entries are literal lists of the shape `(:id)` or `(:id "description" . options)`. Consumers
had to know that shape and walk it. They no longer do:

```lisp
(intent-entry-option mode :mitigation)   ; => "Check expires-at before executing"
```

They are structural, not schematic: they report what the entry holds, not what it ought to.
Ids are keywords and descriptions are strings by convention, but the macros do not enforce
that and these accessors do not pretend otherwise.

All are read-side and total: a non-entry — `nil`, a bare keyword, a dotted, circular, or
odd-length option tail — has no id, description, or options rather than signalling the way a
bare `getf` would, or looping the way a naive proper-list check would. `intent-entry-list`
extends the guarantee to the collection, returning a second value that says whether the field
could be walked at all, so a caller can report an unwalkable field instead of dying on it.
`make-intent` is exported and validates nothing, and
`check-intent-references` sweeps the whole image, so one malformed entry must not take the
audit down with it. That audit now reads `:violates` through `intent-entry-option` instead of
its own private accessor.

The `intent-entry-` prefix is deliberate rather than verbose: `entry-id` and
`entry-description` are exactly what `(defstruct entry id description ...)` generates, and a
downstream package that `:use`s Telos would have clobbered them with nothing but a warning —
silently breaking the audit, which is the failure mode this library exists to prevent.

#### `define-entry-option` — extend the vocabulary without a release

A project with its own conventions should not need a Telos release to name a field Telos never
thought of:

```lisp
(define-entry-option :failure-modes :detected-by :severity)
```

Strictness is unchanged and deliberately so: an unrecognized key is still an error, so a typo
in your own vocabulary is caught like any other — as is a typo in the *field*, with or without
keys after it. Extending is an explicit act, not a silent widening. Declarations are validated
at macroexpansion time, so this must be compiled before the declarations that use it; the
`eval-when` carries it into a fasl and into the rest of its own file. Returns `keys`.

Two limits worth knowing: a forced reload of Telos resets the table to the built-in
vocabulary, and neither the macro nor `add-entry-option` takes a lock. Both say the same
thing — extend at load time, from one thread, in a file that loads before the declarations it
affects.

### Changed

- `check-intent-references` gained a fourth code, `:malformed-field`, whose `:reference` names
  the field. It documents that it never signals, but it walked `:goals` and `:failure-modes`
  with `mapcar` and `dolist` — and `make-intent` will store a dotted or circular cons in a slot
  declared `list`. A field the audit cannot walk is now a finding rather than a `type-error`
  or, worse, a hang. The rest of that entity's fields are still audited.
- `invalid-intent-declaration-reason` gained `:unknown-entry-field` and `:invalid-option-key`,
  both from `define-entry-option` — a field that takes no entry options, and a non-keyword
  option key.
- `*intent-entry-option-keys*` is now built with `list` rather than quoted, so
  `add-entry-option` can push onto it without modifying literal source data.

## [1.1.0] — 2026-07-30

### Added

#### `check-intent-references` — an audit for the intent graph

`:violates` names a goal, but nothing checked that the goal existed, so a typo'd or renamed id
made a failure mode look constrained when it wasn't — the same lie as a swallowed key, one
level up.

It cannot be checked as a declaration is read: the goal may live on a feature that is not
defined yet, and a member legitimately violates a goal declared on its parent (the library's
own `examples/csv-validator.lisp` does exactly that). So the check runs over the finished
image, on demand:

```lisp
(check-intent-references)
;; => ((:severity :error :code :dangling-violates :entity login :entity-type :feature
;;      :reference :secrue :message "Failure mode :LEAK of LOGIN violates :SECRUE, ...")
;;     ...)
```

Codes, which are API: `:dangling-violates`, `:undefined-parent`, `:cyclic-hierarchy`. Findings
are sorted, so a human or a CI diff sees a stable report despite hash-ordered registries. It
never signals — a mid-load image legitimately shows dangling references.

`assert-intent-references` is the one-line form for a test or CI; it signals
`intent-reference-error`, which carries the findings.

The principle this completes: **local shape is strict at macroexpansion; cross-declaration
topology is audited on demand.**

#### `all-intentful-classes`

`defclass/i` keeps intent on the class metaobject, so such classes — especially one with no
`:feature` — appeared in no registry and were invisible to anything sweeping the image. The
metaclass now records its instances' names.

The index is a candidate list, never truth: names are re-derived through `find-class` on every
read, so a stale entry drops out instead of becoming a phantom finding. That is the rule the
next item applies too.

### Changed

- **Duplicate entry ids within one field are rejected** at macroexpansion time. Two goals with
  one id make `:violates` ambiguous and hide a description. Ids are compared within a field, so
  a goal and a constraint may still share one.

### Fixed

- **`feature-members` no longer reports a member that has moved.** `register-member` only ever
  pushed, so re-declaring an entity under a second feature left it listed under both — a query
  answering with something no declaration says. Membership is now verified on read against each
  member's own intent, which also sees a later `defintent` re-pointing an entity; un-registering
  at declaration time could not.

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
