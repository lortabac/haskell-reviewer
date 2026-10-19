# Haskell Style Guide

Haskell conventions for haskell-reviewer. The rules are defaults with a
reason, not laws. Breaking one is fine when there's a reason, but write the
reason down (a comment or a `Note`).

What the tool does: `SPEC.md`.

## Simplicity over cleverness

Prefer direct, easy-to-read code over abstractions that only pay off in some
future that may never come. If DRY would make code much harder to follow,
accept some duplication. If full type safety needs types that hide the
intent, give up some safety for clarity.

The test: can a reader new to the module follow the code without chasing
several layers of indirection? Three similar lines side by side beat a clever
combinator nobody will use again.

If a function is pure, keep it pure. Wrapping it in a monad just for
uniformity adds noise for nothing.

## Make invalid states unrepresentable

Shape data types so they encode the business logic and its invariants as
precisely as the type system allows. A constructor that can't occur, a field
that is always `Just`, a list that is never empty: each is an invariant the
types can carry for you instead of the reader. Prefer sum types over
booleans plus conventions, non-empty containers over "list, but never empty",
and dedicated constructors over sentinel values.

This rule gives way to *Simplicity over cleverness*. When a precise encoding
needs types that hide the intent (deep GADTs, type-level tricks, phantom
indices every call site has to thread), drop the precision. Use a looser type
and a comment that names the invariant.

## Named, unique data types

Prefer `newtype` over naked types for identifiers and semantic scalars. It
costs nothing at runtime and turns a slip like passing a line number where a
column is expected into a type error.

Prefer records over tuples for product types. Field names document the code,
and adding a field later doesn't break call sites that match on shape. Tuples
are fine for purely structural pairs (e.g. a local zip), not for domain
values.

## Records use bare field names

Enable `OverloadedRecordDot`, `NoFieldSelectors` and `DuplicateRecordFields`
for the whole project, in the `.cabal` files. Field names are bare, with no
prefix; access them with `x.field`. Two records in the same module can share a
field name: that's the point of the three extensions.

Don't prefix field names with an abbreviation of the type (e.g. `cArgs`).

## No partial record selectors

A record field must exist in every constructor of its type. If only some
constructors of a sum have a field, its selector is partial: GHC's
`-Wincomplete-record-selectors` (in `-Wall` from 9.14) flags the access, and
the access throws at run time on the constructors without the field. Give
each constructor its own single-constructor record as payload, and match the
constructor before reading fields from the payload. `-Wpartial-fields`
catches the definition itself, but it's not in `-Wall`, so the rule is on the
author.

## Errors as data

Each part of the program that can fail defines its own error type. Report
failures through `Either` (or an accumulator, when you collect many), not as
exceptions. The type signature then says what can go wrong, and callers can
handle errors by matching on them.

Keep exceptions (and `error`) for truly unexpected failures: broken
invariants, impossible cases. Not for predictable problems the user can hit.

## Documentation

Every public definition gets a Haddock `-- |` comment that says what it is
and, when it's not obvious, why it exists. Explain reasons and constraints.
Don't repeat what the type signature already says.

For decisions that span several definitions, use GHC-style notes:

```haskell
{- Note [Fallback to syntactic resolution]

When no .hie file matches the source, we resolve names from imports and
export lists only...
-}
```

Put a `Note [Title]` in one module and point to it from others:

```haskell
-- See Note [Fallback to syntactic resolution] in Some.Module.
```

This keeps long explanations in one place, so copies don't drift apart.

Describe the code as it is now. How it got there doesn't matter. Short
history notes are OK, sparingly, to stop people from trying again something
that already failed or was rejected.

## Module hygiene

Every module has an explicit export list. Group exports and internal helpers
with Haddock section comments (`-- * Types`, `-- * Operations`,
`-- * Internal`). The export list is the module's public contract: reading it
should tell you what the module offers without scrolling the body.

Import container and utility modules qualified:

```haskell
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
```

Open imports are only for modules whose names are part of the project's
shared vocabulary (e.g. the project's own core types).

## Mind compilation times

Avoid features with a big compile-time cost unless the payoff is clear. Don't
use `Generic` or `DeriveGeneric`. Write explicit instances or small helper
functions instead.

Avoid type families.

## Formatting

`ormolu` is the formatter, with default settings. Run it before committing:

```
ormolu -i $(git ls-files --cached --others --exclude-standard '*.hs')
```

Keep lines to about 90 characters or less. `ormolu` doesn't enforce a limit,
so this is on the author: break long expressions, signatures and comments
before they wrap. The only exception is a long literal (a URL, a fixture
string, a generated name) that can't be split without hurting readability.

## Natural language

Keep comments and docs simple, short and natural. No heavy AI style. Imagine
you're a lazy programmer who wants to minimize typing.
