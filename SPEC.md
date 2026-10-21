# haskell-reviewer — Specification

Status: **v1.1 — approved**: v1.0 plus the architecture and tech stack
(§11). The rationale for every decision is in
[DECISIONS.md](DECISIONS.md) (D-01 to D-88). Items to verify during
implementation, none of which block this spec, are in
[OPEN-QUESTIONS.md](OPEN-QUESTIONS.md). Per §10.3 the spec may be revised
from benchmarks; every revision is logged in DECISIONS.md.

§1–§10 specify *behaviour*. §11 gives the high-level architecture and the
tech stack, with no low-level implementation details.

---

## 1. Purpose

`haskell-reviewer` helps a person **understand a Haskell project, and review a
change to it**. It is a reading tool: it never modifies the project, never
builds or runs it (see §9), and never publishes anything.

Guiding principles, in priority order:

1. **Types first.** Type definitions, class definitions and type signatures are
   the most important content of a Haskell project and are always the most
   prominent thing on screen.
2. **Performance.** Start-up and navigation must feel instant. A feature
   whose cost in latency or memory is too high is degraded or dropped,
   rather than kept slow. The targets in §10 are part of the spec, and the
   spec is expected to be **re-adjusted during implementation** according to
   benchmarks (D-36).
3. **Small complexity budget.** Every feature must earn its place. When in
   doubt, leave it out. §12 lists features rejected on purpose.
4. **Familiarity.** The main view looks and behaves like Hackage/Haddock
   documentation, which every Haskeller already knows how to read.
5. **Honesty about precision.** The tool states how exact its semantic
   information is (§3) and never shows a guess as if it were a fact.
6. **Read-only and contained.** Neither the tool nor its LLM integration can
   change anything or reach outside the project.

### 1.1 Non-goals

- Not an editor or IDE: no editing, no refactoring, no building.
- Not a code-review platform: no comments, approvals or GitHub/GitLab
  integration.
- Not a browser for dependencies: external names link out to Hackage.

---

## 2. Invocation and inputs

```
haskell-reviewer [ROOT] [--diff REVSPEC] [options]
```

- `ROOT` (default: current directory) is the project root: normally a
  directory containing `cabal.project` or `*.cabal` files. **Stack is not
  supported** (D-66): `stack.yaml` is never read. Any other directory is
  accepted in degraded form (§8). In diff mode
  the project structure is determined **separately for each side**, so a
  change to `.cabal` files (new modules, changed extensions or source dirs)
  is reflected correctly.
- `--diff REVSPEC` turns on **diff mode**. `ROOT` must then be inside a git
  work tree. Accepted forms:
  - `BASE..HEAD`: compare two commits (any git rev syntax).
  - `BASE...HEAD`: compare `merge-base(BASE, HEAD)` with `HEAD` (PR semantics).
  - `REV`: compare `REV` with the **working tree** (including uncommitted and
    untracked-but-not-ignored files).
- Without `--diff` the tool is in **browse mode** and shows the working tree.
- **ROOT bounds everything** (D-43). When ROOT is a subdirectory of the git
  work tree (a monorepo), the views, the diff and the LLM's visible set
  include only paths under ROOT. The header shows the number of changed
  files *outside* ROOT, with their list in a tooltip, so nothing is hidden
  silently.

On start the tool indexes the project, starts a local HTTP server bound to
`127.0.0.1` on a free port (or `--port N`), and opens the browser
(`--no-open` turns this off). The URL contains a random session token, and
requests without it are rejected. In addition, requests whose `Host` header
is not `127.0.0.1:<port>` or `localhost:<port>` are rejected. Together these
protect against other local users and DNS-rebinding attacks.

### 2.1 Process lifecycle and CLI contract

(D-30)
- The tool runs as a **foreground** process, and there is never a daemon.
  The URL is printed to **stderr**, as are progress and warnings; stdout is
  unused.
- It exits on **Ctrl-C / SIGTERM**, or when no browser tab has been
  connected for **5 minutes** (`--idle-exit MINUTES`; `0` turns this off).
  A browser counts as connected while its event stream is open (§11.6.2,
  D-52). The page sends no heartbeat; the server sends a keep-alive on the
  stream. A tab that is reopened within the
  idle timeout reconnects to the same session.
- **Exit codes**: `0` normal exit (including idle exit), `1` internal error,
  `2` bad usage or invalid input (unknown revision, not a git repo when
  `--diff` is given, unreadable ROOT).
- `--help` lists every flag with a one-line description. `--version` prints
  the tool version, the bundled parser's GHC version, and the HIE
  companions found.

### 2.2 Revisions ("sides")

In diff mode there are two **sides**: *base* and *head*. Each side is a
complete, independently indexed snapshot of the project. Files are read
straight from git objects (for the working tree: from disk), so no checkout
takes place and the user's working tree is never touched.

In browse mode there is exactly one side.

### 2.3 Project model

- **Packages**: every local package listed in `cabal.project`
  (`packages:` globs). Without `cabal.project`, the `*.cabal` files in ROOT,
  as cabal itself does.
  **Hpack is not supported** (D-41). A package that has `package.yaml`
  but no `.cabal` file on a side is treated as a directory without a
  project (§8), and the banner says so. Hpack users can commit the
  generated `.cabal` file, or generate it before reviewing.
- **Components**: library (including sub-libraries), executables, test
  suites, benchmarks. Each has its source dirs, default extensions and
  language.
- **Modules**: the `.hs`, `.lhs` and `.hsig` files that belong to a component.
  Files that belong to no component are still indexed and shown under
  "Other files". `.hs-boot` files are not modules (no identity, D-44):
  they show under "Other files" as text, not indexed. `main-is` is
  relative to `hs-source-dirs` and may contain `..`; the path must stay
  inside the snapshot.
- **Generated modules**: modules whose source is a preprocessor input
  (`.hsc`, Alex `.x`, Happy `.y`, `.chs`, …) cannot be produced without
  running tools (§9.1). They are shown as plain text under their component,
  with a "generated: not indexed" banner, and contribute no declarations.
- **Listed, no source** (D-84): a module listed in a `.cabal` file with
  no source file and no preprocessor input (an autogen module like
  `Paths_*`, one made by a custom Setup, or by codegen not listed in
  `autogen-modules`) shows as "listed, no source" under its component.
  It has no declarations.
- **Other preprocessors** (D-84): a module run through `-F -pgmF` (e.g.
  hspec-discover) or any `-pgmL` tool is shown as text, with a
  "preprocessed by <tool>, not indexed" banner, since we never run project
  preprocessors (§9.1). The one exception is `-pgmL markdown-unlit`,
  supported approximately: the `.lhs` file is read from its
  ```` ```haskell ```` code blocks only. If no code blocks are found, it
  gets the same banner.
- **Module identity** (D-44): a module is identified by
  `<package>:<component>:<Module.Name>`, because the same module name can
  appear in several components (every executable's `Main`, or the same
  module in a library and a test suite). URLs, matching keys, LLM tools and
  the manifest use the short form `Module.Name` only when it is unique on
  that side, and the full form otherwise.
- **Names from dependencies** (D-42): shown with their package and linked
  to Hackage. They are not browsable in-app. The owning package is found
  from, in order:
  1. `dist-newstyle/cache/plan.json`, when present (read-only; gives exact
     versions);
  2. HIE files (exact package);
  3. the component's `build-depends`, plus a module lookup in the local
     Cabal package index on disk (read-only, no network);
  4. otherwise a **Hoogle search link** for the name.

  The link is labelled with how it was found, like every other heuristic
  (§3). A version is shown only with source 1 or 2.

---

## 3. Semantic tiers

Semantic information comes from one of two **tiers**, chosen independently
for each side and each module:

| Tier | Source | Gives |
|------|--------|-------|
| **Syntactic** | Parsing with the bundled GHC parser (§7.1), using the project's language edition and extensions | Declarations, signatures, Haddock comments, exports, imports, local scoping. Cross-module resolution is *import-directed*: exact when, after following explicit import and export lists, exactly one candidate remains; heuristic otherwise. |
| **Precise** | `.hie` files from a prior build whose recorded sources match this side (see rules) | Exact resolution of every identifier, types of identifiers at their binding and, where different, at each use (including inferred types of bindings without signatures); no expression types. Instance resolution, references. |

Rules:
- The syntactic tier is always available, needs no build, and works on
  broken or uncompilable code.
- The precise tier is used for a module only when its `.hie` file is
  **verified current**: the source stored in the `.hie` file is byte-for-byte
  identical to that module's file on that side. A stale `.hie` file is never
  used. In addition, every home-package module it imports, transitively,
  must also be verified current, because a change in an imported module can
  change resolved names and inferred types even when this module's text is
  unchanged. The component's build-relevant `.cabal` stanza
  (default-extensions, language, cpp-options, ghc-options) must also be
  identical on that side. The check is by content, not by revision, so HIE
  files from a working-tree build also serve a committed side wherever the
  files agree.
- **Only the user's own build output is trusted** (D-56). A `.hie` file
  that is part of a snapshot (tracked in git, or present in the base or
  head revision), or reached through a symlink in the snapshot, is never
  used, since a change under review could ship forged ones. Without git
  (browse mode only) this can't be checked: HIE files are used, and the
  banner says so. HIE reading runs in a separate, resource-limited process
  (§11.4.6).
- Every place that shows semantic information marks heuristic results
  visibly (e.g. a dotted underline on a link, with a tooltip saying "resolved
  heuristically").
- The header shows the active tier for each side, e.g.
  `base: syntactic · head: precise (41/43 modules)`.

> In diff mode, the base side usually has no HIE files, since builds only
> exist for the working tree. This asymmetry is expected and shown clearly.

### 3.1 Things that cannot be parsed

- **CPP** (D-25): modules using `CPP` are preprocessed in-process with
  deterministic assumptions, since no build plan exists:
  - `os(…)` / `arch(…)` conditionals and the platform macros follow the
    **host** platform.
  - Cabal flags take their `.cabal` **default** values, unless
    `cabal.project` sets them (D-67).
  - `MIN_VERSION_pkg(…)` evaluates to **true** for any package. But
    `defined(MIN_VERSION_pkg)` and `#ifdef MIN_VERSION_pkg` are true only
    for packages in the component's `build-depends` on that side, with `-`
    in names turned into `_` (D-84). `__GLASGOW_HASKELL__` and
    `MIN_VERSION_GLASGOW_HASKELL(…)` are both derived from the **detected
    project GHC version** (§7.1), falling back to the parser's version, so
    the two can never disagree.
  - Cabal's generated macros (`VERSION_pkg`, `CURRENT_PACKAGE_VERSION`,
    `CURRENT_COMPONENT_ID`) are synthesised from the `.cabal` file on that
    side. As in `cpp`, undefined macros evaluate to 0 in `#if`.
  - `#include` is resolved only **inside the side's snapshot**, using the
    component's `include-dirs`. Anything else (system headers, generated
    headers) is treated as empty, and a warning is shown. GHC's
    `MachDeps.h` (`WORD_SIZE_IN_BITS`, …, for the host) may be built in.
    Not required; planned for M5 (D-84).
  - `--cpp-define NAME[=VAL]` (repeatable) and `--flag [PKG:][-]NAME`
    override any of these. A bare `NAME` applies to every package that
    declares the flag.
  - When a module is at the precise tier, the CPP branches the HIE build
    actually took win over these assumptions, and the UI says so.
  - Excluded regions stay **visible, greyed out**, in the source and diff
    views. They contribute no declarations. The header tooltip lists the
    assumptions in effect.
  - A module with CPP on but no directives needs no CPP pass: the text is
    used as-is (D-84).
- **Template Haskell / quasi-quotes**: splices are shown as written.
  Declarations generated by splices are only known at the precise tier.
- **Parse failures**: the module is still shown in the source and diff views
  with a banner giving the parse error. It contributes no declarations. It
  is still lexically highlighted when it lexes (§5.6), and otherwise shown
  as plain text.

---

## 4. Views

The UI has a fixed **frame** and one **main view** at a time.

### 4.1 Frame

- **Header**: project name, mode and revisions (`main..feature`, abbreviated
  hashes), tiers, a global search box, and a switch between views.
- **Sidebar**: the module tree, grouped by package and component. In diff
  mode every module carries a change marker (added / removed / modified /
  renamed) and a filter "changed only". The tree also has the "Other
  files" group, and shows "listed, no source" and banner entries (§2.3).
- **LLM panel** (§6): a collapsible right-hand panel.

All views are **URL-addressable** (D-27). URLs are readable and semantic,
and **valid within one session only**: they are never promised to survive a
restart, a reload of the working tree, or a different revision. They make
back/forward, new tabs and copy-paste between tabs work. Shape:

```
/<token>/docs/<side>/<module>[#<decl>]
/<token>/src/<side>/<path/to/File.hs>[#L<n>[-L<m>]]
/<token>/diff/<path/to/File.hs>[?layout=unified|split][#<side>-L<n>]
/<token>/changes[#<decl-id>]
```

`<side>` is `head` / `base` in diff mode and `tree` in browse mode.
`<module>` uses the identity rules of §2.3. `<decl>` is
`<namespace>:<name>`, where the namespace is `type`, `class`, `con`,
`field`, `val`, `pat` or `inst`. This tells the type `Foo` apart from the
constructor `Foo`. Fields always use `field:<Parent>.<name>`, where the
parent is the field's type (e.g. `field:Config.port`), the pattern synonym
for pattern synonym record fields, or the instance's first constructor
for data instance fields. So duplicate record fields never clash (D-88). Instances use `inst:<Class>:<Head>`, with the head
printed as in §4.6 and percent-encoded. The `<decl-id>` used by `/changes`
is the same `<decl>` (fields and instances included) qualified by its
module. An
unresolvable URL shows a "not found in this snapshot" page with a search box
prefilled from the URL, never a blank page.

### 4.2 Docs view (default)

A module page in the style of Haddock:

1. Module header: name, package/component, module Haddock comment,
   extensions in effect.
2. **Synopsis**: a compact list of all declaration heads.
3. **Declarations** in source order, grouped by Haddock section headings when
   present:
   - **Types**: `data` / `newtype` / `type` / `type family` / `data family`,
     with constructors, record fields, strictness/unpack annotations and
     deriving clauses.
   - **Classes**: superclasses, fundeps, methods with signatures, associated
     types, default signatures, `MINIMAL` pragma.
   - **Functions and values**: the signature (or, at the precise tier, the
     inferred type, marked *inferred*), plus its Haddock comment.
   - **Pattern synonyms**, with signatures.
   - **Type-level companions**: standalone kind signatures,
     `type instance` / `data instance` and closed type family equations
     are shown with the type or family they belong to.
   - **Other declarations**: fixity declarations (shown next to the
     operator they affect), `foreign import` / `foreign export` (as
     functions, flagged), and role annotations (with their type). `RULES`
     and `COMPLETE` pragmas appear in a collapsed "Pragmas" block at the end
     of the module page. A signature shared by several names (`f, g :: T`)
     is shown once, under every name it covers.
   - **Instances**: listed under the class and under each type, collapsed by
     default, as on Hackage. Orphan instances are flagged.
4. Every identifier in a type or signature is a **link** to its definition
   (another module page, or Hackage for external names).

Differences from Hackage, made on purpose for reviewers:
- **All top-level declarations** are shown by default, not just exported
  ones (D-10). Exported ones carry a marker. An "exported only" toggle,
  remembered across sessions, gives the Hackage-like presentation. A module
  with a header but no export list exports everything. A module with no
  `module` header at all is `module Main (main) where`, as the Haskell
  Report says.
- Each declaration has a **"source" toggle** that shows its source inline,
  with the same identifier links (§4.3).
- Each declaration has a **"go to source"** action that opens the full-source
  view at that declaration.
- Each declaration has an **"ask"** action (§6).

**In diff mode** (D-13) the docs view shows the head side by default, with a
side switch. Each declaration touched by the diff carries the badge of its
§4.6 primary classification (*signature*, *body*, *docs*, *formatting*,
*added*), plus its *moved* / *renamed* flags. The badge links to that
declaration's diff. Declarations removed on head are
listed in a collapsed "Removed in this change" block at the end of the
module page. The sidebar's "changed only" filter also applies here.

### 4.3 Inline declaration source

The source of exactly one declaration: its signature, all equations, its
`where`-clauses, and any pragmas attached to it (`INLINE`, `SPECIALISE`, …).
An **extent** is a *set* of source ranges, not one range, because a
signature, its pragmas and its equations need not be next to each other.
Non-adjacent ranges are shown in source order, separated by an elision
marker that gives the line numbers.
Rendered with syntax highlighting (§5.6) and the same identifier links as
the full source. Several declarations may be expanded at once.

### 4.4 Full-source view

The whole file, syntax-highlighted (§5.6), with line numbers.
- Every identifier is a link to its definition (go-to-definition).
- Hovering an identifier shows its type when known (precise tier), or else
  its declared signature.
- **Find references**: for a selected identifier, a list of the places where it
  is used. These are exact at the precise tier and textual-but-scoped at the
  syntactic tier, and labelled accordingly.
- A gutter outline shows the top-level declarations, with a link from each
  back to the docs view.
- **Find in file** (D-70): `f`, or a toolbar button, opens a find bar that
  searches the whole file, with a match count announced to screen readers.
  Views are virtualised (§11.7), so the browser's own find (Ctrl-F, left
  untouched) sees only the lines on screen. The toolbar says so
  permanently: "Browser find sees only visible lines — press f to search
  the whole file".
- In diff mode, changed lines carry gutter markers, and a "show diff here"
  action switches to the diff view at the same place.

### 4.5 Diff view

Available in diff mode only. It shows the textual diff file by file.
- Layouts: **unified** and **side-by-side**, toggled globally and remembered
  across sessions.
- Context lines can be expanded step by step, or for the whole file.
- Both sides are syntax-highlighted (§5.6) and navigable: identifiers link to the
  definition on *their own side* (a base-side line links into the base
  snapshot).
- Renames are detected the way git does by default (exact match, then
  content similarity ≥ 50 %) and shown as such. Copies are not detected:
  a copied file shows as added (D-74).
- **Find in file** (`f`, or a toolbar button) searches the whole file on
  both sides, as in §4.4.
- Option to ignore whitespace-only changes.

### 4.6 Expanded (semantic) diff view

Available in diff mode only. Instead of hunks it shows **the declarations that
the diff touches**, rendered the way the docs view renders them.

**Definition of "touched"**: a top-level declaration (or class method, or
instance) is touched if any line of its *extent* changed on either side. The
extent (§4.3) includes its signature, equations, `where`-blocks, attached
pragmas and attached Haddock comment. Declarations are matched across sides
by *(module, namespace, name)*; instances are matched by their instance key
(below).

Each touched declaration gets exactly **one primary class** (D-40). When
several apply, the first one in this order wins:
1. **Added / Removed**: it exists on one side only.
2. **Signature changed**: its type-level header differs. This covers a type
   signature, standalone kind signature, data constructors or fields,
   `deriving` clauses, class methods, or instance context. "Differs" means
   the token streams differ; layout and comments are ignored.
3. **Body changed**: the implementation differs, as token streams.
4. **Docs changed**: the Haddock comment differs.
5. **Formatting only**: only whitespace or non-Haddock comments changed.
   These entries are listed, collapsed, at the bottom.

On top of the primary class, an entry can carry **flags**:
- **moved**: same declaration, now in a different module;
- **renamed** (D-19): a declaration removed on base and one added on head,
  of the same kind, with normalised bodies above a similarity threshold
  (§4.6.1). These are paired and flagged *renamed*, with an "inferred"
  label and a one-click "show as removed + added" override. Each removed
  declaration is paired at most once. Below the threshold they stay
  removed + added.
- **docs also changed**: when the primary class is 1–3.
- **inferred type changed** (precise tier, both sides): a body change that
  also changed the inferred type of a binding without a signature. Such an
  entry is **promoted** into the *Signature changed* group, since it is an
  API change in disguise.

Moving or renaming therefore never buries an API change: a moved
declaration with a new signature is listed under *Signature changed*,
flagged *moved*.

**Instances and deriving** (D-20): every instance is treated as a
declaration, whether written as an `instance`, a standalone `deriving`, or a
`deriving` clause. The **instance key** is the class name plus the instance
head, printed with type variables α-renamed in order of appearance (so
`Maybe a` and `Maybe b` are equal), and with every class parameter for
multi-parameter classes. Like other declarations, an instance can be
flagged *moved* when its module changes. Turning a `deriving` clause into
a standalone `deriving` (or the reverse) counts as a strategy change. Adding or
removing a derived class therefore produces (a) a *signature changed* entry
for the type, since its `deriving` clause is part of its head, and (b) an
*added*/*removed* entry for the instance itself. A change of deriving
strategy (`stock` → `newtype` → `anyclass` → `via T`) counts as a
*signature change* of the instance.

#### 4.6.1 Similarity metric

(D-28) Used for rename detection, and for moves when the name also changed.
1. Each declaration's extent is lexed with the GHC lexer. Whitespace,
   comments and Haddock are dropped.
2. Occurrences of the declaration's **own name**, including recursive
   self-references, are replaced by one placeholder. All other identifiers
   are kept as they are, since normalising them would make unrelated small
   functions look alike.
3. Similarity is the **multiset Jaccard index over token bigrams**. Bigrams
   rather than single tokens keep some order information.
4. A pair is a candidate when similarity ≥ **0.8** (configurable as
   `diff.rename_threshold`). Declarations of fewer than **8 tokens** match
   only when their normalised streams are identical; this avoids pairing
   trivial one-liners.
5. Pairing is greedy, from the highest score down, one-to-one, and ties are
   broken by module, then by name. This makes it deterministic.
6. The score is shown in the "inferred" label's tooltip.

Layout:
- Declarations are grouped by **primary class, then by module**, in this
  order: signature changes, added, removed, body changes, docs changes,
  formatting only. Moves and renames are shown as flags inside these groups,
  and a filter can show flagged entries only. Types and classes come before functions within each group. This
  puts API-level changes on top, in line with *types first*.
- For signature changes, the old and new heads are shown one above the other
  with intra-line highlighting.
- Each entry can be expanded to show the diff of *that declaration only*,
  even when the declaration moved between files.
- **Affected (not modified)**: a final, collapsed section that lists
  untouched declarations which *directly use* a declaration classified as
  *signature changed* or *removed*. "Directly use" means one level of
  references only, with no transitive closure. Each entry names the changed
  declaration(s) it depends on. Users of a **signature-changed**
  declaration are looked up on the **head** side. Users of a **removed**
  declaration are looked up on the **base** side, where the name still
  exists, and listed only if they survive unchanged on head. The precise
  tier gives exact entries; syntactic-tier entries are labelled
  heuristic.
- Changes outside any declaration (imports, export lists, module header,
  pragmas, `.cabal` files, non-Haskell files) are listed in a final
  **"Other changes"** section.

**Navigation**: the view has no identifier links, so that it stays a
summary. Hovering an identifier still shows its type or signature. Clicking
a declaration opens the **full-source view** at that declaration on the head
side, or on the base side if the declaration was removed.

### 4.7 Global search ("jump to")

One search box, fuzzy-matching module names and declaration names (including
constructors, fields and class methods). There is no search by type
(D-14). Selecting a result opens it in the docs view. In diff mode, results are taken from the head side by default.

---

## 5. Cross-cutting behaviour

### 5.1 Sides in navigation

In diff mode every location belongs to a side. Following a link never
switches sides silently. The header shows the current side, and a "view
on other side" action is available wherever the location exists on both.

### 5.2 Snapshots and reload

Each side is a **snapshot** taken at start-up; it never changes while being
read. Revision sides (commits) are immutable anyway. For the working-tree
side there is no file watching. Instead:
- A **Reload** action in the header re-reads the working tree and the HIE
  files, then re-indexes.
- After a reload, the current location is preserved by *(module,
  declaration)* when possible. If it no longer exists, the user lands at the
  nearest enclosing module with a notice.
- LLM threads carry the snapshot they were asked against. Threads from an
  older snapshot are marked *stale* (§6.6).

### 5.3 Keyboard

Every action is reachable by keyboard; a `?` overlay lists the bindings.
The style is single-key and GitHub/Gmail-like (D-21). Bindings are fixed in
v1 and work only when no text input has focus. For WCAG 2.1 SC 2.1.4, one
setting, **"single-key shortcuts: on/off"** (default on, toggle in the `?`
overlay and the user config), turns them all off (D-45). Every action
also has a UI control, so nothing is lost with shortcuts off.

| Key | Action |
|-----|--------|
| `/` | Focus global search |
| `j` / `k` | Next / previous declaration (docs, expanded diff) or hunk (diff) |
| `n` / `p` | Next / previous **change** (any view, diff mode) |
| `]` / `[` | Next / previous changed file or module |
| `Enter` | Open the focused item |
| `g d` | Go to definition of the identifier under the cursor/focus |
| `g r` | Find references |
| `s` | Toggle the inline source of the focused declaration |
| `f` | Find in file (source and diff views) |
| `1` `2` `3` `4` | Docs · Source · Diff · Expanded diff (`3`/`4` diff mode only) |
| `u` | Toggle unified / side-by-side |
| `b` | Switch side (base/head) |
| `a` | Ask the LLM about the focus/selection |
| `y` | Copy the focused item's primary identifier (§5.7) |
| `Esc` | Close the overlay/panel; clear the selection |
| `?` | Show the keyboard help |

### 5.4 Accessibility

(D-29) Target: **WCAG 2.1 level AA**.
- Keyboard-complete (§5.3), with visible focus on every interactive element
  and a logical focus order: header, sidebar, main view, LLM panel. There
  is a "skip to main content" link.
- Semantic HTML first. The sidebar is an ARIA `tree`. Diff tables expose
  per-line roles, with an accessible name for each line ("added line 42",
  "removed line 40"). Identifier links have names that include their
  target.
- The LLM answer stream is an `aria-live="polite"` region that announces
  the result when the answer finishes, not token by token.
- Contrast ≥ 4.5:1 for text in both themes, including syntax-highlighting
  and diff colours.
- `prefers-reduced-motion` turns off animated transitions.
- Virtualised views (§11.7) can't be read top to bottom with a screen
  reader's virtual cursor. Instead, `j`/`k`, `n`/`p` and find in file move
  focus and scroll, and the focused line is announced.
- Text scales to 200% without loss of function. Side-by-side diffs fall
  back to unified below a minimum width.

### 5.5 Visual design

- Hackage-inspired layout (synopsis, declaration blocks, collapsible
  instances) in the tool's own clean style. It is not a copy of Haddock's
  CSS (D-22).
- **Light and dark** themes, following the OS by default, with a manual
  override. No custom theming.
- A monospace font for all code. Signatures wrap at `->` and `=>` the way
  Haddock does, and never scroll horizontally in the docs view.
- Change colours carry meaning (added, removed, modified) and are always
  paired with a second cue (a gutter sign, an icon or a label), for
  colour-blind accessibility.

### 5.6 Syntax highlighting

(D-31) Highlighting is kept deliberately lightweight.
- It is **lexical only**: each token is coloured by its class, from this
  fixed set: *keyword*, *type / constructor* (capitalised identifiers and
  operators beginning with `:`), *variable*, *operator*, *literal*,
  *comment* (including Haddock), *pragma*, and *CPP-excluded*.
- The tokens come from the **GHC lexer run that already produces the
  identifier links**, in its comment-preserving mode. CPP directive lines
  and excluded regions are never seen by GHC's lexer, so they are
  classified by the CPP pass itself and merged back by position. The client
  uses no highlighting library: the server sends a token stream and the
  client maps classes to theme colours.
- It applies to the full-source, inline-source, diff and expanded-diff
  views. Haddock-rendered text in the docs view is not highlighted, though
  code inside it is.
- There is **no semantic colouring**, such as distinguishing locals from
  top-level or imported names. Links and hover already convey that.
- A file that fails to **lex** is shown as plain text. A file that lexes
  but fails to parse is still highlighted (§3.1).
  Non-Haskell files (`.cabal`, `.yaml`, Markdown, …) are always plain text.

### 5.7 Copying

(D-32) Copying important information must take one action.
- A **copy button** appears on hover or focus next to:
  - file paths (copied relative to ROOT);
  - module names;
  - qualified declaration names (`Data.Foo.bar`; for instances, the
    instance head);
  - revision hashes (always the **full** hash, even when the label shows
    an abbreviation);
  - LLM messages, both questions and answers (copied as raw Markdown);
  - each code block inside an LLM answer.
- **Copy current location**, in the header, copies `path:line` (or
  `path:line-line` for a selection) on the current side.
- The `y` key copies the focused item's primary identifier.
- Every copy gives brief visual feedback ("Copied"), and the same message
  is announced to screen readers. Copying never includes UI chrome such as
  line numbers or gutter markers. Views are virtualised (§11.7), so a
  copied selection is built from the file's text, not from the DOM, and a
  long selection copies every line it spans. A plain text selection in the source and
  diff views copies only the code.

---

## 6. LLM integration

### 6.1 Entry points

The user can ask a question:
- **Top-level**: from the LLM panel, about the project or the change in
  general.
- **At a line**: from the gutter of the source or diff view.
- **At a selection**: on any text selected in the source, diff, inline-source or
  docs view.
- **At a declaration**: the "ask" action in the docs and expanded-diff views.

The anchor (side, file, line range, selected text, declaration) is attached to
the question as context and shown as a chip above the question, where the
user can remove it. A selection in the docs view (rendered text, not
source) anchors to the **enclosing declaration's source extent**, with the
selected text included as a quote (D-26).

### 6.2 What the LLM can do

The LLM works in a tool-calling loop with **read-only tools only**. The
tools and their containment are identical for every backend (§6.7). With
the API backends, haskell-reviewer runs the loop itself. With the
`claude-cli` backend the CLI runs the loop, and the tools are served to it
over MCP (§6.8).

| Tool | Description |
|------|-------------|
| `list_files(side, path_glob)` | Lists the files on a side. |
| `read_file(side, path, line_range?)` | Reads a file on a side. |
| `search(side, regex, path_glob?)` | Searches file contents. |
| `get_declaration(side, qualified_name)` | Returns the source and signature of a declaration. |
| `find_references(side, qualified_name)` | Lists the places a name is used (tier-labelled). |
| `get_diff(path?)` | Returns the diff, for the whole change or one file (diff mode only). |
| `list_changed_declarations()` | Returns the data behind the expanded diff view (diff mode only). |

Containment rules:
- Every path is resolved against the snapshot of a side, *not* against the
  file system. `..` segments, absolute paths and symlinks that escape the
  root are rejected.
- **Visible set**: only files tracked by git, plus untracked-but-not-ignored
  files on a working-tree side. Without git (browse mode only): every
  regular file under `ROOT`. The `.git` directory is never visible.
- **Deny-list**: applied *on top of* the visible set, in every mode. It is
  configurable in the user config, and its default is non-empty: `.env*`,
  `*.pem`, `*.key`, `**/secrets/**`, …. Denied files are still shown to
  the human user in the source and diff views. The deny-list filters **every
  byte sent to the LLM**: tool results (including `search` hits,
  `find_references` snippets, `get_diff` and `list_changed_declarations`),
  the manifest and the anchor. Denied files show up only as a count
  ("2 files hidden"). Asking about a denied file shows a refusal chip
  instead of sending the anchor.
- There is no network access, no command execution, and no writing, whether
  to the project, to config, or to anything else. The LLM never receives a
  tool that could do these things.

**Automatic context** (D-16). Each thread starts with three parts, in this
order. Everything else (the diff itself, other code) is fetched through
tools. The user can inspect the exact initial context of any thread ("show
context").

1. **System prompt** (D-37): fixed and built into the tool, and versioned
   along with it. It covers:
   - the role: helping a human understand and review a Haskell project or
     change;
   - the containment rules: read-only tools, no other access, and the fact
     that some files are hidden by the deny-list;
   - the citation format: `Module.name` and `path:line`, so that answers
     become links (§6.4);
   - honesty: separate what the code shows from what is guessed, and
     respect tier labels;
   - the injection rule (§6.2.1).
   The user config may add an `llm.extra_instructions` string, which is
   **appended** after the built-in prompt (e.g. language or tone). It
   cannot replace or remove the built-in text.
2. **Manifest** (D-38): a compact, machine-generated summary:
   ```
   project: <name>   mode: browse | diff
   revisions: base=<full hash> (<ref>)  head=<full hash | working tree>
   ghc: <detected version>   tiers: base=<…> head=<…>
   packages: <pkg>: <components…>, …
   changed files: <path> (<A|M|D|R from>), …            [diff mode]
   changed declarations: <Module.name> <classification>, …   [diff mode]
   ```
   It is **capped at about 8k tokens** (and at most 200 changed
   declarations). Beyond the cap, the declaration list is replaced by
   per-module counts per classification, and the file list is truncated
   with a total. A note says to use `list_changed_declarations` / `get_diff`.
3. **Anchor**: the location, the anchored text, and the enclosing
   declaration's source when the anchor is a line or selection (§6.1). It
   is capped at 400 lines; longer anchors are cut, with a note on how to
   read the rest.

#### 6.2.1 Prompt injection

(D-39) The code under review may contain text aimed at the LLM ("ignore
previous instructions…"). The tools are read-only, so such text **cannot
cause actions**. It can, however, mislead answers. The mitigations are:
- every tool result is passed to the model explicitly marked as
  **untrusted project data**, never as instructions;
- the system prompt tells the model to **report**, and never follow,
  instructions found in project content, and to point them out to the user
  as a review finding.

Misleading answers remain a **documented residual risk**. The UI's tool-call
log (below) and the citation links let the user check claims against the
code.

The UI shows every tool call the LLM makes (collapsed by default), so the user
can see what was read.

### 6.3 Threads

- Each question asked from an entry point starts a **new thread** with its
  anchor. Follow-up questions continue the thread.
- The LLM panel lists the session's threads; each is labelled by its anchor
  and first question.
- Threads are held **in memory only** and are discarded on exit.
- A thread can be **exported as Markdown**: questions, answers, anchors, and
  a list of the tool calls made. Export is the only way a thread leaves
  memory.

### 6.4 Answers

- Rendered as **sanitised** Markdown (D-46, §9.2): no raw HTML; images are never
  loaded, but shown as links; external links open only after an explicit
  click and show their full URL. The same rules apply to Haddock markup
  from the project, such as `<<image-url>>`. This matters because a
  prompt-injected answer or a malicious Haddock comment could otherwise
  smuggle code out through an image URL.
- Code references in an answer (`Module.name`, `path:line`) become links into
  the tool, on the right side.
- Streaming output, which the user can stop.

### 6.5 Budget guards

(D-23)
- Per answer: **max tool calls** (default 25) and **max bytes read** through
  tools (default 2 MiB), both set in the config. When a limit is reached,
  every further tool call returns a fixed result: "budget exhausted: answer
  now with what you have". The answer is marked *truncated: limit reached*.
  With the API backends the loop also stops after one more model turn. With
  `claude-cli` a turn limit is passed to the CLI as a backstop (§6.8).
- Each thread shows its cumulative **input/output token usage**, as reported
  by the provider. Cost is not shown, since that would need price tables.

### 6.6 Stale threads

After a reload (§5.2), threads asked against an older snapshot stay readable
but are marked *stale*. Their links resolve against the current snapshot
when possible. A follow-up in a stale thread tells the LLM that the code may
have changed.

### 6.7 Providers and privacy

- There are three backends, selected in the config file (D-05, D-33):
  - `openai-compatible`: any OpenAI-compatible endpoint, including local
    servers such as Ollama, llama.cpp or vLLM.
  - `anthropic`: the Anthropic API with an API key.
  - `claude-cli`: the user's own installed `claude` CLI, signed in with the
    user's **own Claude subscription** (or key). This backend exists so that
    users without an API budget can use the tool (§6.8).
- The `claude-cli` endpoint is always treated as non-loopback.
- The first time a session would send code to a **non-loopback** endpoint
  (any host that does not resolve to `127.0.0.0/8` or `::1`), the tool asks
  for confirmation once and states the endpoint host. The confirmation can be
  pre-granted per endpoint in the user config.
- API keys are read from environment variables or the config file, and never
  from the project directory. haskell-reviewer **never reads, stores or
  forwards Claude subscription credentials**. With `claude-cli`,
  authentication is entirely the CLI's business.

### 6.8 The `claude-cli` backend

**Why it is built this way.** Anthropic's terms forbid third-party
applications from using subscription (OAuth) credentials directly. They do
explicitly allow a user to sign in to the **unmodified** Claude Code CLI with
their own subscription (see the Claude Code docs, *Legal and compliance →
Authentication and credential use*). haskell-reviewer therefore never makes
subscription API calls itself. It runs the user's installed `claude` binary
as a subprocess, in headless mode (`-p`, streaming JSON input and output).
This is a policy-sensitive integration: it is version-checked, and if
Anthropic's terms or the CLI's interface change, this backend may have to
change or go.

**Containment** (D-34). Defence in depth: layers 1–4 each independently
keep the LLM away from anything but our read-only tools; layers 5–6 add
hygiene and verification.
1. **No built-in tools**: the CLI is started with an empty built-in tool set
   (`--tools ""`), so it has no Read, Bash, Edit, WebFetch, WebSearch, etc.
   Restricted mode is also enabled where available.
2. **Only our tools**: the §6.2 tools are exposed through a local MCP server
   (stdio), passed with `--mcp-config` and `--strict-mcp-config`, so every
   other MCP server the user has configured is ignored. Headless mode
   refuses any tool needing a permission prompt, so our tools, and only
   ours, are explicitly allowed (`--allowedTools "mcp__hr__*"`). The MCP
   server is haskell-reviewer itself in a bridge mode. It enforces all of
   §6.2's containment rules and the budget guards (§6.5), whatever the CLI
   asks for.
3. **No project influence**: the CLI ignores project and local settings
   (and with them any hooks, permissions or MCP servers a reviewed repo
   might ship in `.claude/`). It also ignores user-level settings files and
   hooks, because hooks execute commands.
4. **No auto-loaded context**: the CLI runs in a fresh, empty temporary
   directory, not in ROOT. The reviewed project's `CLAUDE.md` and similar
   files are therefore never auto-loaded, and the project is reachable only
   through our MCP tools.
5. **No persistence**: sessions are not saved to disk
   (`--no-session-persistence`), consistent with D-12.
6. **Verification at start-up**: the first message of every CLI session
   reports its tool set. If that set is not exactly
   `{mcp__hr__list_files, mcp__hr__read_file, …}` (the §6.2 tools, names
   fixed per release), the session is killed, the backend is refused for
   this run, and the user is told why. A CLI version outside the tested
   range triggers a warning but still runs, as long as verification passes.

**System prompt.** Our built-in prompt (§6.2) **replaces** Claude Code's own
(`--system-prompt`), rather than being appended to it.

**Residual context outside our control** (D-47). Even so, the CLI may still
load the **user's own** global memory file (`~/.claude/CLAUDE.md`),
auto-memory, and any **organisation-managed** policy settings (including
managed hooks). Disabling these would also disable subscription sign-in.
None of it comes from the project under review, so project containment is
unaffected, but the initial context is not entirely ours. "Show context"
says so for `claude-cli` threads, listing what may have been added. This is
documented as a known limitation.

**Threads.** Each live thread is one long-lived CLI subprocess. At most
**4** threads are live at once (configurable). When the limit is exceeded,
the least recently used thread's process is closed and that thread becomes
read-only. Its "continue" action starts a new thread seeded with the old
transcript.

**Budget guards and usage** work as in §6.5. Tool-call and byte limits are
enforced by the MCP bridge. Token usage is taken from the CLI's result
events. Subscription rate-limit errors appear inline as ordinary backend
errors.

**Detection.** The configured binary path, or else `claude` on `PATH`
(looked up as in §11.4.1, never inside ROOT), is
checked at start-up. If it is missing or not signed in, the LLM is reported
as not configured (§8), with the reason.

---

## 7. Configuration

- One user config: `$XDG_CONFIG_HOME/haskell-reviewer/config.toml` (on macOS
  the same XDG path, defaulting to `~/.config`). It holds the LLM provider,
  endpoint, model, key source, the deny-list and UI defaults.
- UI choices made in the page and "remembered across sessions" (theme
  override, diff layout, "exported only", single-key shortcuts) are stored
  by the tool in `$XDG_STATE_HOME/haskell-reviewer/ui.json` (D-68). They
  take precedence over the config's UI defaults. The tool never rewrites
  `config.toml`.
- There is **no per-project configuration** of any kind (D-15): no file in
  the project, and no per-project section in the user config. The project
  under review cannot influence the tool, and there is only one place to
  look.
- CLI flags override config values for a single run. Project-specific needs
  (`--hie-dir`, `--ghc`, `--cpp-define`, …) are given as flags.
- Things that *are* inferred from the project are inputs, not configuration:
  the package structure, default extensions and the GHC version (§7.1). They
  can never touch LLM or security settings.

### 7.1 GHC versions: one parser, per-version HIE extractors

(D-35, superseding D-18) Supporting several GHC versions must not bloat the
main binary. The design rests on two facts:
- *Parsing* is largely backwards compatible. The newest GHC parser, given
  the project's language edition and extensions, accepts nearly all code
  written for older GHCs.
- *Reading `.hie` files* needs the GHC code of the exact major version that
  wrote them, and several GHC or `ghc-lib-parser` versions cannot be linked
  into one binary.

Therefore:
- **The main binary contains exactly one parser**: the newest supported
  GHC's. It is used for every project, whatever its GHC version. The GHC
  version the project targets is still determined, in this order: `--ghc
  X.Y` (CLI flags always win, §7); trusted HIE files (§3); `with-compiler` in
  `cabal.project`; otherwise the parser's own version. It is used for `__GLASGOW_HASKELL__` (§3.1) and for the HIE step
  below. Code the newest parser rejects (rare syntax removed in newer GHCs)
  falls back to text-only display (§3.1), and the banner says why.
- **Precise tier through companion extractors**: a small, separate
  executable for each GHC major series, `haskell-reviewer-hie-<X.Y>`, reads
  `.hie` files of that version. It converts them into a **neutral,
  versioned index format** that the main binary reads. The companions are
  *our* code, not project code, so running them does not break §9.1.
- Companions are optional and installed only for the versions the user
  needs. They are discovered next to the main binary or on `PATH`. The
  **three most recent GHC major series** are supported, and each companion
  is built from source with its GHC (`make install-hie`, §11.8).
- When `.hie` files are found but the matching companion is missing,
  those modules use the syntactic tier, and the header hint names the
  companion to install.
- The converted index is cached outside the project, under
  `$XDG_CACHE_HOME/haskell-reviewer/`, keyed by the content hash of each
  `.hie` file, so a restart does not convert everything again.
- The detected GHC version and the extractors in use are shown in the
  header tooltip and in `--version`.

---

## 8. Error handling and degraded modes

| Situation | Behaviour |
|-----------|-----------|
| ROOT has no `cabal.project` and no `*.cabal` file | Treated as a single implicit package; module names inferred from paths; banner shown. |
| Language for the implicit package and "Other files" | GHC2021, GHC's default with no flags (D-84). |
| ROOT has `stack.yaml` but no `cabal.project` | `stack.yaml` is ignored (D-66). The `*.cabal` files in ROOT are used, else the row above applies. The banner says Stack is not supported. Committing a `cabal.project` fixes it for later revisions. |
| A `.hie` file is part of a snapshot (tracked or committed) | Ignored (D-56); the header hint says why. |
| HIE companion crashes, times out or returns bad output | Syntactic tier for the affected modules; the reason is in the header hint. |
| Package has `package.yaml` but no `.cabal` (Hpack) | Not supported (D-41): that package is treated as plain directories with a banner explaining how to generate the `.cabal` file. |
| Generated module source (`.hsc`, `.x`, `.y`, `.chs`) | Plain text, "generated: not indexed" (§2.3). |
| `--diff` but not a git repo | Error, exit code 2. |
| A module fails to parse | Text-only fallback (§3.1). |
| HIE files missing or stale | Syntactic tier, shown in the header. |
| LLM not configured | LLM entry points are hidden; a single hint explains how to enable them. |
| LLM endpoint failing | Error shown inline in the conversation; the rest of the tool is unaffected. |
| `claude-cli` missing, not signed in, or failing tool-set verification (§6.8) | That backend is refused for this run, with the specific reason; no fallback to another backend without the user's choice. |
| HIE files found but no matching companion extractor (§7.1) | Syntactic tier for those modules; the header hint names the companion to install. |
| Newest parser rejects a module written for an older GHC | Text-only fallback with the reason in the banner (§3.1, §7.1). |

---

## 9. Security model

### 9.1 The code under review is untrusted

Reviewing a pull request means reading code the reviewer does not yet trust.
Therefore:
- **The tool never executes project code.** That includes `Setup.hs`, TH,
  custom preprocessors and build hooks.
- Only CPP and unlit (including the markdown-unlit style) run, in-process,
  as our own code. Project preprocessors never run.
- Project files cannot change the tool's configuration (§7).

### 9.2 The browser page is a security boundary

(D-46) The page holds the session token and renders untrusted content: the
project's code and Haddock, and LLM answers. So:
- a strict **Content-Security-Policy**: `default-src 'self'; img-src 'self'
  data:; connect-src 'self'; script-src 'self'; object-src 'none';
  base-uri 'none'; form-action 'none'; frame-ancestors 'none'`, with no
  inline scripts and no remote resources of any kind. The SharedWorker
  (§11.6.2) falls under `default-src 'self'`;
- `Referrer-Policy: no-referrer`, so the token in the URL never leaks;
- all untrusted text is rendered as text, and Markdown/Haddock through a
  sanitiser that allows no raw HTML (§6.4).

### 9.3 Getting the precise tier

Because a build runs arbitrary code (TH, custom `Setup.hs`), the tool **never
builds**, not even on request. The precise tier comes only from HIE files the
user has already produced, e.g. by building with `-fwrite-ide-info
-hiedir=…`. They are discovered in `dist-newstyle`, or given with
`--hie-dir DIR` (repeatable); Stack users pass `.stack-work` this way. HIE files that are
part of a snapshot are never used (§3, D-56). The docs and the
"precise tier unavailable" hint explain how to produce them.

---

## 10. Performance

Performance is a top principle (§1). The numbers below are **requirements**,
measured on a recent laptop with a warm OS file cache, and backed by
benchmarks that are part of the project.

### 10.1 Start-up targets

(D-36)
| Milestone | Target |
|-----------|--------|
| URL printed on stderr and server accepting connections | < 300 ms |
| Typical project (≤ 300 modules): UI usable (module tree, search, any module page) | < 1 s |
| Large project (2,000 modules / 500k lines): UI usable | < 10 s (D-24) |
| Navigation within an indexed module | < 100 ms |
| Opening a not-yet-indexed module on demand | < 300 ms |
| Large project, precise tier, **cold** HIE cache (first run, all `.hie` converted) | Syntactic UI within the 10 s target; precise tier fills in progressively, with no fixed bound in v1 (to be set from benchmarks) |

Diff-mode targets cover **both sides**. Except where marked *cold*,
targets assume a warm HIE-conversion cache (§7.1).

The server starts and prints the URL **before** indexing begins. The page
appears at once and fills in progressively.

### 10.2 Progressive indexing

- Indexing is **progressive**. Modules are parsed in the background, and a
  module the user requests jumps the queue. The header shows indexing
  progress.
- Features that need the whole index say so while it is incomplete:
  declaration-level search, find-references, "Affected (not modified)" and
  the LLM `find_references` tool. They mark their results *partial*, and do
  not block.
- In diff mode the changed modules on both sides are indexed first, so that
  the expanded diff is ready as early as possible.
- The memory budget is to be set from the first benchmarks. As a
  provisional target, it should stay under 2 GiB for the large reference
  project in diff mode, with both sides indexed.

### 10.3 Benchmarks may change the spec

(D-36) The spec is **expected** to change during implementation in
response to benchmarks. When a feature cannot meet its targets:
1. the measurement is recorded;
2. the options are considered: optimise, degrade (e.g. compute lazily, or
   only on request), or drop;
3. the outcome is decided **case by case** and logged in DECISIONS.md, and
   this spec is updated.

There is deliberately no fixed list of what to cut first. The features
most likely to be reconsidered are the costly whole-project analyses:
"Affected (not modified)", rename detection, syntactic-tier
find-references, and inferred types on hover.

---

## 11. Architecture and tech stack

This section fixes the high-level architecture and the tech stack
(D-48 to D-76). It gives no low-level details such as module layouts,
exact types or endpoint lists. Those are settled in code. Like the rest of
the spec, it can change from benchmarks (§10.3).

### 11.1 Overview

```
 browser tabs ──┐
                ├─ SharedWorker ──── SSE (one per browser) ─────┐
                └─ fetch: JSON / CBOR, POST for actions ────────┤
                                                                ▼
                                           haskell-reviewer (main process)
     git cat-file --batch + plumbing ◄──────────┤
     haskell-reviewer-hie (short-lived) ◄───────┤  CBOR on stdout
     LLM HTTP (anthropic, openai-compatible) ◄──┤
     claude CLI ──stdio MCP──► haskell-reviewer mcp-bridge ──unix socket──┘
```

- **One main process** does all the work: it reads snapshots, indexes them,
  computes diffs, serves the UI and runs LLM threads. There is no daemon
  (§2.1).
- **Helper processes** are all short-lived or tied to the session: `git`
  (read-only, §11.4.1), HIE companions (§11.4.6), and for `claude-cli` the
  `claude` binary plus our own MCP bridge (§11.6.3).
- **The browser** gets structured data and renders everything itself
  (§11.7). The server never sends HTML apart from the static app shell.

### 11.2 Repository and packages

(D-58) One monorepo:

| Path | What | Builds with |
|------|------|-------------|
| `haskell-reviewer/` | Main package: library (all logic) and executable (thin `main`). | GHC 9.10, 9.12, 9.14 (D-64) |
| `haskell-reviewer-index/` | Neutral, versioned index format (§7.1): types plus CBOR codecs. Small, few dependencies. | The same 3 GHCs |
| `haskell-reviewer-hie/` | The HIE companion executable. It links the `ghc` library of the GHC that builds it. | Each supported GHC, one build each |
| `web/` | Frontend (SolidJS + TypeScript), one npm package. | Node (pinned), only to change the frontend |
| `web/dist/` | The **built** frontend bundle, committed (D-65). | Embedded in the main binary |

The main library splits into layers. Lower layers never import upper ones:

1. **Core** (pure): project model, CPP evaluator, extraction of our own
   module model from the GHC AST, resolution, line diff, semantic diff,
   similarity, search scoring, rich text, containment rules.
2. **Effects** (handles, D-63): git access, file system, companions, LLM
   transports, clock. Each is a record of functions, so tests can plug in
   fakes (a fake git, a scripted LLM).
3. **Session**: snapshot generations, the indexing scheduler, LLM threads,
   preferences (§11.5).
4. **Server**: HTTP routing, security headers, wire encoding, SSE (§11.6).
5. **App**: CLI parsing and wiring. Imports all layers; none imports it.

### 11.3 Tech stack

| Concern | Choice | Notes |
|---------|--------|-------|
| Backend language | Haskell (GHC 9.10–9.14) | Direct access to the GHC parser and HIE files. |
| Program style | Plain `IO` plus the handle pattern; pure core | No effect library (D-63). |
| Haskell parser | `ghc-lib-parser`, newest supported GHC only (9.14 today) | D-35. |
| HIE reading | `ghc` library, inside the companions only | D-35, D-57. |
| `.cabal` files | `Cabal-syntax` | The reference parser: conditionals, common stanzas (D-67). |
| `cabal.project` | Our own small parser | Only the fields we need (§11.4.2). |
| CPP | Our own evaluator | Keeps positions and excluded regions (D-55). |
| Markdown | `commonmark` (+ `commonmark-extensions`) | Parsed into our rich-text AST (D-59). |
| Haddock markup | `haddock-library` | Same AST. |
| HTTP server | `warp` + `wai`, small hand-written router | D-49. |
| Compression | gzip middleware (`wai-extra`) | For responses above a size threshold. |
| Static assets | `file-embed` | The bundle is compiled into the binary. |
| Concurrency | `stm`, `async` | §11.5. |
| Wire formats | `aeson` (JSON), `cborg` (CBOR) | D-51. |
| Index and cache format | `cborg` / `serialise`, versioned | D-57. |
| Hashing | `crypton` (+ `ram`) | Git object ids (SHA-1 or SHA-256) for working-tree files, cache keys. Also the session token. |
| CLI parsing | `optparse-applicative` | |
| Config | `toml-parser` | `config.toml` (§7). |
| Anthropic API | `claude` package (Hackage) | Typed streaming, including tool-input deltas (D-62). |
| OpenAI-compatible API | `openai` package: request types and the raw event stream; our own decoder for streamed tool-call deltas | Its typed `ToolCall` lacks the delta `index` (D-62). |
| MCP (bridge) | Our own minimal stdio JSON-RPC server | Only `initialize`, `tools/list`, `tools/call`, `ping` (D-62). |
| Frontend | TypeScript (strict), SolidJS, Solid Router | D-48. |
| Frontend build | Vite; ESLint + Prettier; Vitest | |
| Styling | Plain CSS with custom properties (themes, design tokens); system font stacks; no CSS framework, no bundled fonts | D-48. |
| CBOR in the browser | `cbor-x`, **no-eval build** | CSP forbids `eval` (D-51). |
| Virtual scrolling | A headless virtualiser (TanStack Virtual) | All source and diff views (D-70). |
| Haskell tests | `tasty`, `tasty-hunit`, `hedgehog`, `tasty-golden`, `temporary` | §11.9. |
| Browser tests | Playwright + axe-core | §11.9. |
| CI | GitHub Actions | §11.9. |
| License | BSD-3-Clause | D-73. |

Third-party API packages sit behind our own provider interface (§11.6.3),
so replacing one touches only its adapter.

### 11.4 Backend pipeline

From the revisions to what the UI shows:

```
revspec ─► git layer ─► snapshot (file list + blobs, per side)
                           │
                           ├─► project model (.cabal, cabal.project)
                           │
                           ├─► per module: CPP ─► lex + parse ─► module model   (shared across sides)
                           │                                        │
                           │                          resolution (per side) ◄── HIE index (companions)
                           │
                           └─► line diff + renames ─► semantic diff (§4.6)
```

#### 11.4.1 Git layer

(D-53) Git is used **only as a read-only object store**.
- One long-lived `git cat-file --batch` per session reads blobs and trees.
  A few plumbing commands do the rest: `rev-parse --verify`,
  `merge-base`, `ls-tree`, and `ls-files` (for the tracked and
  untracked-but-not-ignored files of a working-tree side). There is no
  `git diff` and no `git status`. Nothing writes to `.git`: no index
  refresh, no temporary index, no new objects.
- Hardening, for every call:
  - a minimal environment that keeps `HOME` and `XDG_CONFIG_HOME` (so the
    user's `safe.directory` still applies), sets `LC_ALL=C` and a `PATH`
    with only the absolute entries outside ROOT, and drops everything
    else, every other `GIT_*` included;
  - `GIT_OPTIONAL_LOCKS=0`: no index refresh;
  - `GIT_NO_LAZY_FETCH=1`: a partial clone never goes to the network;
  - `GIT_NO_REPLACE_OBJECTS=1`: `refs/replace` can't swap content;
  - `GIT_TERMINAL_PROMPT=0`: never prompt;
  - `--no-pager`, `-c core.fsmonitor=false`, `-c core.hooksPath=/dev/null`:
    repo config can't run a command;
  - `-c core.untrackedCache=false`: no cache to write;
  - `-c safe.bareRepository=explicit`: a bare repo planted in the tree
    is not used;
  - a time limit, 30 s at start-up and 120 s for `ls-files` (a FIFO in
    the tree can block git): then it is killed;
  - `--end-of-options` before any user-given revision;
  - no inherited file descriptors, and a soft fd limit of at most 4096
    (D-86).

  Git's `safe.directory` refusal is reported as is. The minimum git version
  is 2.44 (for `GIT_NO_LAZY_FETCH`), checked at start-up (exit code 2).
- **Helper lookup**: `git`, `claude` and the companions are looked up only
  in absolute `PATH` entries, and a binary inside ROOT is never run. A
  change under review can't add a binary that we then start (§9.1).
- **Working-tree side** (D-75): git's clean filters (CRLF, `ident`,
  encodings, LFS) mean a file on disk can differ from its blob and still be
  unchanged. So git says which tracked files are clean: `ls-files -s`
  gives their blob ids, and `diff-files --name-only` lists the dirty ones.
  Both are read-only with the settings above. Clean files are read from
  git objects. Only dirty and untracked files are read from disk; their ids
  are computed in process, with the EOL rules of `.gitattributes` applied.
  A dirty file under any other filter (e.g. LFS) is shown as its on-disk
  text, with a "filtered file" note.
- Submodules are not entered. A submodule shows as one entry with its
  commit id. Git LFS files show as their pointer text.

#### 11.4.2 Project model

(D-66, D-67)
- `.cabal` files are parsed with `Cabal-syntax`. Conditionals are resolved
  with the host platform and the flag values below, as in §3.1.
  `impl(ghc …)` uses the detected GHC version (§7.1).
- A component with no `default-language` uses Haskell98, as Cabal does
  (D-84).
- `cabal.project` is parsed by our own parser, which reads only
  `packages:`, `with-compiler:`, `flags:` (global and per `package`
  stanza), and `import:` of files **inside the snapshot**. Remote imports
  are ignored, with a warning. `cabal.project.local` and
  `cabal.project.freeze` are ignored, because they are per-user or not
  relevant.
- **Flag values**: `.cabal` default < `cabal.project` (`flags:`) <
  `--flag`. The header tooltip shows where each value came from. Flags set
  only through `constraints:` are not read, and automatic flags
  (`manual: False`) keep their default, since there is no solver.
- Stack is not supported. `stack.yaml` is never read (§2, §8).

#### 11.4.3 CPP

(D-55) Our own evaluator implements what §3.1 needs: `#if`/`#ifdef`/`#elif`
expressions, `#define` (object- and function-like), `#undef`, `#include`
inside the snapshot, and `#error` / `#warning` shown as notes. Its output is
the text GHC sees, plus a **position map** back to the original file and
the list of excluded regions. These feed the greyed-out display and the
CPP token class (§5.6). Unknown or unsupported constructs produce a warning
on the module, never a crash.

#### 11.4.4 Parsing and the module model

(D-61) Each module is lexed and parsed once with `ghc-lib-parser`, and at
once converted into **our own compact module model**. The GHC AST is then
dropped, since keeping ASTs for two sides of a large project would break
the memory budget (§10.2). The model holds:
- header, exports, imports, extensions in effect;
- declarations with namespace, name, extent (a set of ranges, §4.3), head
  tokens, Haddock (as rich text, §11.4.9), and flags (exported, orphan, …);
- the token stream (class, span, occurrence id), used for highlighting,
  links, hover and the similarity metric;
- an occurrence table: for each identifier span, its binder or its target
  candidates, resolved in the next step.

Spans: lines and columns are 1-based, columns count code points (a tab is
one column), and the end is exclusive. The HIE index uses the same rule
(D-87).

Anything the model lacks is recomputed by parsing again on demand.

**Sharing across sides.** The parse result is keyed by *(blob id,
effective parse settings)*. The settings are the language, the extensions,
a hash of the CPP environment, and the hashes of included files. Equal keys
share one result, so in diff mode an unchanged module is parsed and stored
once. The source text is also stored once per blob. Resolution depends on
the imports of each side, so it stays per side, and it is cheap compared
with parsing.

#### 11.4.5 Resolution

Per side, the occurrence table is resolved against the side's module
graph: import-directed at the syntactic tier (§3), and replaced by the HIE
index at the precise tier. The results are a definition target for each
occurrence, and the per-side tables built from them: name → declarations,
and a reverse-reference table for find-references and "Affected". The
reverse table is built in the background, and is marked *partial* until it
is complete (§10.2).

#### 11.4.6 HIE companions

(D-56, D-57)
- **Discovery** looks under `dist-newstyle/` and every `--hie-dir`.
  Companions are found as `haskell-reviewer-hie*` next to the main binary
  or on `PATH` (§11.4.1). Each is asked which GHC version it handles
  (`--ghc-version`), and the answer is cached per binary (path, size,
  mtime). The main binary reads the `.hie` header (GHC version) itself to
  pick the companion.
- **Trust rule**: as in §3. The check uses real paths: a `.hie` file whose
  real path, or any parent directory, is tracked or a symlink in the
  snapshot is rejected. The rule applies to `--hie-dir` too.
- **Protocol**: the main process starts a companion in batch mode with a
  list of `.hie` paths on stdin. The companion writes one CBOR record per
  file to stdout, in the neutral format of `haskell-reviewer-index`, with a
  magic header and a format version.
- **Limits**: each companion runs with an RTS heap cap (it is built with
  `-rtsopts`) and a per-file timeout. If it crashes or times out, the file
  at fault uses the syntactic tier, and a new companion carries on with the
  rest. Bad output is treated the same way. The header says why.
- **Cache**: `$XDG_CACHE_HOME/haskell-reviewer/hie/<format major>/`, one
  file per `.hie`, keyed by its content hash and the companion's version. Files are written atomically
  (temporary file, then rename), and entries unused for 30 days are pruned
  at start-up.

#### 11.4.7 Line diff and renames

(D-54) All diffs are computed **in process**, from the two snapshots.
Commit-to-commit and commit-to-working-tree therefore take the same code
path.
- **File pairing**: files with equal paths; then renames, first by exact
  blob match, then by content similarity ≥ 50 % among the remaining
  added/removed files, the way git does by default. Copies are not
  detected (§4.5). Like git's `diff.renameLimit`, the similarity step is
  capped (1,000 files per side by default); above that, only exact
  renames are found, and a notice says so.
- **Hunks**: histogram diff (as `git diff --histogram`), with a Myers
  fallback for inputs where histogram degrades. Context is unlimited,
  because both full texts are at hand, so "expand context" costs nothing.
- **Ignore whitespace** re-runs the diff on normalised lines.

#### 11.4.8 Semantic diff

As in §4.6: declarations are matched by *(module, namespace, name)*, then
by *(namespace, name)* across modules for moves, then by the similarity
pass (§4.6.1) for renames. Hunks are mapped to declaration extents through
line ranges on each side. Classification compares the head-token and body
token streams of the module model, so it needs no new parse. The changed
modules on both sides are indexed first (§10.2), so this view is ready
early.

#### 11.4.9 Rich text

(D-59) LLM Markdown (`commonmark`) and project Haddock (`haddock-library`)
are parsed **on the server** into one small rich-text AST. The AST cannot
express raw HTML, so sanitising (§6.4, §9.2) is structural rather than a
filter. Identifier and `path:line` references are resolved on the server
into links. While an answer streams, the server re-parses at most every
100 ms, and sends only the blocks that changed since the last update.

#### 11.4.10 Search

(D-69) Global search (§4.7) runs on the server, against a per-side name
table that grows while indexing. It uses an fzf-like scorer and returns a
capped list. The client debounces keystrokes. Results that come from an
incomplete index are labelled *partial*.

### 11.5 State and concurrency

(D-60)
- **Snapshot generations.** Each side has a current *generation*: an
  immutable file list, plus one slot per module (`pending`, `parsing`,
  `ready`, `failed`). Slots are held in STM. A request reads one
  generation from start to end, so it never sees a mix of two.
- **Scheduler.** A priority queue feeds a pool of parse workers (about one
  per core, `-threaded`). The priorities are: modules a request is waiting
  for, then changed modules in diff mode, then the rest. A request for a
  module that isn't ready raises its priority and waits for it.
- **Reload** (§5.2) builds a new generation and swaps it in atomically.
  Unchanged blobs hit the shared parse results (§11.4.4), so a reload
  re-parses only what changed.
- **GC.** Big indexes may cause long GC pauses. Benchmarks pick the fix:
  compact regions or the non-moving GC (Q-GC).
- **Session state** also lives in STM: LLM threads, budgets (§6.5), SSE
  clients and preferences.
- **Files the tool writes** (§1): the HIE cache (`$XDG_CACHE_HOME`), UI
  preferences (`$XDG_STATE_HOME`, default `~/.local/state`, also on macOS;
  D-68), a private temporary directory for the `claude-cli` working
  directory and the bridge socket (removed on exit), and the
  `--trace-file` if given. Nothing else, and nothing inside ROOT unless
  the user points `--trace-file` there.

### 11.6 Server and protocols

#### 11.6.1 HTTP server

(D-49) `warp` on `127.0.0.1`. In front of every route, middleware does the
following:
- checks the `Host` header and the session token (§2), comparing the token
  in constant time;
- sets the security headers: the CSP and `Referrer-Policy` of §9.2,
  `X-Content-Type-Options: nosniff`, and `Cache-Control: no-store` for API
  responses. Static assets also live under the token path, and their
  hashed names let the browser cache them as immutable;
- gzips large responses, except the event stream (from M1).

The listening socket sets `TCP_MAXSEG` to 1460, so a big response doesn't
stall on loopback (D-86).

The router is a plain pattern match on path segments. The API is small, so
a framework would cost more than it saves.

#### 11.6.2 Browser protocol

- **Reads** are `GET` requests. Small or structural payloads (module tree,
  search results, threads, prefs) are JSON. **Token-heavy** payloads (docs
  pages, full source, inline source, diff, expanded diff) are **CBOR**
  (D-51). Every response carries its snapshot generation id, and the client
  caches by *(generation, resource)*.
- **Actions** (ask, stop, reload, set preference, …) are `POST` requests
  with JSON bodies.
- **Push** (D-52): **one SSE connection per browser**, held by a
  **SharedWorker** that passes events on to all tabs.
  - Why: browsers allow about 6 HTTP/1.1 connections per origin, and
    plain HTTP gives no HTTP/2. One stream per tab would hang the UI after
    a few tabs.
  - URL: `GET /<token>/api/events`. On open the server sends a `hello`
    event (the only event in M0).
  - Events: indexing progress, generation changes, LLM stream updates,
    notices.
  - Fallback: without SharedWorker, each tab opens its own stream, and a
    notice warns about the limit.
- **Liveness**: a browser is connected while its SSE stream is open. The
  server writes a keep-alive comment every 15 s, so a closed tab is noticed
  within about 30 s, and the idle timer of §2.1 starts when no stream is open.
  The stream route has no Warp timeout and is never gzipped.
- **Reconnect**: events carry ids. After a reconnect the worker refetches
  the state it shows (index progress, open LLM threads), so a lost event
  costs nothing.
- **Contract** (D-50): the types are hand-written on both sides. The
  Haskell test suite writes a **golden fixture** for every endpoint (CBOR
  fixtures have a JSON dump next to them for readable diffs). The TypeScript
  tests feed the fixtures to the **runtime decoders**, which production
  also uses, so a bad payload fails loudly at the boundary.

#### 11.6.3 LLM engine

(D-62)
- **Provider interface**: one neutral conversation model (messages, tool
  calls, tool results, usage, stop reasons) and one streaming interface.
  The adapters are `anthropic` (the `claude` package), `openai-compatible`
  (the `openai` package plus our delta decoder) and `claude-cli`. The
  OpenAI-compatible adapter uses the Chat Completions API, since that is
  what local servers implement (Q-OPENAI-COMPAT).
- **Tool loop**: for the two API backends, our loop runs model turns,
  executes tool calls and enforces the budgets (§6.5). The `claude-cli`
  backend runs its own loop and reaches the same tools through the bridge.
- **Single choke point**: every tool, the manifest and the anchor read the
  project through one **LLM view** of a snapshot. That view applies the
  visible set and the deny-list (§6.2). Nothing else can produce bytes for
  the model, so a new tool cannot forget the rules.
- **claude-cli**: the CLI runs as a subprocess with stream-JSON input and
  output, with the flags of §6.8. Its MCP config starts
  `haskell-reviewer mcp-bridge`, a hidden subcommand of the same binary.
  The bridge speaks minimal MCP over stdio, and relays each call over a
  Unix socket in the private temp directory (mode 0700), authenticated with
  a per-session secret. The secret is passed in a mode-0600 file in that
  directory, never on a command line (other users can read argv). The
  directory path is kept short for the macOS socket path limit. All checks and budgets are enforced in the main
  process, never in the bridge.
- **Cancellation**: "stop" cancels the thread's `async`. That closes the
  HTTP stream, or sends an interrupt to the CLI.

### 11.7 Frontend

(D-48)
- **SolidJS** single-page app. No virtual DOM, so big token views update
  fast. Routing follows §4.1's URL
  shapes.
- **Rendering**: everything is built from the structured data: token
  streams, declaration models and the rich-text AST. `innerHTML` and its
  relatives are banned by a lint rule, and checked in CI.
- **Virtualisation** (D-70): every source and diff view is virtualised,
  so only the visible lines are in the DOM. Rows carry ARIA row indexes,
  so assistive tech knows where it is in the file. Our own **find in file**
  (`f`, plus a toolbar button) searches the whole file on the client, and
  announces the match count in a live region. The browser's Ctrl-F is left
  alone, and the toolbar shows a permanent hint that it only sees the
  visible lines (§4.4, §4.5).
- **Styling**: plain CSS with custom properties. Themes and the WCAG
  contrast pairs (§5.4) are defined once, as design tokens. System font
  stacks only.
- **Preferences** (theme, layout, "exported only", shortcuts) are read from
  and written to the server (D-68). The port changes on every run, so
  `localStorage` would forget them.
- **Bundle**: the CI checks that the bundle contains no `eval` or `new
  Function`, since the CSP blocks them. This is why the `cbor-x` no-eval
  build is used.

### 11.8 Build, install and distribution

(D-64, D-65)
- **Source only in v1.** There is no Hackage release and there are no
  prebuilt binaries. Installation:
  - `make install` builds and installs the main binary. It needs only GHC
    and `cabal`, since the frontend bundle is committed.
  - `make install-hie GHC=ghc-9.10` builds the companion with that GHC and
    installs it as `haskell-reviewer-hie-9.10`. `--version` and the header
    hint (§7.1) print the exact command for a missing companion.
  - `make web` rebuilds the frontend bundle. It needs Node, pinned through
    the lockfile and an engines field.
- The main binary builds with GHC 9.10, 9.12 and 9.14, so users can use the
  GHC they already have. It is linked with `-threaded`.
- Platforms: Linux and macOS (D-17).

### 11.9 Quality

(D-71, D-72)
- **Unit and property tests** (`tasty`, `hedgehog`) on the pure core: CPP
  evaluator, line diff, similarity, classification, path containment,
  rich-text sanitising.
- **Golden tests on fixture repos.** Scripts build small git repos with
  fixed authors and dates. The tests check the index, the change list, the
  manifest and the API fixtures (§11.6.2). Cases include CPP, TH, renames,
  moves, instances and deriving, a monorepo ROOT, Hpack and Stack-only
  repos, CRLF and LFS files, and parse failures.
- **Security regression suite**: path escapes and symlinks; deny-list
  leaks through every LLM tool, the manifest and the anchor; token and
  `Host` checks; security headers; malicious Haddock and Markdown; `.hie`
  files tracked in git or behind a tracked symlink; a helper binary
  planted in ROOT; companion crash and timeout; git config that tries
  to run commands.
- **Browser end-to-end tests** (Playwright) with axe-core checks in both
  themes: keyboard flows, URL routing, the SharedWorker connection, find in
  file.
- **CI** (GitHub Actions): Linux and macOS × GHC 9.10/9.12/9.14 for the
  Haskell packages; one web job (type check, lint, Vitest, bundle
  reproducibility, no-eval check); one end-to-end job, on Linux only
  (Chromium, Firefox, WebKit).
- **Benchmarks** (`make bench`) run on three pinned reference projects: a
  small one, a typical one and a 2,000-module one. They measure the §10
  targets and memory. They are run by hand and before each release, not in
  CI, because shared runners are too noisy. CI only does a smoke run on the
  small project: it checks that the harness works, with no timing checks
  (D-76). Results feed the D-36 decisions.
- **Diagnostics**: `--verbose` and `--debug` raise the stderr log level.
  `--trace-file PATH` writes timing spans (indexing phases, request
  latency) as JSON lines. Project code is never logged at the default level.
  There is no telemetry of any kind.

---

## 12. Rejected features (deliberately out of scope)

- Review comments, approvals, or posting to GitHub/GitLab.
- Browsing dependency sources in-app.
- Editing, refactoring, formatting or running code.
- Hoogle-style search by type (D-14).
- Per-project configuration (D-15).
- Windows (v1) (D-17).
- "Reviewed" markers (D-04).
- Building the project (D-07).
- File watching / live re-indexing (D-08).
- Persisting LLM conversations (D-12).
- Transitive impact analysis (D-09: one level only).
- Semantic syntax colouring and client-side highlighting libraries (D-31).
- Several parsers in one binary; building `.hie` files (D-35, D-07).
- Reading or handling Claude subscription credentials (D-33).
- Replaceable system prompt; heuristic prompt-injection scanning (D-37, D-39).
- Hpack (`package.yaml`) support (D-41).
- Remote images or raw HTML in rendered content (D-46).
- Stack support; reading `stack.yaml` (D-66).
- Hackage releases and prebuilt binaries in v1; install is from source
  (D-65).
- Detection of copied files in the diff (D-74).
- Bundled fonts and CSS frameworks (D-48).
