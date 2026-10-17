# haskell-reviewer — Specification

Status: **v1.0 — approved**. The rationale for every decision is in
[DECISIONS.md](DECISIONS.md) (D-01 to D-47). Items to verify during
implementation, none of which block this spec, are in
[OPEN-QUESTIONS.md](OPEN-QUESTIONS.md). Per §10.3 the spec may be revised
from benchmarks; every revision is logged in DECISIONS.md.

This document specifies *behaviour*. Implementation is discussed only at the
level needed to show feasibility (§11).

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
  directory containing `cabal.project`, `stack.yaml`, or a single `*.cabal`
  file. When both `cabal.project` and `stack.yaml` exist, `cabal.project`
  is used unless `--stack` is given. Any other directory is accepted in
  degraded form (§8). In diff mode
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
  The connection is tracked with a heartbeat from the page every ≤ 30 s.
  The server tolerates gaps of several minutes, because browsers throttle
  timers in background tabs. A tab that is reopened within the idle timeout
  reconnects to the same session.
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
  (`packages:` globs) or `stack.yaml`, or else the single `.cabal` file.
  **Hpack is not supported** (D-41). A package that has `package.yaml`
  but no `.cabal` file on a side is treated as a directory without a
  project (§8), and the banner says so. Hpack users can commit the
  generated `.cabal` file, or generate it before reviewing.
- **Components**: library (including sub-libraries), executables, test
  suites, benchmarks. Each has its source dirs, default extensions and
  language.
- **Modules**: the `.hs`, `.lhs` and `.hsig` files that belong to a component.
  Files that belong to no component are still indexed and shown under
  "Other files".
- **Generated modules**: modules whose source is a preprocessor input
  (`.hsc`, Alex `.x`, Happy `.y`, `.chs`, …) cannot be produced without
  running tools (§9.1). They are shown as plain text under their component,
  with a "generated: not indexed" banner, and contribute no declarations.
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
     Cabal/Stack package index on disk (read-only, no network);
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
| **Precise** | `.hie` files from a prior build whose recorded sources match this side (see rules) | Exact resolution of every identifier, types of identifiers (including inferred types of bindings without signatures) and of some expressions, instance resolution, references. |

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
  - Cabal flags take their **default** values.
  - `MIN_VERSION_pkg(…)` evaluates to **true**. `__GLASGOW_HASKELL__` and
    `MIN_VERSION_GLASGOW_HASKELL(…)` are both derived from the **detected
    project GHC version** (§7.1), falling back to the parser's version, so
    the two can never disagree.
  - Cabal's generated macros (`VERSION_pkg`, `CURRENT_PACKAGE_VERSION`,
    `CURRENT_COMPONENT_ID`) are synthesised from the `.cabal` file on that
    side. As in `cpp`, undefined macros evaluate to 0 in `#if`.
  - `#include` is resolved only **inside the side's snapshot**, using the
    component's `include-dirs`. Anything else (system headers, generated
    headers) is treated as empty, and a warning is shown.
  - `--cpp-define NAME[=VAL]` (repeatable) and `--flag [PKG:][-]NAME`
    override any of these. A bare `NAME` applies to every package that
    declares the flag.
  - When a module is at the precise tier, the CPP branches the HIE build
    actually took win over these assumptions, and the UI says so.
  - Excluded regions stay **visible, greyed out**, in the source and diff
    views. They contribute no declarations. The header tooltip lists the
    assumptions in effect.
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
  renamed) and a filter "changed only".
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
constructor `Foo`. Instances use `inst:<Class>:<Head>`, with the head
printed as in §4.6 and percent-encoded. The `<decl-id>` used by `/changes`
is the same `<namespace>:<name>` qualified by its module. An
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
- Renames and copies, as detected by git, are shown as such.
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
  line numbers or gutter markers. A plain text selection in the source and
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

**Detection.** The configured binary path, or else `claude` on `PATH`, is
checked at start-up. If it is missing or not signed in, the LLM is reported
as not configured (§8), with the reason.

---

## 7. Configuration

- One user config: `$XDG_CONFIG_HOME/haskell-reviewer/config.toml` (on macOS
  the same XDG path, defaulting to `~/.config`). It holds the LLM provider,
  endpoint, model, key source, the deny-list and UI defaults.
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
  X.Y` (CLI flags always win, §7); HIE files found; `with-compiler` in
  `cabal.project`; the Stack resolver; otherwise the parser's own version. It is used for `__GLASGOW_HASKELL__` (§3.1) and for the HIE step
  below. Code the newest parser rejects (rare syntax removed in newer GHCs)
  falls back to text-only display (§3.1), and the banner says why.
- **Precise tier through companion extractors**: a small, separate
  executable for each GHC major series, `haskell-reviewer-hie-<X.Y>`, reads
  `.hie` files of that version. It converts them into a **neutral,
  versioned index format** that the main binary reads. The companions are
  *our* code, not project code, so running them does not break §9.1.
- Companions are optional and installed only for the versions the user
  needs. They are discovered next to the main binary or on `PATH`. Releases
  ship companions for the **three most recent GHC major series**.
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
| ROOT has no cabal/stack project | Treated as a single implicit package; module names inferred from paths; banner shown. |
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
- CPP is the only preprocessor that runs, and it runs in-process.
- Project files cannot change the tool's configuration (§7).

### 9.2 The browser page is a security boundary

(D-46) The page holds the session token and renders untrusted content: the
project's code and Haddock, and LLM answers. So:
- a strict **Content-Security-Policy**: `default-src 'self'; img-src 'self'
  data:; connect-src 'self'; script-src 'self'`, with no inline scripts and no
  remote resources of any kind;
- `Referrer-Policy: no-referrer`, so the token in the URL never leaks;
- all untrusted text is rendered as text, and Markdown/Haddock through a
  sanitiser that allows no raw HTML (§6.4).

### 9.3 Getting the precise tier

Because a build runs arbitrary code (TH, custom `Setup.hs`), the tool **never
builds**, not even on request. The precise tier comes only from HIE files the
user has already produced, e.g. by building with `-fwrite-ide-info
-hiedir=…`. They are discovered in the usual `dist-newstyle` / `.stack-work`
locations, or given with `--hie-dir DIR` (repeatable). The docs and the
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

## 11. Implementation sketch (high level, non-normative)

- **Language**: Haskell. This gives direct access to the GHC parser
  (`ghc-lib-parser`, newest version only) and, in the companions, to the
  `ghc` library for reading HIE files (§7.1).
- **Backend**: one process. It indexes each side into an in-memory model
  (packages → modules → declarations, with an identifier table), computes the
  semantic diff, and serves a JSON API and static assets.
- **Frontend**: a single-page app bundled into the binary. Rendering is done
  on the client from structured data (declarations, tokens with link
  targets), not from server-rendered HTML.
- **Semantic diff**: declarations are matched as described in §4.6, using
  *(module, namespace, name)* keys, then *(namespace, name)* across modules for moves,
  and then a similarity pass over the remaining removed/added pairs for
  renames. Hunks are mapped to declaration extents through line ranges on each side.
- **Distribution**: one main binary, plus optional per-GHC HIE companions,
  for Linux (x86_64, aarch64) and macOS (aarch64, x86_64) (D-17). Windows
  is not supported in v1.
- **LLM bridge**: the same main binary, run in an MCP-bridge mode, serves
  the §6.2 tools to the `claude-cli` backend. It connects back to the main
  process over a local socket with a per-session secret.

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
