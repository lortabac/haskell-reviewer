# haskell-reviewer — Roadmap

This is the plan to get from [SPEC.md](SPEC.md) v1.1 to a v1.0 release.

The work is cut into milestones. Each milestone ships a program that runs
end to end and that people can try. After each one we collect feedback and
adjust the rest of the plan, or the spec, if needed.

Sizes are relative (S < M < L). There are no dates.

---

## Rules for every milestone

- **It runs.** Each milestone is a binary, installed with `make install`
  (D-65), that does something useful on a real project. No half-wired
  features behind flags.
- **Performance from day one.** The benchmark harness (`make bench`) exists
  from M0. It is run by hand and before each release, not in CI (D-71). CI
  only does a smoke run (D-76). From M1 on, every milestone re-checks the
  §10.1 targets that apply with `make bench`, on the typical *and* the
  large reference project. A miss follows §10.3: record it, pick optimise /
  degrade / drop, log it in DECISIONS.md, update SPEC.md.
- **Security from day one.** The page shows untrusted code from M0, so the
  token, the `Host` check and the security headers (CSP, `no-referrer`,
  `nosniff`; §2, §9.2, §11.6.1) come first, with CI checks for no `eval`
  and no `innerHTML` (§11.7). Sanitised rendering comes with the first
  Haddock (M1): a rich-text AST that can't express raw HTML (D-59).
- **Accessibility baseline from M1.** Semantic HTML, visible focus,
  contrast, and axe-core checks in both themes in the Playwright tests, in
  CI (D-71). The full WCAG 2.1 AA audit is in M9.
- **Tests** (§11.9, D-71).
  - Unit and property tests (`tasty`, `hedgehog`) on the pure core,
    including rules that must be deterministic (classification, rename
    pairing).
  - Golden tests (`tasty-golden`): fixture modules → declarations / tokens.
  - Fixture git repos, built by scripts with fixed authors and dates.
  - API contract (D-50): a golden fixture per endpoint, checked by the TS
    runtime decoders.
  - Playwright end-to-end tests per view, with axe-core.
  - Security regression suite: token, `Host`, headers, sanitiser, path
    escapes, deny-list, git config, planted helpers; later forged `.hie`.
- **Feedback round.** Each milestone ends with testers using it on their
  own projects. Each milestone lists the questions we want answered.
  Answers can reorder later milestones or change the spec (logged in
  DECISIONS.md).

---

## Overview

| # | Milestone | Size | What a tester gets |
|---|-----------|------|--------------------|
| M0 | Walking skeleton | S | Run it, browser opens, files shown as plain text |
| M1 | Docs view | L | Hackage-like pages for every module, with search |
| M2 | Navigation | M | Source view, links, highlighting, references, keyboard |
| M3 | Diff mode, textual | L | Review a branch: diff view, two sides, change markers |
| M4 | Semantic diff | L | **Reviewer beta**: expanded diff grouped by API impact |
| M5 | Real-world projects | M | Full CPP, dependency links, odd setups |
| M6 | LLM, API backends | L | Ask questions about code and changes |
| M7 | `claude-cli` backend | M | Same, with the user's Claude subscription |
| M8 | Precise tier | L | Exact links, types on hover, inferred types, from HIE files |
| M9 | v1.0 | M | Source release, a11y audit, docs |

Diff mode comes early (M3, M4) because reviewing changes is the main use of
the tool, and we want feedback on it soon. After M4 the order of M5, M6/M7
and M8 is picked from feedback (see [Order can change](#order-can-change)).

---

## M0 — Walking skeleton (S)

**Goal**: the whole pipeline exists, from CLI to browser, and is fast and
safe. Almost no features.

**In**
- Monorepo (§11.2, D-58). M0 has `haskell-reviewer` and `web/`; the index
  and hie packages come in M8. Library layers: core, effects (handles,
  D-63), session, server, app (CLI and wiring). BSD-3 (D-73).
- Builds with GHC 9.10, 9.12 and 9.14 (D-64). `make install` from source,
  with `web/dist/` committed, needs only GHC and `cabal`; `make web`
  rebuilds the bundle with pinned Node (D-65, §11.8).
- CI on GitHub Actions (§11.9, D-72): Linux and macOS × 3 GHCs; web job
  (type check, lint, Vitest, bundle reproducibility, no-eval check);
  end-to-end job; `make bench` smoke run (D-76).
- Frontend (D-48, §11.7): TypeScript strict, SolidJS, Vite; plain CSS with
  custom properties; system fonts. Embedded with `file-embed`. Lint rule
  against `innerHTML`. Check early that it all works under the strict CSP
  (no inline scripts, no `eval`, SharedWorker).
- API contract (D-50, §11.6.2), JSON only for now.
- CLI contract (§2.1): `ROOT`, `--port`, `--no-open`, `--idle-exit`,
  `--help`, `--version` (parser's GHC version from M1), `--verbose`,
  `--debug`, `--trace-file PATH` (§11.9, D-72); exit codes 0/1/2; output
  on stderr only.
- HTTP server (D-49, §11.6.1): `warp` + `wai`, hand-written router, on
  `127.0.0.1`. Random token in the URL (constant-time compare), `Host`
  check, CSP, `no-referrer`, `nosniff`, `no-store` on the API (§2,
  §9.2).
- Event stream (D-52, §11.6.2): SSE, one per browser through a
  SharedWorker; per-tab fallback with a notice. Open stream = connected;
  15 s keep-alive; event ids, refetch after reconnect. Idle exit when no
  stream is open (§2.1).
- Sidebar with the files under ROOT, plain-text file view, escaped.
  URLs `/<token>/src/tree/<path>` (§4.1).
- Minimal git layer for the file list (D-53, D-75, §11.4.1): `git
  ls-files` (tracked and untracked-but-not-ignored), hardened env and
  flags. Helpers looked up only in absolute `PATH` entries outside ROOT.
  Git ≥ 2.44 checked at start-up. Without git: plain disk walk. The rest of the git
  layer is in M3.
- Benchmark harness (`make bench`, D-71, D-76) and three reference
  projects: small (~20 modules), typical (~300 modules), large (~2,000
  modules / 500k lines). The reference projects are picked here and pinned
  to fixed commits.

**Spikes** (time-boxed, outcome logged in DECISIONS.md)
- Run the newest `ghc-lib-parser` (9.14 today, §11.3) over the reference
  projects. Count the modules it rejects (start of Q-OLD-SYNTAX) and the
  modules that use CPP. These counts set the expected numbers for later
  "done when" checks and decide how much CPP goes into M1.
- Sketch the neutral HIE index format (§7.1), so the M1 data model has a
  place for precise data and tier labels from the start. A written design
  in DECISIONS.md; the package comes in M8.

**Done when**
- URL on stderr and server accepting connections in < 300 ms (§10.1).
- Tests for token, `Host`, headers, idle exit when the last stream closes
  (with a short timeout) and exit codes 0/1/2.
- Git: git ≥ 2.44 check (exit 2); plain disk walk without git; git config
  that tries to run a command; a `git` binary planted in ROOT is never run
  (§11.9).
- Q-SSE checked on the three browsers: keep-alive, reconnect,
  background-tab throttling.
- `make install` works with only GHC and `cabal`.
- CI green on the full matrix.

**Risks**: part of the stack (Solid, Vite) needs something the CSP blocks;
SharedWorker or SSE quirks in one browser (Q-SSE).

**Feedback**: install from source and start-up; CLI ergonomics. (Mostly
internal.)

---

## M1 — Docs view, browse mode (L)

**Goal**: "Hackage for my project". The first milestone worth showing to
users. It tests the core idea: *types first*.

**In**
- Project model (§2.3, §11.4.2): `cabal.project` (own parser for the
  needed fields, D-67), else the `*.cabal` files in ROOT; `.cabal` via
  `Cabal-syntax`, conditionals from host platform and flags (`.cabal`
  default < `cabal.project`); components with source dirs, extensions and
  language; `.hs`, `.lhs` and `.hsig` files; implicit package for plain
  dirs (§8); `stack.yaml` ignored, with a banner (D-66, §8); Hpack banner
  (D-41); generated modules shown as text (§2.3); "Other files" (with
  `.hs-boot`); "listed, no source" modules; `-pgmF` "preprocessed, not
  indexed" banner; markdown-unlit `.lhs` (D-84); module identity (D-44).
- Parsing with the bundled parser, using each component's language and
  extensions (§3). The GHC AST is converted at once into our compact
  module model and dropped (D-61). Parse failure → text with a banner
  (§3.1).
- Basic CPP (§3.1). In M1, since the M0 counts say CPP is common (D-83):
  own evaluator with a position map (D-55); host platform, flags from
  `.cabal` defaults and `cabal.project`, `MIN_VERSION_*` = true (also
  for `defined`/`#ifdef`), GHC macros from the detected version (`--ghc`,
  `with-compiler`, else the parser's version; §7.1). Excluded regions
  greyed. Full CPP is in M5.
- Docs view (§4.2): every declaration kind, synopsis, Haddock sections,
  instances under class and type (collapsed), orphan flag, fixity next to
  the operator, Pragmas block, shared signatures, exported marker, "exported
  only" toggle (remembered), Main-without-header rule.
- Haddock parsed on the server (`haddock-library`) into the rich-text AST,
  no raw HTML; links resolved on the server (D-59, §11.4.9, §9.2).
- Inline declaration source (§4.3), with multi-range extents. Plain text
  for now.
- Links inside a module (local scoping). Other names are shown but not yet
  linked.
- Global search (§4.7): modules and declaration names, *partial* while
  indexing. On the server, debounced (D-69).
- Sidebar module tree by package and component (§4.1). Docs URLs and the
  "not found in this snapshot" page.
- Light/dark themes following the OS, with a manual override
  (remembered); signature wrapping at `->` / `=>` (§5.5).
- User config file (§7), read only, with the UI defaults. Other sections
  (LLM, deny-list) are added by the milestones that need them.
- Remembered UI settings (D-68, §7): preferences endpoint and
  `$XDG_STATE_HOME/haskell-reviewer/ui.json`, winning over the config's UI
  defaults. First used by "exported only" and the theme override.
- CBOR for token-heavy payloads (D-51, §11.6.2), with the `cbor-x` no-eval
  build.
- gzip for large responses, except the event stream (§11.6.1, D-79). The
  first large payloads come here.
- Progressive indexing: snapshot generations in STM, priority-queue parse
  workers (D-60, §11.5); requested module jumps the queue, progress in the
  header (§10.2).

**Left out**: highlighting, cross-module links, references, keyboard.

**Done when**
- Typical project: UI usable < 1 s (tree, search, any module page);
  navigation < 100 ms; on-demand module < 300 ms (§10.1).
- Large project: UI usable < 10 s, or the gap is recorded per §10.3.
- Golden tests for each declaration kind and for extents.
- Rich-text tests: raw HTML is escaped; `<<image-url>>` becomes a link,
  never an image.
- Reference projects: number of text fallbacks matches the M0 counts.
- Q-GC measured on the large project; compact regions or the non-moving
  GC picked if needed (§11.5).
- A fixture repo with only `stack.yaml` gets the banner.

**Risks**: < 10 s on 2,000 modules with progressive indexing; the memory
use of the in-memory model; GC pauses with a large index (Q-GC).

**Feedback**
- Is the types-first page the right main view?
- All declarations by default, or exported only?
- Density and layout: too much, too little?

---

## M2 — Navigation (M)

**Goal**: move around a project as fast as in an IDE, without building it.

**In**
- Full-source view (§4.4): virtualised, with ARIA row indexes (D-70,
  §11.7); line numbers, outline gutter, links back to docs. Find in file
  on `f` or a toolbar button, with the permanent Ctrl-F hint.
- Lexical highlighting (§5.6) from the GHC lexer tokens, in source and
  inline-source views, CPP regions merged by position.
- Cross-module resolution, import-directed (§3), with heuristic results
  marked (dotted underline + tooltip).
- External names shown with a Hoogle search link for now. The full
  dependency lookup (D-42) is in M5.
- Hover: declared signature.
- Find references, syntactic (§4.4), marked *partial* while indexing.
- Keyboard (§5.3) for browse mode: `/ j k Enter g d g r s f 1 2 y Esc ?`,
  the `?` overlay and the single-key on/off switch (D-45, remembered).
- Copy (§5.7): buttons on paths, modules, names; "copy current location";
  `y`; announced feedback. A selection is copied from the file's text, not
  the DOM.

**Done when**
- Navigation < 100 ms with links and highlighting on; typical and large
  targets still hold.
- Golden tests for resolution (exact vs heuristic) and token classes.

**Risks**: the cost of syntactic find-references on large projects (a
§10.3 candidate).

**Feedback**
- Are heuristic links honest enough? Too many dotted underlines?
- Is the keymap natural?
- What's missing to use it instead of the editor for reading code?

---

## M3 — Diff mode, textual (L)

**Goal**: review a branch. Even without semantics, this should already be
nicer than a web diff, because every line links into the right snapshot.

**In**
- `--diff` with `BASE..HEAD`, `BASE...HEAD` and `REV` (working tree) (§2).
- Git as a read-only object store (D-53, §11.4.1), on top of the M0 git
  layer: one `cat-file --batch` plus plumbing; nothing writes to `.git`.
- Sides read from git objects, never checked out (§2.2). Working-tree side
  (D-75): clean tracked files from git, dirty and untracked files from
  disk, with EOL rules. Submodules as one entry, LFS files as pointer text.
  Project model per side (§2). Parse results shared across sides by (blob,
  settings) (D-61).
- ROOT inside a bigger git tree: views and diff limited to ROOT, changed
  files outside ROOT counted in the header (D-43).
- Sides in navigation (§5.1): current side in the header, "view on other
  side", no silent switching. `b` key.
- Diff view (§4.5): unified / side-by-side (remembered), context
  expansion, ignore whitespace, renames found as git does, no copy
  detection (D-74), highlighting and links on both sides, each into its
  own side. All diffs in process: histogram with Myers fallback, unlimited
  context (D-54, §11.4.7). Virtualised, with find in file (D-70). Narrow
  screens fall back to unified (§5.4).
- Sidebar change markers and "changed only" filter (§4.1).
- Source view: gutter markers and "show diff here" (§4.4).
- Docs view on head with side switch (§4.2). Search on head by default
  (§4.7). Badges come in M4.
- Header shows revisions; copy buttons give full hashes (§5.7).
- Keys: `n p ] [ u 3`. Diff URLs (§4.1).
- Reload (§5.2): a new snapshot generation swapped in atomically (D-60),
  for browse mode and for a working-tree side, keeping the location by
  (module, declaration).

**Done when**
- §10.1 targets hold with both sides indexed.
- Memory measured on the large project in diff mode; budget set
  (provisional 2 GiB, §10.2) and logged.
- Fixture repos cover the three revspec forms, renames, untracked files,
  `.cabal` changes between sides, ROOT in a subdirectory, exit code 2
  cases, CRLF and LFS files, a copied file shown as added.
- Q-GC re-checked with both sides indexed.

**Risks**: indexing two sides doubles time and memory; clean filters and
EOL rules (D-75).

**Feedback**
- Is it already better than the GitHub/GitLab diff for review?
- Is the side model clear, or do people get lost?

---

## M4 — Semantic diff (L) → reviewer beta

**Goal**: the main promise of the tool. A change shown as declarations,
API changes first.

**In**
- Expanded diff view (§4.6): "touched" by extent; one primary class per
  entry; flags *moved*, *renamed*, *docs also changed*; layout by class then
  module, types before functions; old/new heads with intra-line
  highlighting; per-declaration diff, even across files; "Other changes".
- Instances and deriving (D-20): instance keys with α-renaming, deriving
  clause vs standalone, strategy changes.
- Rename detection (§4.6.1): bigram Jaccard, `diff.rename_threshold` in
  the config (default 0.8), min 8 tokens, greedy deterministic pairing,
  score in the tooltip, "show as removed + added" override.
- "Affected (not modified)" section, one level, syntactic tier (labelled
  heuristic).
- Docs view badges and "Removed in this change" block (D-13).
- `/changes` URLs, `4` key, `j k` in this view.
- Changed modules indexed first on both sides (§10.2).

**Done when**
- One fixture repo per primary class and per flag, plus the deriving cases.
- Property tests: classification and pairing are deterministic.
- Cost of rename detection and "Affected" measured on the large project.
  If too slow, decide per §10.3.

**Risks**: edge cases in classification (CPP, TH splices, declarations
split across ranges); rename false positives.

**Feedback**
- Is the group order right for reviewing?
- Rename false positives / misses? Is 0.8 a good threshold?
- Is "Affected" worth its cost?

---

## M5 — Real-world projects (M)

**Goal**: works on the messy projects people actually have. Driven by what
broke during M1–M4 feedback.

**In**
- Full CPP (§3.1): Cabal macros synthesised per side, `#include` inside
  the snapshot with `include-dirs`, optional built-in `MachDeps.h`
  (D-84), `--cpp-define`, `--flag` on top of
  `.cabal` defaults and `cabal.project` (D-67); the header tooltip shows
  the assumptions and where each flag value came from (§11.4.2). (Basic
  CPP is already in M1, D-83.)
- Q-OLD-SYNTAX resolved: lexing shim if needed, or text fallback with the
  reason in the banner (§7.1, §8).
- Dependency names (D-42): `plan.json`, then `build-depends` + local
  package index, then Hoogle. Labelled by source; version only from
  `plan.json` (HIE source comes in M8).
- Fixes for whatever project layouts testers report.

**Done when**
- Reference projects: text fallbacks reduced to the cases Q-OLD-SYNTAX
  accepts as unfixable, each with a reason.
- Golden tests for CPP rules and for each dependency-lookup source.

**Risks**: CPP guesses that are wrong for many projects (wrong platform
branch, missing generated headers).

**Feedback**: which projects still break? Which CPP guesses are wrong?

---

## M6 — LLM, API backends (L)

**Goal**: ask questions about the project and the change, with the LLM
locked to read-only access.

**In**
- LLM and deny-list sections of the config file (§7).
- Backends `openai-compatible` and `anthropic` (§6.7) behind one provider
  interface (§11.6.3): `claude` package for Anthropic, `openai` package plus
  our own tool-call delta decoder (D-62). Keys from env or config only.
- LLM panel, entry points and anchor chips (§6.1), including docs-view
  selections (D-26). `a` key.
- Tool loop with the §6.2 tools; containment by snapshot; visible set;
  deny-list on every byte sent (tool results, manifest, anchor); refusal
  chip. Every byte for the model goes through one LLM view of the snapshot
  (§11.6.3).
- Context (§6.2): built-in system prompt, `llm.extra_instructions`,
  manifest with caps, anchor capped at 400 lines, "show context".
- Prompt-injection rules (§6.2.1); tool-call log.
- Threads in memory, Markdown export (§6.3).
- Answers (§6.4): Markdown parsed on the server (`commonmark`) into the
  rich-text AST (D-59, §11.4.9), citation links to the right side,
  streaming, stop, copy buttons.
- Budgets and token usage (§6.5). Stale threads after reload (§6.6).
- Non-loopback confirmation, pre-grant in config (§6.7).
- "LLM not configured" hint and inline backend errors (§8).

**Done when**
- Containment test suite: path escapes, symlinks, `.git`, deny-list
  applied to each tool, manifest and anchor.
- Budget limits tested with a fake backend that loops on tool calls.
- Works with Ollama, llama.cpp, vLLM and Anthropic (Q-OPENAI-COMPAT).

**Risks**: answer quality with small local models; tool-calling quirks of
"OpenAI-compatible" servers (Q-OPENAI-COMPAT).

**Feedback**
- Which entry points get used?
- Are answers good with manifest-only context, or does the LLM waste tool
  calls finding basics?
- Are the default budgets right?

---

## M7 — `claude-cli` backend (M)

**Goal**: users without an API budget can use the LLM through their own
Claude subscription. Needs M6: it reuses its tools, containment, panel,
threads and config.

**In**
- Hidden `mcp-bridge` subcommand: our own minimal MCP stdio server
  (D-62). It relays over a Unix socket in a private temp dir; the
  per-session secret is never on a command line (§11.6.3).
- Containment layers 1–6 (§6.8, D-34), our system prompt replaces the
  CLI's, start-up tool-set verification.
- Q-CLAUDE-CLI-FLAGS resolved with real headless runs; tested CLI version
  range recorded.
- Live-thread limit and "continue" for closed threads (§6.8).
- Budgets and checks enforced in the main process; the bridge only relays
  (§11.6.3). Turn-limit backstop; token usage from result events.
- Detection and refusal reasons (§8). `claude` looked up only in absolute
  `PATH` entries outside ROOT (§11.4.1). Residual context listed in "show
  context" (D-47).

**Done when**
- A test with a fake project `.claude/` dir, hooks and `CLAUDE.md` shows
  none of them reach the CLI.
- Verification refuses a session whose tool set is wrong.
- A `claude` binary planted in ROOT is never run.

**Risks**: the CLI flags or Anthropic's terms may change. The backend is
kept apart from the rest, so it can be changed or dropped alone.

**Feedback**: does it feel the same as the API backends? Rate limits?

---

## M8 — Precise tier (L)

**Goal**: exact answers when the user has built the project with HIE files.

**In**
- `haskell-reviewer-index` and `haskell-reviewer-hie` packages (D-57,
  D-58). Companion for one GHC series, built with `make install-hie GHC=…`
  and installed as `haskell-reviewer-hie-<X.Y>`, writing the neutral,
  versioned index (§7.1) as batch CBOR. Discovery next to the binary or in
  absolute `PATH` entries outside ROOT (§11.4.1, D-75). Missing-companion hint gives the exact command (§8, §11.8).
- Companions run with a heap cap and a timeout; a crash or timeout →
  syntactic tier for that file (D-56, §11.4.6).
- HIE discovery in `dist-newstyle` and `--hie-dir` (Stack users pass
  `.stack-work` there; §9.3). HIE files as a source for the project GHC
  version (§7.1).
- Trust rule: `.hie` files tracked in the snapshot or behind a symlink in
  it are never used; without git they are used, with a banner (D-56, §3).
- Cache under `$XDG_CACHE_HOME`, keyed by `.hie` content hash and
  companion version, pruned after 30 days unused (§11.4.6).
- Verified-current rules (§3): same source bytes, all home imports current
  (transitive), same build-relevant `.cabal` stanza. Per-module, per-side
  tiers in the header.
- Using it: exact links and references, types on hover, inferred types in
  docs (marked *inferred*), exact "Affected", *inferred type changed* flag
  with promotion (§4.6), TH-generated declarations, HIE CPP branches win
  over the assumptions (§3.1), HIE as package source for dependency links
  (D-42).
- `--version` lists the companions found.

**Done when**
- Fixture projects built with that GHC; stale-HIE cases fall back to
  syntactic.
- Security suite: tracked and symlinked `.hie` files; companion crash and
  timeout; a companion planted in ROOT is never run.
- Q-PERF-COLD: cold-cache bound set from benchmarks and written into §10.1.
- Warm-cache targets of §10.1 still hold.

**Risks**: the HIE format changes between GHC series, so each companion
is partly new work; the transitive check may be slow on large projects.

**Feedback**: is the precise tier worth the build step? Is the tier display
clear?

---

## M9 — v1.0 (M)

**Goal**: ship.

**In**
- WCAG 2.1 AA audit and fixes (§5.4): screen reader pass, ARIA tree, diff
  line names, live region for answers, 200% zoom, reduced motion, contrast
  of highlight and diff colours.
- Full `make bench` pass on all §10 targets before the release (D-71);
  memory budget confirmed.
- Companion works with the other two of the three latest GHC series
  (9.10–9.14 today, §7.1).
- Source release (D-65, §11.8): tagged version; `make install` and
  `make install-hie` checked on Linux and macOS; committed bundle matches
  `make web`. No prebuilt binaries, no Hackage.
- User docs: install from source, flags, config file, how to produce HIE
  files (§9.3), known limits (Hpack, Stack, Windows, `claude-cli` residual
  context).
- SPEC.md brought in line with every change logged during the work.

**Done when**: all §10 targets met or officially revised; the audit has no
open AA issues; `make install` and `make install-hie` work from a clean
checkout on Linux and macOS.

---

## Order can change

- M0–M4 are in dependency order. Each one builds on the one before.
- After M4, these blocks are independent of each other:
  - M5 (real-world fixes),
  - M6 then M7 (LLM; M7 needs M6),
  - M8 (precise tier).

  Pick the order from M4 feedback:
  - many projects break or look wrong → M5 first;
  - testers ask "what does this change do?" → M6 first;
  - testers distrust heuristic links → M8 first.
- M7 can be dropped without touching anything else if the CLI or the terms
  change.
- M8 is late on purpose: the syntactic tier already gives every feature.
  The risk of a late redesign is kept low by the M0 index sketch and by a
  data model that carries tier labels from M1.
- Any §10.3 decision (degrade / drop) can move or shrink a milestone.

---

## Spec coverage

Every section of SPEC.md and the milestone(s) that deliver it.

| Spec | Topic | Milestone |
|------|-------|-----------|
| §1 | Principles | all |
| §2 | Invocation, server, token, `Host` check | M0 (ROOT bounds the file list); `--diff` and changed files outside ROOT M3 |
| §2.1 | Lifecycle, CLI contract | M0 (parser version M1, companions in `--version` M8) |
| §2.2 | Sides | M3 |
| §2.3 | Project model, module identity | M1; dependency links M5 (HIE source M8) |
| §3 | Tiers | syntactic M1–M2; precise M8 |
| §3.1 | CPP | basic M1 (D-83), full M5; HIE branches M8 |
| §3.1 | TH, parse failures | M1 (TH declarations M8) |
| §4.1 | Frame, URLs | M0–M1; diff URLs M3; `/changes` M4 |
| §4.2 | Docs view | M1; side switch M3; badges M4 |
| §4.3 | Inline source | M1 (highlighting M2) |
| §4.4 | Full-source view | plain text M0; M2; diff markers M3 |
| §4.5 | Diff view | M3 |
| §4.6 | Expanded diff | M4 (inferred-type flag M8) |
| §4.6.1 | Similarity metric | M4 |
| §4.7 | Global search | M1; head side by default M3 |
| §5.1 | Sides in navigation | M3 |
| §5.2 | Snapshots, reload | M3 |
| §5.3 | Keyboard | M2; diff keys M3–M4; `a` M6 |
| §5.4 | Accessibility | baseline M1; audit M9 |
| §5.5 | Visual design, themes | M1 |
| §5.6 | Highlighting | M2 |
| §5.7 | Copying | M2; hashes M3; LLM messages M6 |
| §6.1–§6.7 | LLM, API backends | M6 |
| §6.8 | `claude-cli` | M7 |
| §7 | Config file | M1 (UI defaults, remembered settings); rename threshold M4; LLM and deny-list M6 |
| §7.1 | GHC versions, companions | detection M1; HIE detection and companions M8; all three series M9 |
| §8 | Degraded modes | with each feature (M1, M3, M5, M6, M7, M8) |
| §9.1 | Untrusted code | all (never execute) |
| §9.2 | Browser hardening | M0; rich-text AST M1 |
| §9.3 | Getting HIE files | M8; docs M9 |
| §10 | Performance | harness (`make bench`) M0; targets checked from M1; memory budget M3; full pass M9 |
| §11.1 | Architecture overview | M0 (git M0/M3, LLM M6, bridge M7, companions M8) |
| §11.2 | Repository and packages | M0 (index and hie packages M8) |
| §11.3 | Tech stack | each item with its feature |
| §11.4.1 | Git layer | file list M0; object store, working-tree side M3 |
| §11.4.2 | Project model | M1; `--flag` M5 |
| §11.4.3 | CPP | basic M1, full M5 |
| §11.4.4 | Parsing, module model | M1; sharing across sides M3 |
| §11.4.5 | Resolution | local M1; cross-module M2; HIE M8 |
| §11.4.6 | HIE companions | M8 |
| §11.4.7 | Line diff, renames | M3 |
| §11.4.8 | Semantic diff | M4 |
| §11.4.9 | Rich text | Haddock M1; Markdown M6 |
| §11.4.10 | Search | M1 |
| §11.5 | State, concurrency | M1; reload M3; LLM state M6 |
| §11.6.1 | HTTP server | M0 (gzip: M1) |
| §11.6.2 | Browser protocol | M0 (CBOR M1) |
| §11.6.3 | LLM engine | M6; bridge M7 |
| §11.7 | Frontend | M0; preferences and CBOR M1; find in file, virtualisation M2 |
| §11.8 | Build, install | M0; `make install-hie` M8; source release M9 |
| §11.9 | Quality | M0, growing with each milestone; full bench pass M9 |
| §12 | Rejected features | never built |

## Open questions

| Question | Where |
|----------|-------|
| Q-OLD-SYNTAX | counts in M0, resolved in M5 |
| Q-CLAUDE-CLI-FLAGS | M7 |
| Q-PERF-COLD | M8 |
| Q-SSE | M0 |
| Q-GC | measured in M1, re-checked in M3 |
| Q-OPENAI-COMPAT | M6 |
| Q-HIE-SIZE | M8 |
| Q-HIE-UNIT | M8 |
| Q-HIE-STANZA | M8 |
| Q-HIE-PATCH | M8 |
