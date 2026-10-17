# Decision log

| ID | Decision | Rationale | Round |
|----|----------|-----------|-------|
| D-01 | Local web app: a single binary serves a UI on `127.0.0.1` and the browser opens it. | Hackage-like rendering, links, side-by-side diffs and text selection need a rich UI. A browser gives these without per-toolkit work. | 1 |
| D-02 | Two semantic tiers: syntactic (always) and precise (HIE, when verified current). | Must work on broken or unbuilt PRs, and be exact when a build exists. | 1 |
| D-03 | Diff input is git revisions; ROOT is a git work tree. | Both sides can be read exactly from git objects, so no checkout or patch application is needed. | 1 |
| D-04 | Purely read-only: no comments, no "reviewed" markers. | Complexity budget. | 1 |
| D-05 | LLM backends: OpenAI-compatible endpoints plus Anthropic. *(Amended by D-33: plus `claude-cli`.)* | Covers hosted and local models with two adapters. | 1 |
| D-06 | Scope is a Cabal/Stack multi-package project; dependency names link to Hackage. | Browsing dependencies in-app would multiply complexity and indexing time. | 1 |
| D-07 | The tool never builds the project; it only uses existing, verified-current HIE files. | A build runs untrusted code (TH, Setup.hs), and reviewing a PR must be safe. | 1 |
| D-08 | No file watching; snapshots plus a manual Reload. | A stable snapshot suits reviewing, and costs less. | 1 |
| D-09 | The expanded diff has a collapsed "Affected (not modified)" section with one level of direct users. | High review value; it reuses find-references. | 1 |
| D-10 | The docs view shows all top-level declarations by default, with an "exported only" toggle. | Reviewers need to see internals. | 1 |
| D-11 | In the expanded diff, only whole declarations are clickable (they open the full-source view); hover still works. | The view stays a summary. | 1 |
| D-12 | LLM: several threads per session, in memory only, exportable as Markdown. | Separate contexts, no storage design. | 1 |
| D-13 | In diff mode the docs view shows the head side with change badges that link to the per-declaration diff; removed declarations are listed separately. | Turns the docs view into an API-change map at little cost. | 2 |
| D-14 | Name search only; no search by type. | Complexity budget. | 2 |
| D-15 | No per-project config, not even in the user config; project needs are passed as CLI flags. | Untrusted input; one place to look. | 2 |
| D-16 | LLM automatic context = system prompt + manifest + anchor; everything else is fetched through tools. | Cost and predictability on large diffs. | 2 |
| D-17 | Platforms: Linux and macOS; no Windows in v1. | Scope. | 2 |
| D-18 | ~~Parsers for the 3 latest GHC majors in one binary.~~ **Superseded by D-35.** | Not feasible: ghc-lib-parser versions cannot be linked together. | 2 |
| D-19 | Renames are detected by body similarity, labelled "inferred", and can be overridden to removed + added. | Refactoring PRs would otherwise be noisy. | 3 |
| D-20 | Every instance (explicit, standalone deriving, or deriving clause) is a declaration matched by (class, head type); deriving changes = signature change of the type + added/removed instance. | Instances are API. | 3 |
| D-21 | Fixed single-key keymap in GitHub/Gmail style; not remappable in v1. *(Amended by D-45: can be switched off.)* | Familiar; scope. | 3 |
| D-22 | Own clean style with a Hackage-inspired layout; light/dark following the OS. | Readability without theming scope. | 3 |
| D-23 | LLM limits per answer on tool calls and bytes read; token usage shown per thread (no cost). | Prevents runaway loops and bills; no price tables. | 3 |
| D-24 | Target 2,000 modules / 500k LOC: UI within 10 s, progressive indexing, partial results labelled. | Industrial codebases. | 3 |
| D-25 | CPP is evaluated with host os/arch, default flags and `MIN_VERSION_pkg` = true (GHC macros from the detected version; see §3.1); `--cpp-define`/`--flag` override; excluded regions stay visible, greyed. | Deterministic with no build plan; wrong guesses stay visible. | 4 |
| D-26 | A selection in the docs view anchors to the enclosing declaration's source extent, plus a quote. | Gives the LLM real code coordinates. | 4 |
| D-27 | URLs are readable and semantic, valid within one session only; unresolvable URLs show a search fallback. | Back/forward and tabs, without a promise of permalinks. | 5 |
| D-28 | Rename similarity = multiset Jaccard over GHC-lexer token bigrams, own name normalised, ≥ 0.8, min 8 tokens, greedy deterministic pairing. | Deterministic and explainable. | 5 |
| D-29 | Accessibility target is WCAG 2.1 AA. | Professional baseline. | 5 |
| D-30 | Foreground process; Ctrl-C, or idle exit after 5 min with no tab; exit codes 0/1/2; output on stderr only. | No lingering daemons; scriptable. | 5 |
| D-31 | Syntax highlighting is lexical only (8 token classes), reusing the GHC lexer tokens; no client-side library, no semantic colouring. | Lightweight; adds no extra pass. | 6 |
| D-32 | Copy buttons on paths, modules, qualified names, full revision hashes, LLM messages and code blocks; "copy location"; `y` key; announced feedback. | Copying important information must be easy. | 6 |
| D-33 | Third LLM backend `claude-cli`: drives the user's own unmodified `claude` binary headlessly; haskell-reviewer never handles subscription credentials. | Low-budget users; the only route that complies with Anthropic's credential policy. | 6 |
| D-34 | `claude-cli` containment: no built-in tools; only our MCP tools (strict); no project or user settings or hooks; empty temp cwd; no persistence; start-up tool-set verification. | Defence in depth; "the LLM can only read the project". | 6 |
| D-35 | One parser (newest GHC) in the main binary; precise tier via optional per-GHC companion `haskell-reviewer-hie-X.Y` converting `.hie` into a neutral cached index. | Supports 3 GHCs without bloat; HIE reading is version-locked. | 6 |
| D-36 | Performance is principle #2, with hard start-up targets (URL < 300 ms, typical < 1 s, large < 10 s); benchmarks may change the spec; cuts are decided case by case. | User priority: features yield to performance. | 6 |
| D-37 | Fixed built-in, versioned system prompt; an optional `llm.extra_instructions` is appended, never replacing it. | Customise tone or language without weakening the rules. | 7 |
| D-38 | Manifest capped at about 8k tokens / 200 declarations, then per-module counts plus a pointer to tools; anchor capped at 400 lines. | Predictable cost per thread. | 7 |
| D-39 | Prompt injection: tool results marked untrusted; the prompt says report, never obey; documented residual risk; no heuristic scanner. | Read-only tools bound the impact; the scanner isn't worth its cost. | 7 |
| D-40 | Each expanded-diff entry has one primary class (added/removed > signature > body > docs > formatting) plus flags (moved, renamed, docs-also, inferred-type-changed, the last promoted to signature). | Deterministic; moves and renames never bury API changes. | 8 |
| D-41 | No Hpack support; documented limitation. | User decision; complexity budget. | 8 |
| D-42 | Dependency package resolution: plan.json → HIE → build-depends + local package index → Hoogle search link; labelled by source. | Uses only on-disk data; honest precision. | 8 |
| D-43 | ROOT bounds views, diff and LLM; changes outside ROOT are counted in the header. | Monorepos; nothing is hidden silently. | 8 |
| D-44 | Module identity is `package:component:Module`; the short form only when unique. | `Main` and duplicate module names across components. | 8 |
| D-45 | One setting turns off single-key shortcuts; every action also has a UI control. | WCAG 2.1 SC 2.1.4 (Level A). | 8 |
| D-46 | Browser hardening: strict CSP, no remote resources, no-referrer, sanitised Markdown/Haddock (no raw HTML, images as links). | Untrusted content shares the page with the session token; blocks exfiltration via image URLs. | 8 |
| D-47 | claude-cli: our prompt replaces Claude Code's; the user's own memory and managed policy are documented as residual context outside our control. | `--bare` would disable subscription sign-in; the project's containment is unaffected. | 8 |
