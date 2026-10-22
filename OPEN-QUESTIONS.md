# Open questions

The spec is approved as v1.1. The items below are verification tasks for the
implementation phase. Their outcomes are logged in DECISIONS.md, and SPEC.md
is updated if needed (§10.3).

## To verify during implementation (not blocking the spec)
- **Q-RTS-BLACKHOLE**: On GHC 9.12 and 9.14 (not 9.10) the threaded test
  binary sometimes hung (about 5% of runs of the PackageGlob tests): several
  capabilities spin on a `CAF_BLACKHOLE` whose blocking queue was already
  woken, so GC sync never ends and SIGTERM is ignored. Not our code (pure,
  no `unsafe*`); standalone repros didn't hang. Workaround: shared test
  values are built once with `withResource` (Note [Shared values in tests]).
  Open: root cause, GHC issue, and whether the server can hit it. Until
  then, the server builds shared values fully before request threads use
  them.
- **Q-OLD-SYNTAX**: How real is "the newest parser rejects old code"? The
  known cases (e.g. changes to `forall`/`!`/`~` lexing, `NoStarIsType`) need
  to be checked against the 3 supported GHCs, which may require a lexing-mode
  shim.
  M0 (D-83): the 9.14 parser rejects 0 of 2,355 files in the reference
  projects, and no old-syntax case in the extras. On a checklist of known
  cases, 9.10, 9.12 and 9.14 accept and reject the same things; `forall`
  as a term name is rejected from 9.10 on (9.6 accepts it; 9.8 not
  tested). Re-run: `spikes/parser-count/old-syntax-probes/run.sh`. No
  shim needed for now.
  Left open until M1 parses more projects (check the text fallbacks).
- **Q-CLAUDE-CLI-FLAGS**: Confirm the exact `claude` flags for D-34/D-47
  with a real headless run on the tested CLI version range. Flags were seen in
  `claude --help` 2.1.281: `--tools`, `--mcp-config`, `--strict-mcp-config`,
  `--allowedTools`, `--restricted`, `--setting-sources`, `--system-prompt`,
  `--no-session-persistence`, `--input-format/--output-format stream-json`.
  Also confirm that the tool list appears in the init event, the MCP tool
  naming (`mcp__<server>__<tool>`), and a turn-limit flag for the §6.5
  backstop.
- **Q-PERF-COLD**: Set a bound for the cold HIE-conversion cache from the
  first benchmarks (§10.1).
- **Q-GC**: Check that major GC pauses stay within the < 100 ms
  navigation target with large indexes. Options: compact regions for
  finished module models, or the non-moving collector (§11.5).
- **Q-OPENAI-COMPAT**: Check that the `openai` package's raw stream and our
  delta decoder work with Ollama, llama.cpp and vLLM, not only OpenAI
  (§11.6.3).
- **Q-SSE**: Check SharedWorker + SSE keep-alive and reconnect on the
  three browsers, including background-tab throttling (§11.6.2).
  Known: Chrome's Memory Saver discards idle tabs, which closes their
  stream, so the tool may idle-exit while the tab still looks open (D-80).
  M0 (D-89): `make e2e` now checks, on Chromium and Firefox (and WebKit in
  CI): one stream for several tabs, the fallback (no SharedWorker), a
  second browser, keep-alive over 40 s, a closed browser noticed within
  about 30 s, reconnect after a cut, the end of a session (new run, and
  30 s refused), and on Chromium a tab frozen for 40 s (CDP) keeps the
  stream. All pass locally on Chromium and Firefox.
  WebKit: can't run on the dev machine (Fedora; Playwright's WebKit wants
  Ubuntu libs), so it runs only in CI. Not known yet whether its
  SharedWorker has EventSource; if not, the fallback kicks in and the
  "one stream" test fails there.
  Still manual: real background-tab throttling, Memory Saver, real
  Safari, sleep/wake.
- **Q-HIE-SIZE**: Measure the index size and decode time on pandoc and
  wire-server built with `-fwrite-ide-info`, against the guess of 10–20 KB
  per module (D-87). Are types at uses and local types worth keeping?
- **Q-HIE-UNIT**: Which unit ids does cabal give executables, test suites
  and sub-libraries, with cabal 3.14/3.16 and GHC 9.10–9.14? Does the
  `-inplace-<comp>` pattern hold without `plan.json`, and with
  `--hie-dir`? (Matching a `.hie` to a component, D-87.)
- **Q-HIE-STANZA**: A `.hie` file doesn't record extensions, flags or
  cpp-options. How do we check "same build-relevant stanza" (§3)? Option:
  compare the side's stanza with the working tree's, and require the
  `.hie` to be newer than the `.cabal` file.
- **Q-HIE-PATCH**: `readHieFile` rejects other patch levels of the same
  series (`hieVersion` 9122 vs 9124). Check `readHieFileWithVersion` with
  a series-only check. Else, one companion per exact GHC version (a §7.1
  change).
