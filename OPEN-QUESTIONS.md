# Open questions

The spec is approved as v1.1. The items below are verification tasks for the
implementation phase. Their outcomes are logged in DECISIONS.md, and SPEC.md
is updated if needed (§10.3).

## To verify during implementation (not blocking the spec)
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
  Not checked yet: EventSource inside a SharedWorker on Safari. Known:
  Chrome's Memory Saver discards idle tabs, which closes their stream, so
  the tool may idle-exit while the tab still looks open (D-80).
