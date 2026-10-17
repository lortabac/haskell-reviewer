# Open questions

The spec is approved as v1.1. The items below are verification tasks for the
implementation phase. Their outcomes are logged in DECISIONS.md, and SPEC.md
is updated if needed (§10.3).

## To verify during implementation (not blocking the spec)
- **Q-OLD-SYNTAX**: How real is "the newest parser rejects old code"? The
  known cases (e.g. changes to `forall`/`!`/`~` lexing, `NoStarIsType`) need
  to be checked against the 3 supported GHCs, which may require a lexing-mode
  shim.
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
