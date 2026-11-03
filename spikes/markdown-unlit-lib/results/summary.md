# markdown-unlit 0.6.0 as a library: results (M2/T02)

## 1. Setup

- GHC 9.14.1 only (one GHC is enough for a spike). cabal 3.16.1.0.
- Own cabal.project (`packages: . ../../haskell-reviewer`, imports the
  root freeze, root's allow-newer for cborg, pins markdown-unlit
  ==0.6.0 and base-compat ==0.15.0). The root cabal.project never sees
  it; CI builds only the root.
- Dry run planned only base-compat, markdown-unlit, haskell-reviewer
  (inplace) and the spike.
- Build: first build ~50 s (incl. base-compat and markdown-unlit); a
  clean build with those in the store, haskell-reviewer lib included:
  41 s, 0.9 GB max RSS.
- Wrappers (src/LibUnlit.hs):
  - `libUnlitMarkdownRaw`: library `unlit` text as is, with `#line`.
    Nothing if `parse` + selector find no block (no sort, no readMaybe).
  - `libUnlitLooseRaw`: our unlitLoose, Bird/LaTeX check skips the
    library's fenced lines (from `parse`).
  - `libLayout`: blocks from `parse`, our 6-line copy of toPredicate,
    our 5-line copy of parseReorderingKey. File order and no indent:
    same lines, prose blanked, `sameLines`. Else blocks one after the
    other with one `Copied (Pos fileLine (indent+1))` run per line.
    Without blank and `#line` lines it equals the library's text on
    every case and every census file.

## 2. Line mapping (results/linemap.txt)

"names" = top-level names; "wrong" = name span that doesn't cut the
name out of the file.

| Case | P0 ours | P1 lib-raw | P2 lib-layout |
|---|---|---|---|
| a plain | ok 2/2 | ok 2/2 | ok 2/2, in place, tokens = P0 |
| b fence indented 3 | ok 2/2 | 2 wrong (cols off by 3) | ok 2/2, moved, tokens = P0 |
| c `top` block last | parse error (`module` after decls) | ok | ok, moved |
| d `~~~ haskell` | no block (NoMarkdownCode), 0 names | ok 2/2 | ok 2/2, in place |
| e tabs, indented fence | ok 2/2 | 2 wrong (tab map on wrong lines) | ok 2/2, moved, tokens = P0 |
| f CPP | ok 2/2; regions 11-14 | 2 wrong, 3 "#line ignored" (one per block) | ok 2/2, tokens = P0, same regions |
| g `{-# LINE 100 #-}` | ok | wrong (foo at 100:1) | ok |
| h code line `#line 1 "x"` | wrong (foo at 1:1) | wrong | wrong |
| i `haskellish`, `{.haskell .ignore}` | parse error (takes `haskellish`) | ok | ok |
| j `~~~` inside a ```` ```sh ```` block | 2 names | 1 name | 1 name |
| k (extra) CPP in indented fence | parse error, 2 "directive not in column 1" | 1 wrong, 2 "#line ignored" | ok; regions layout 3-6 -> file 13-16 |

What lib-raw does:
- Lines follow `#line` when CPP is off (GHC renumbers).
- Columns are wrong after dedent, and so are tab columns: our
  ColumnMap is keyed by the text's lines, not GHC's renumbered ones.
- With CPP: runCpp drops every `#line` with a warning (one per
  block), so all lines after the first block are off.
- readPragmas still finds `LANGUAGE CPP` when line 1 is `#line` (f, k).
- With `#line` lines around it, a `{-# LINE 100 #-}` also takes
  effect (g). P0/P2 blank it (Note [LINE pragmas]). Cause not chased.

h is the known limit: a code line `#line N "f"` without CPP moves
GHC's lines in every pipeline (Note [LINE pragmas],
fixtures/decls/hash-line.golden). With CPP, runCpp drops it
(CppTest:451). It also makes the library's `unlit` text ambiguous, so
use `parse`, never the text (option B is out).

j: the library closes on any fence, so `~~~` closes the sh block and
the pairing shifts: `bar` is lost. That is what the real build does.
Ours parses more than GHC would.

P2 lands every name and every token (same as P0 wherever P0 is right).
That includes CPP composed after the layout (`mapSpan layout` on top of
the cpp map) and CPP regions mapped back (k). Note: with dedent, CPP
directives in an indented fence reach column 1, so CPP works there
(k). Ours reads them as text.

## 3. Cost (results/alloc.tsv, results/ladder.tsv)

Medians of 3 one-process samples at 1 MiB (textLimit). MB = 10^6
bytes allocated; ms = wall. "x" = lib-layout bytes / ours-md bytes.

| Shape | ours-md | lib-raw | lib-layout | x | ours-loose | lib-loose |
|---|---|---|---|---|---|---|
| S1 servant Server.lhs | 15.8 MB, 7.8 ms | 257 MB, 56 ms | 250 MB, 87 ms | 15.8 | 28.7 MB, 10 ms | 500 MB, 91 ms |
| S2 one big block | 32.5, 8.9 | 311, 119 | 301, 121 | 9.3 | 54.7, 12.5 | 588, 198 |
| S3 tiny blocks | 172, 35 | 570, 162 | 538, 217 | 3.1 | 264, 39 | 963, 250 |
| S4 fences only | 187, 17 | 319, 52 | 319, 37 | 1.7 | 527, 68 | 967, 148 |
| S5 prose | 9.9, 3.6 | 228, 21 | 228, 20 | 23 | 64, 13 | 503, 49 |
| S6 huge info string | ~0, 3.9 | 887, 205 | 444, 150 | huge | ~0, 5.9 | 1105, 267 |
| S7 `{` no `}` | ~0, 3.9 | 870, 267 | 435, 135 | huge | ~0, 5.8 | 1088, 362 |
| S8 top:<digits> | ~0, 4.1 | 889, 200 | 445, 120 | huge | ~0, 6.1 | 1107, 291 |
| S9 top:((..1..)) | ~0, 4.0 | 889, 172 | 445, 118 | huge | ~0, 5.9 | 1107, 301 |
| S10 many top:i | 78, 14 | 520, 108 | 493, 153 | 6.4 | 121, 12 | 784, 137 |
| S11 indented fence | 9.7, 8.1 | 283, 82 | 239, 118 | 25 | 16, 14 | 517, 178 |
| S12 one line | ~0, 1.9 | 218, 20 | 218, 19 | huge | ~0, 6.0 | 436, 42 |
| S13 ``` / ~~~ | 134, 14 | 319, 46 | 319, 37 | 2.4 | 440, 56 | 967, 131 |

- Linear: bytes(1 MiB)/bytes(256 KiB) is 3.97-4.17 for every lib
  impl and shape (limit 6). Ours: 3.77-4.11.
- String floor: ~218 bytes per input char (S12, S5), up to ~540
  (S3) for lib-layout, ~1.05 KB for lib-loose (it parses twice).
- Residency: max_mem_in_use at 1 MiB minus the `none` run on the same
  shape (`none` holds just the input). The RTS updates it only at a
  major GC, so it's a lower bound, and `none` reads 0 when no major GC
  ran. Real file (S1): ours-md 10.5 MB, lib-raw 24.1, lib-layout 33.6.
  Worst over all shapes: ours-md 29.4 MB (S3), lib-raw 98.6 (S3),
  lib-layout 126.9 (S3; max_live 52 MB). S6/S7 show 65 MB even for
  `none`: that's the input generator, not unlit.
- S8/S9 ladders (16 B to 1 MiB, doubling, lib-raw, lib-layout,
  lib-loose): no timeout. Largest step 1 MiB: 0.12 s (lib-layout),
  0.26 s (lib-raw), 0.31 s (lib-loose). readMaybe on deep parens is
  linear (checked alone: 500k parens 0.46 s in ghc -e).
- "Has a block?" without sort or readMaybe (lib-has-block, medians at
  1 MiB): stops at the first selected block, so ~0 on S1, S2, S3, S10.
  With no Haskell block it reads the whole file as String: S4 and S13
  319 MB / 38 ms, S5 228 MB / 20 ms, S12 218 MB / 20 ms. A huge first
  fence line costs the same: S6-S9 435-445 MB / 58-84 ms. Ratio
  1M/256K is 3.99-4.00. It runs outside the 2 s limit in
  earlyWarnings: linear, but not free.
- Unlit's share of a parse (S1, 1 MiB, unlit + parseSource + span
  mapping): ours 287 MB / 89 ms total, unlit 5.5% of bytes, ~9% of
  time. Library 521 MB / 153 ms total, lib-layout 48% of bytes, ~57%
  of time.
- D-173/D-216 are background only (model limits, not unlit).

## 4. Census (results/census.tsv)

`t13a-projects` (servant at c12cc37; optparse-applicative, pandoc,
wire-server have no .lhs), `t13b-projects/cabal`,
`t13b-projects/clash-compiler` (no .lhs). Symlinks not followed.

- 36 files: 33 servant, 3 cabal testsuite. Two more servant files
  (servant-client/README.lhs, servant-http-streams/README.lhs) are
  symlinks to README.md: no source, never read.
- Product mode: 28 MarkdownUnlit (servant cookbook/tutorial named by a
  .cabal, plus servant-auth-server README via `test-suite readme`),
  8 LooseLiterate (5 servant: MysqlBasics, PostgresPool, MultiVerb,
  OpenIdConnect, ServersEqual; 3 cabal). Loose choice: library and
  ours agree on all 8 (5 markdown, 3 GHC unlit).
- Differing code lines: only servant-auth-server README: 111 lines,
  7 blocks, class `tilde` (all `~~~ haskell`). Ours: no block,
  NoMarkdownCode, 0 names. Library layout: parses, 14 names, all
  right. Every other file: 0 differing lines.
- No `top` block, no indented Haskell fence (only 6 indented
  ```` ```json ```` in Docs.lhs), no `-optL` anywhere. All 33 servant
  files lay out in place: no reorder, no dedent.
- Info strings seen (fence lines of the 36 real files, indent and
  trailing blanks stripped): ```` ``` haskell ```` 180 (exact: three
  backticks, one space, `haskell`, nothing after; 212 lines start with
  it, the 32 extra are ```` ``` haskell ignore ````), ```` ```haskell ````
  125, ```` ``` haskell ignore ```` 32, json 8, javascript 8, bash 7+3,
  `~~~ haskell` 7, `haskell ignore` 4, `haskell,ignore` 3 (one word:
  unselected by both), console 2, text with 8-9 backticks, others 1
  each.
- Parse (product settings): both parse 35/36. MultiVerb.lhs (loose,
  GHC2021) fails in both with the same error, not unlit's doing.
- Name slices: 0 wrong on either side. Tokens (census.tsv
  same_tokens): equal on 34 of 36 files: the 31 servant files read as
  markdown, and the 3 cabal files (GHC unlit on both sides, so equal by
  construction). Not equal: servant-auth-server README (ours has no
  code). Not compared: MultiVerb.lhs (both fail). P1 (raw text) has 0
  wrong names too (in place, no dedent).

## 5. D-221 gaps

| Gap | Cases | Census | Text fix enough? |
|---|---|---|---|
| `~~~` fences | d | 1 file (servant-auth-server README: 7 blocks, 111 lines, 14 names) | yes, in place |
| Class as whole word (`.`, `{}`) | i | 0 | yes |
| `top` / `top:N` reorder | c | 0 | no: needs layout + span map |
| Dedent of indented fence | b, e, k | 0 | no: needs layout + map (columns, tabs); also changes CPP (k) |
| Pairing: any fence closes | j | 0 | yes; unlitLoose's Bird/LaTeX check must use the same fence lines |

## 6. Recommendation: (C)

Rule check for (A):
- lib-layout linear: yes.
- S8/S9 under 2 s at 1 MiB: yes.
- ≤ ~5× ours-md at 1 MiB with wall well under 100 ms: no. S1 15.8×
  and 87 ms (median; 76-89 ms over the 6 S1 lib-layout samples in
  alloc.tsv); S5 23×; S11 25×; one-line shapes far
  more; S3 217 ms; lib-loose up to 362 ms.

So (C): port the 0.6.0 rules to SourceText on Text (~40 lines, MIT
credit in Note [Unlit], no runtime dep). Keep the library as a
test-suite oracle (differential property + the tarball's UnlitSpec
cases). base-compat and markdown-unlit go in the freeze, test-only.
Bird/LaTeX unlit stays ours.

T02b shape:
1. Fences `` ` `` and `~`, closed by any fence; classes as words (`.`
   is a space, `{...}` unwrapped to the first `}`); same fence lines in
   unlitLoose's Bird/LaTeX check.
2. In place when blocks are in file order with no indent (all of the
   corpus). Else reordered/dedented text plus a PositionMap through
   SpanMap, composed after CPP, CPP regions mapped. P2 shows this
   works (b, c, e, k). The corpus has none, so ask the human: full
   mapping, or show such files as text with a notice.
3. NoMarkdownCode and earlyWarnings decide "has a block" without
   sorting or reading `top:` keys.
4. Tests per gap; name-slice goldens for `top`, dedent, CPP-in-indent;
   linear-time test for `top:` digits and parens.
5. Census: servant-auth-server README parses (14 names).
6. SPEC §2.3 ("approximate" -> markdown-unlit 0.6.0's rules), §11.3
   row; new decision closing D-221's follow-up, amending D-84, D-223.
7. Freeze: add `any.base-compat ==0.15.0`, `any.markdown-unlit
   ==0.6.0` by hand (note like D-150); CI on 9.10/9.12/9.14.
