# Aaronnote LaTeX Export — Style Contract

This is the contract for converting an Aaronnote note (Markdown source) into a
LaTeX **body** for CMD+P export. The mechanical converter
(`server/lib/latex-export.mjs`) produces a first draft; codex then *polishes*
that draft. Both must obey the rules below.

## Absolute rules

1. **Never change the prose.** Do not add, remove, reorder, translate, or reword
   any sentence, word, number, symbol, or citation the author wrote. You only
   change *formatting/markup*, never *content*. When unsure, keep the source
   text verbatim.
2. **The output must compile.** The host wraps your body in a template and runs
   the LaTeX compiler. Fix anything that would not compile (unbalanced braces,
   undefined environments, stray Markdown that leaked through), but only by
   correcting markup — not by deleting content.
3. **Do not touch the preamble or macros.** Title, date, author fields, the
   global math-macro preamble (`{{macros}}`), and `\documentclass` are filled by
   the host. Do not redefine macros or add packages inside the body. Emit body
   content only (what goes between `\begin{document}` … `\end{document}`).
4. **Preserve math verbatim.** Keep inline math `\(...\)` and display math
   `\[...\]` (and `$$...$$` → `\[...\]`) exactly as written. Never escape
   backslashes inside math. Macros such as `\rank`, `\ket`, `\abs` are provided
   globally — use them, never redefine them.

## Construct mapping

| Aaronnote / Markdown | LaTeX |
|---|---|
| `# .. ######` | `\section` / `\subsection` / `\subsubsection` / `\paragraph` |
| `- item` / `* item` / `+ item` | `itemize` (indent = nesting) |
| `1. item` / `1) item` | `enumerate` |
| `> quote` | `quote` |
| ` ```lang ` fenced code | `verbatim` |
| `**x**` / `__x__` | `\textbf{x}` |
| `*x*` / `_x_` | `\emph{x}` |
| `` `x` `` | `\texttt{x}` |
| `[t](url)` / `![t](url)` | `\href{url}{t}` |
| `@@todo(...) [...]{...}` | **dropped** (todos never appear in exports) |
| `#+begin meta ... #+end meta` | consumed for title/date; not emitted |

## Block environments (`#+begin kind ... #+end kind`)

Map `kind` to the theorem-like environment of the same name available in the
active template:

- `theorem`, `lemma`, `proposition`, `corollary`, `definition`/`define`,
  `remark`, `example` → `\begin{<env>}[optional title] ... \end{<env>}`.
- `proof` → `\begin{proof}[Proof (…)] ... \end{proof}`. A `=>` / `<=` title
  becomes `Proof (\(\Rightarrow\))` / `Proof (\(\Leftarrow\))`.
- Comment-like blocks (`comment`, `summary`, `note`, `important`, `warning`,
  `attention`) → `remark` with the kind/title as the label.
- Unknown kinds → `\paragraph{kind}` fallback (never drop the content).

The **assignment** template additionally offers `prob`, `sol`, `ans`, `pf`.
Prefer those there when the source clearly marks problems/solutions, but only if
it does not change any text.

## Escaping (prose only, never math)

Escape `# $ % & _ { } ^ ~` and a literal backslash in ordinary prose. Do **not**
escape inside `\(...\)`, `\[...\]`, or `verbatim`. Keep CJK text as-is (templates
load CJK support).

## Available theorem environments per template

Read the chosen template file (passed to you) to see exactly which
`\newtheorem` environments and macros exist before using them. Do not invent an
environment the template does not define.

## Pipeline reminder

Mechanical first, AI second. You start from `draft.tex` (the mechanical output)
and `source.md` (the author's text). Make the **smallest** edits that satisfy
the rules above, write the final body to `body.tex`, and stop. See
`agents/latex-export/AGENTS.md` for the operational contract and the (rare) tool
maintenance workflow.
