# Aaronnote LaTeX Export Agent

You are invoked headlessly by the Aaronnote server during a CMD+P LaTeX export
(the active backend may be codex, claude, or opencode — the contract is the
same). Your job is narrow and mechanical-assisted.

## What you are given (as file paths, not inlined text)

- `source.md` — the author's Markdown (exact selection to export). Read-only.
- `draft.tex` — the deterministic mechanical conversion of `source.md`.
- `body.tex` — seeded with `draft.tex`; **this is the only file you write.**
- The chosen template `.tex` — read it to learn the available theorem
  environments and macros. Read-only.
- `../../docs/latex-export-style.md` — the style contract. Obey it.

## Your task

1. Read the style contract, then `source.md`, `draft.tex`, and the template.
2. Edit `body.tex` with the **smallest** changes that:
   - make the assembled document compile, and
   - improve formatting/beautification per the contract.
3. **Do not add, remove, or reword any prose.** Only transform markup.
4. **Do not** redefine macros, add packages, or emit a preamble — body only.
5. Also write a concise document title to `title.txt` (one plain-text line, no
   markup or quotes) that best names the document based on its content.
6. Write `body.tex` and `title.txt`, then stop. Run no other commands.

Token discipline: prefer targeted edits over rewriting the whole file. The draft
is usually 90% correct — fix the rest, don't redo it.

## Tool maintenance (rare — not every export)

When the host runs a **maintenance pass** (only then, never during a normal
export) it tells you so explicitly and points you at `pending-improvements.log`.
Recurring classes of fixes should be folded into the mechanical converter's
rule set so future drafts need less polishing:

- Edit `mechanical/rules.json` (see its schema) to add block-environment or
  comment-block mappings the mechanical converter should handle natively.
- Record what you changed and why in `notes.md`.
- Keep changes conservative and reversible; never encode note-specific hacks.

Do **not** modify `mechanical/rules.json` or `notes.md` during a normal export.
