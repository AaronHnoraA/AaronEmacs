# Noema composition and writing plan

## Goal

Offer strong writing and lightweight model interaction in any Emacs buffer
without building another frontend or LLM client.

## Implementation rule

Emacs `tools/agent/{compose,context}` directly reuses the complete shared gptel source for composition, context, presets, transient
configuration and rewrite/diff review. Noema only supplies stable product entry
points and connects selected work to its Project/WorkNode context.

Agentic work uses the shared Emacs runtime with pristine package-vc agent-shell + acp.el; Magent supplies its local
agent, queue, ledger and gptel integration. These surfaces share Noema project
context but do not collapse compose buffers, AgentSessions and WorkNodes into
one object.

The context handed to each surface and the frozen Run budget are specified in
[agent-context.md](agent-context.md).

## Current entry points

1. `+agent/compose` opens the gptel composition surface.
2. `+agent/compose-send` sends from the current buffer.
3. `+agent/compose-add-context` uses gptel's context system.
4. `+agent/compose-menu` exposes the complete gptel transient.
5. `+agent/compose-rewrite` opens gptel rewrite/diff review.
6. `+agent/start` opens a structured Magent/Codex/Claude/OpenCode/Pi
   session through agent-shell.
7. `noema-marginnote-sync` runs a MarginNote digitization prompt in a pinned
   session of its own with the managed `marginnote-digitize` pack.  The Noema
   AI menu (`%`) exposes it under `M`; there is no separate MarginNote menu.
   Recognition and workflow decisions remain in the member Skills.

Editable Noema profiles and prompt templates remain under `etc/noema/`.
Future improvements should compose these mature implementations rather than
forking or imitating them.
