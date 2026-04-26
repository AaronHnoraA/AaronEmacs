# ai-workbench AI Writing Plan

## Research notes

Source: https://github.com/lobehub/lobehub

LobeHub positions agents as the unit of work, with project/workspace organization, editable memory, branching conversations, artifacts, files/knowledge base, and multi-provider support. For ai-workbench's writing use case, the useful translation is: writing profiles are agents/personas, Org/Markdown buffers are the workspace, selected text and files are knowledge/context, and editable prompt templates are memory/behavior.

## Goals

1. Make active capabilities discoverable without leaving Emacs.
2. Treat writing profiles as first-class personas rather than hidden prompt files.
3. Let writing prompts be drafted directly into Claude/Codex for final editing.
4. Keep all injected behavior editable under `etc/ai-workbench`.
5. Add fast writing workflows: polish, rewrite, summarize, translate, outline, continue, critique.

## Implementation steps

1. Add editable template management.
   - Add template discovery and edit commands.
   - Surface templates in the status panel.

2. Add writing templates.
   - Add editable writing workflow template.
   - Support task modes without hardcoding long prompts in Elisp.

3. Add direct writing prompt drafting.
   - Reuse the same reference picker and template renderer.
   - Draft generated prompts into the current Claude/Codex TUI.
   - Let the user edit or submit inside the backend buffer.

4. Tighten status panel as the workbench hub.
   - Show backend, writing profile, session state, and editable templates.
   - Add keys for profile preview, template editing, output, and result.

5. Verify.
   - Run ERT tests.
   - Run batch byte compile with `load-prefer-newer`.
