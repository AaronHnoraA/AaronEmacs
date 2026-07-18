# Embedded Magent provenance

Magent is merged into ai-workbench as repository source, not installed or
loaded as a Magent package dependency.

- Upstream: <https://github.com/jamie-cui/magent>
- Imported commit: `412a12cbe9151d11eb66ce3dbdd893324fb36825`
- Imported: 2026-07-18
- Local path: `site-lisp/ai-workbench/magent/`
- Upstream license: GPL-3.0-or-later (the upstream `LICENSE` is retained)

The imported source, prompts, skills, tests, scripts, and documentation are
kept together so updates can be reviewed against a precise upstream commit.
Compiled files and upstream Git metadata are not part of the import.

## ai-workbench integration delta

The local integration deliberately keeps these changes small and auditable:

1. `magent-agent-sampler-function` makes the provider sampler dynamically
   selectable per queued turn.
2. `magent-runtime-submit` captures an optional sampler on each submission, so
   queue order cannot leak one backend into another.
3. process request handles can be aborted by Magent's normal cancellation path.
4. `ai-workbench-magent.el` provides the shared API/CLI control plane.
5. `ai-workbench-magent-cli.el` maps Codex, Claude Code, and OpenCode JSON event
   streams into Magent's provider-neutral events.
6. Path normalization and tests account for macOS `/tmp`/`/var` aliases, and
   memory scan exclusions are matched relative to the configured root.

API turns use Magent's native gptel agent loop and Magent tools. CLI turns keep
their coding agent's own tools and permission model; Magent owns the outer
queue, durable session, lifecycle, cancellation, transcript, and outer-turn
audit trail.

## Update procedure

1. Fetch the desired upstream commit into a temporary clone.
2. Compare it with the commit recorded above, including prompts and tests.
3. Import source while excluding `.git` and compiled files.
4. Reapply or revise only the integration delta listed above.
5. Run Magent unit tests, ai-workbench bridge tests, Emacs byte compilation,
   and the startup smoke test before updating the recorded commit.
