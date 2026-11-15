# Agent task isolation: plan and verification

> **For agentic workers:** Use executing-plans to implement this plan task by task. The user has authorized the primary agent to implement and verify the Emacs infrastructure repair.

**Goal:** Temporary ACP workers cannot change ordinary-session defaults or become durable project conversations.
**Architecture:** Keep ownership in `tools/agent/acp.el`; API and worktree creation pass per-task configuration through that boundary. Noema export keeps its existing adapter. Do not patch upstream agent-shell.
**Tech Stack:** Emacs Lisp, ERT, deterministic Node ACP fixture.
**Spec:** `docs/design/agent-management.md`, C62 (closing G16) and the user's explicit isolation requirement.

## Global constraints

- Preserve current ordinary model/effort preferences and running conversations.
- Temporary origins: subagent, probe, marginnote, latex-export and side; background visibility alone does not redefine an ordinary conversation.
- Keep task buffers/results reviewable; do not delete native provider history or user worktrees.
- Model/effort belong to a private configuration copy, never a provider/global variable.

## Review focus

- Asynchronous model-success callbacks in a temporary session must not save defaults.
- Effort-only configuration must isolate preferences without requiring a model pin.
- Ordinary manual sessions must still remember explicit model selections.
- Worktree workers must receive the same model/effort contract before checkout creation.
- Hidden, idle workers may be reclaimed; running/queued work must remain protected.

## Task 1: Isolate temporary sessions

Files: `lisp/tools/agent/acp.el`, `test/agent-acp-tests.el`.
Interfaces: a session-local config marker and the shared temporary-origin policy; retain `+agent-acp-pinned-config(config, model-id, effort)`.

- [x] Add failing tests for temporary-origin model writeback, effort-only pins and durable adoption.
- [x] Run focused ERT and confirm failures.
- [x] Copy/mark configuration before startup, guard preference writeback, and align temporary registration/idle cleanup policy.
- [x] Run the relevant ACP tests and check ordinary preferences remain intact.

## Task 2: Per-task API configuration and verification

Files: `lisp/tools/agent/{api,worktree}.el`, `test/agent-{api,delegation}-tests.el`, `test/fixtures/agent-flow-acp.mjs`, Noema's API description and operations skill, `docs/design/agent-management.md`.
Interfaces: optional `modelId` and `effort` on spawn or background create; normal and worktree paths share the same private copy.

- [x] Add failing API/stdio fixture tests for task model/effort and unaffected new ordinary session.
- [x] Add optional task fields, validate before side effects, use the existing pin helper.
- [x] Run `make agent-delegation-test` and `make research-test`; report any unrelated failures accurately.
- [x] Close G16 with the implemented contract; update the API description and skill.
- [x] Review the full diff, load only the changed runtime boundary into Emacs, and verify live task/ordinary state without changing user defaults.

Implementation record: user chose to retain ordinary settings; no reset of `var/agent/models.json` or provider configuration is authorized or needed.

## Evidence

- The original temporary-origin/preference/adoption regressions failed before the fix. The first ACP integration run also exposed silent fallback for an unavailable requested model; verification now fails that receipt before sending any prompt.
- `make agent-delegation-test`: 16/16 passed, using real stdio ACP processes. Includes model and effort selection, worker model changes, ordinary defaults and explicit ordinary changes, unavailable model/effort, queue and notification behavior.
- `make research-test`: startup 2/2, shared Agent/Noema group 242/242, MarginNote/capability/manager group 27/27, research group 95/96. The remaining failure is `noema-pi-deploy-writes-a-manager-only-agent-directory`: an existing independent change to `resources/pi/SYSTEM.md` says “global butler” where the test expects “Emacs companion”. Neither that prompt nor its assertion was changed by this repair.
- The final research group, skipped by Make after that failure, was run separately: 113/113 passed.
- An older popup fixture did not bind its physical ACP state, so the existing stale-client guard rejected it. The fixture now supplies the real state and verifies that a different state is still rejected; the production guard is unchanged.
- Loaded only `acp.el`, `requests.el`, `api.el`, `worktree.el` into the running Emacs. A real API review worker selected Astra/xhigh and passed pre-prompt verification. A subsequently created ordinary session still selected the retained Terra/xhigh settings and had no task options. The empty verification session was then closed.
- Hashes of both `var/agent/models.json` and `~/.codex/config.toml` remained unchanged across live loading, worker creation and ordinary-session verification.
- Independent ACP review found conflicting category defaults and effort-before-model ordering. Each was reproduced by a failing stdio regression. The helper now removes inherited entries by advertised category as well as concrete ID, only for explicitly pinned options, and applies task effort after retained defaults so model-dependent effort values are available. The final combined ACP/API/delegation/worktree suite passed 80/80, including all 18 delegation tests and export regressions.
- The same ACP reviewer confirmed both findings resolved after inspecting the final implementation and regression log; no remaining defect was found within that bounded re-review. The final helper was loaded into the running Emacs, and ordinary settings hashes still matched the original live snapshot.
- A batch state comparison loaded the HEAD versions of the changed shared modules, then the repaired versions, using the repository's state snapshot/comparison functions. After warming the snapshot's keymap autoloads, both snapshots contained 20179 lines with zero differences in global hooks, advice, keys, customizable values and features. This checks module reload state; it is not a separate cold-start baseline checkout.
- Logs and receipts are under `var/noema/marginnote/ipad-current/`; no native provider history or user worktree was removed.
