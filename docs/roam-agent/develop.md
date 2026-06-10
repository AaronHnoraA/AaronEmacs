# Development Gate

This file controls when autonomous AI maintenance is allowed to change tooling such as `agent/skill/*.py`, prompts, or other behavior that alters the maintenance system itself.

## Default Rule

- Regeneration of derived files under `agent/index/`, `agent/wiki/`, and compact context files is allowed during normal maintenance.
- Autonomous AI must not change maintenance scripts, prompts, or workflow rules unless one of the gate conditions below is met.

## Gate Conditions

Autonomous development is allowed only when at least one condition is true:

1. A human explicitly asked for the development change in the current conversation.
2. There is a severe defect:
   - data loss risk
   - incorrect source verification behavior
   - broken maintenance flow
   - repeated invalid output that makes retrieval unreliable
3. The corresponding issue in `## Active Issues` has at least `5` valid autonomous votes.

If none of the conditions holds, the AI may only update this file by adding or editing votes or clarifying issue text. It must not modify the tooling itself.

## Voting Rules

- AI may create a new issue when no close existing issue matches the request.
- AI may cast votes on existing issues or on an issue it created in the same activation.
- Each AI activation may cast at most one vote per issue.
- An autonomous vote counts only once per `issue_id` and `basis_head`.
- `basis_head` means the current `git rev-parse HEAD` seen by that activation before it makes changes.
- AI must not create duplicate issues for the same request. It should merge votes into the closest existing issue.
- AI must not copy an old vote with a new row unless it has materially new evidence.
- AI must not vote on more than `3` issues in one activation.
- Human-authored votes or direct human instructions override autonomous vote limits.

## Anti-Abuse Rules

- Count only rows marked `auto`.
- Count only the earliest vote for a given `(issue_id, basis_head)` pair.
- A valid autonomous vote must include:
  - `vote_id`
  - `issue_id`
  - `basis_head`
  - `kind`
  - short `reason`
- If an activation finds suspicious duplicate issues, repeated restatements, or vote farming, it must consolidate the issue text and ignore the duplicated votes.
- New AI-created issues should use stable IDs such as `DEV-002`, `DEV-003`, and include enough notes to help later activations merge duplicates instead of opening parallel issues.
- AI may not bump vote counts by editing historical rows; add a new row or do nothing.

## Required Maintenance Behavior

- `make llm` runs must read this file before deciding to edit tooling.
- If tooling changes are blocked, the AI should prefer:
  - regenerating derived files
  - improving documentation
  - recording a vote here
  - doing nothing
- Any autonomous tooling change must cite the triggering condition in its final report.
- After a development item is implemented, remove its issue row and related vote rows from this file instead of keeping a long historical log.

## Active Issues

| issue_id | area | title | status | human_override | votes_needed | notes |
| --- | --- | --- | --- | --- | --- | --- |
| None | - | - | - | - | - | Add rows only for still-open development requests. |

## Vote Ledger

| vote_id | issue_id | basis_head | kind | source | reason |
| --- | --- | --- | --- | --- | --- |
| None | - | - | - | - | Remove rows after the corresponding development item is completed. |
