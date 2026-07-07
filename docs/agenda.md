# Aaronnote Agenda

Aaronnote agenda is a server-backed task/project/time-tracking system for
Markdown notes. The Markdown line is the single source of truth: every edit
writes back to the original `@@todo`/`@@project`/`@@milestone`/`@@clock`
command, and the Web agenda page is the only agenda UI — Emacs just opens it.

For the full DSL grammar (canonical keys/aliases, date/repeater/duration
syntax, diagnostics) and view-model shapes, see
[`lisp/roam/aaronnote/docs/agenda.md`](../lisp/roam/aaronnote/docs/agenda.md).
This page is the config-level summary.

## Syntax

```md
@@todo(state) [task text] {key: value, ...}
@@itodo(state) [task text] {key: value, ...}
```

`itodo` is an alternate spelling of `todo` — identical grammar and status set,
distinguished only by its own widget badge. `state` is usually `todo`,
`doing`, `blocked`, `done`, or `cancelled`. Omitting the state means `todo`.

| Canonical key | Read aliases | Meaning |
|---|---|---|
| `id` | - | Stable id (base36, 6 chars), minted on demand — see Dependencies. |
| `ddl` | `due`, `deadline` | Deadline. |
| `sche` | `scheduled`, `start` | Scheduled/start date. |
| `end` | `finish` | End date (Gantt/duration tasks). |
| `prio` | `priority` | Priority `A` through `F`; missing priority sorts like `D`. |
| `repeat` | `rep`, `every` | Repeater: `+1w`, `++1w`, `.+3d`; bare `1w` means `+1w`. |
| `warn` | `lead` | Deadline warning lead time such as `3d`, `2w`, `1m`; default is 14 days. |
| `after` | `dep` | Forward dependency references, separated with `&`. |
| `blocks` | - | Reverse dependency: `T {blocks: X}` is equivalent to `X {after: T}`. |
| `effort` | - | Estimated duration (`2h`, `90m`, `1d` = 8h workday) for the clocktable comparison. |
| `project` | `proj` | Project grouping key. |
| `done` | - | Most recent completion date, written by the engine. |
| `log` | - | Completion history, `&` separated and capped by the engine. |

Date values are parsed by the runtime, so ISO dates, relative forms such as
`+3d`, and supported natural/CJK forms work everywhere. Multi-word values must
be quoted. The command parser is quote-aware, so commas and semicolons inside
`"..."` or `'...'` are preserved.

`@@project`/`@@milestone`/`@@clock` also accept a bracket-less title (bare
text before `{`) instead of `[title]`.

When a line already uses an alias such as `due`, patches keep that alias. Newly
added metadata uses canonical keys.

## Dependencies

Dependencies intentionally use text references instead of hidden ids:

```md
@@todo [write conclusion] {after: "draft theorem & [[Lemma Note]]::check proof"}
```

Reference matching is runtime-computed:

- `#id` resolves directly against a todo's stable `id:` attr, no fuzzy
  matching — durable across the target's title being edited later.
- Without a note prefix (and no `#id`), the reference matches another todo
  in the same file by text.
- With `[[Note Title]]::`, the title is matched against note title/aliases.
- Todo text matching tries exact text, then a unique prefix, then a unique
  substring.
- Broken or ambiguous references become lints. They do not block a task.
- Resolved dependencies (from either `after` or `blocks`) block `todo`/`doing`
  tasks until the target todo is `done` or `cancelled`; no file write is
  needed to unlock them.
- Dependency cycles (via `after` or `blocks`) surface as a `cycle` lint in
  the Gantt model.

Ids are minted **on demand**, never on every save (org-id model): `create-todo`
always mints one; the dependency picker and `clock-in` mint one for an
existing target the first time it needs a durable anchor. Clients should not
hand-write dependency refs when a target todo is selected — they call
`todo-dep-ref`, which mints an id for the target if it doesn't have one and
returns `#id`. Passive completion (typing `after:`/`blocks:` and picking a
candidate) never mints an id: a candidate with one completes to `#id`,
otherwise to the shortest unique text ref.

## Repeating Tasks

Completing a todo through `patch-todo` with `op: "complete"` runs the server
completion engine.

- Without `repeat`, the status becomes `done` and `done:` is written.
- With `repeat`, `ddl` and/or `sche` roll forward, status resets to `todo`,
  `done:` is updated, and `log:` is appended.
- `+N` moves once from the old date, `++N` moves until future, and `.+N` moves
  from the completion date.
- Independently, the agenda's calendar/day-bucket view projects an open
  repeating todo's future occurrences (display-only, plain `+N` stepping) as
  `virtual: true` entries within the requested date range.

## Time Tracking

```md
@@clock [task-ref]{from: <date>, to: <date>, task: "#id"}
```

The bracket title is a dependency reference (same grammar as `after`/
`blocks`) naming the todo being timed; `task` is a stable-id anchor that
wins over the title text when present. `clock-in` always mints an id for
the target todo first, so the clock keeps attributing correctly even after
the todo's title changes. `to` is optional — a clock with `from` but no
`to` is running. Only one clock may run vault-wide; starting a new one
auto-closes whatever is running. The runtime aggregates clocks into
per-task/per-day/per-project totals and compares against a todo's `effort`.

## Project Rollup

Todos are grouped onto `@@project` entries by the same key everywhere
(explicit `project:`, else the nearest preceding same-file `@@project`'s
slugified title, else the note's own title). The runtime computes per-project
open/doing/done/blocked counts, a progress percentage (explicit `progress:`
wins, else `done / (total - cancelled)`), and summed effort/clocked minutes.
The Gantt model draws one swimlane per project, spanning either the project's
own `sche`/`end` or the min/max of its child tasks' dates.

## View Model

The shared API (`agenda --json`) returns day buckets, the urgency-sorted todo
list, lints, stats, and — with `includePlanning`/`includeGantt` — projects,
milestones, clocks, `clocktable`, `projectModel`, and `gantt` (tasks/backlog/
milestones/lanes). See the linked reference doc for the exact shape.

API/actions:

- `agenda --json '{"from":"2026-07-06","days":7,"includePlanning":true,"includeGantt":true}'`
- `create-todo --json '{"text":"Draft intro","project":"paper","ddl":"2026-07-15","prio":"A"}'`
- `patch-todo --json '{...}'`
- `clock-in --json '{...}'` / `clock-out --json '{...}'`
- `todo-dep-ref --json '{"targetId":"...","sourceId":"..."}'`
- `completions:todo-refs --json '{"prefix":"...","file":"..."}'` — completion
  candidates for `after:`/`blocks:`/`task:` values.
- `update-todo` remains as a compatibility wrapper.

## Web

The Web agenda page (`/agenda`) is the only agenda UI: week/list/month/log/
gantt/projects/clocktable/lints tabs, priority/deadline/scheduled/repeat
edits, quick todo capture, dependency selection, clock in/out, marks, bulk
status changes, lints, Gantt drag-to-reschedule, and source jumps. Quick
capture accepts `task | project=paper | ddl=today | sche=+1d | prio=A |
file=inbox.md`; if `file` is omitted, capture uses the current selected todo's
file, else `inbox.md`. All writes call `notes.createTodo`/`notes.patchTodo`/
`clockIn`/`clockOut`.

Inline todo widgets in the editor are display-only: status, priority badge,
repeat marker, and dependency pill are read from the parsed command and never
become a second source of truth.

The editor also completes `after:`/`blocks:`/`task:` values (same popup
mechanism as tag/roam/path completion), backed by `completions:todo-refs`.

## Emacs

Emacs does not render agenda UI natively — it opens the Web page:

- `M-x my/aaronnote-roam-agenda` — agenda (dispatch key `A`).
- `M-x my/aaronnote-roam-agenda-calendar` — month calendar view.
- `M-x my/aaronnote-roam-agenda-log` — completion log view.
- `M-x my/aaronnote-roam-agenda-gantt` — Gantt view.
- `M-x my/aaronnote-roam-agenda-projects` — project rollup view.
- `M-x my/aaronnote-roam-agenda-clock` — clocktable view.
- `M-x my/aaronnote-roam-agenda-lints` — lint view.
- `M-x my/aaronnote-roam-agenda-search` — agenda with a search query.
- `M-x my/aaronnote-roam-jump-file-todo` — jump to a todo in the current file
  (dispatch key `F`); this one stays local to Emacs (a `completing-read` over
  the current buffer's todos), not a web redirect.

`after:`/`blocks:`/`task:` values also complete locally, via
`my/aaronnote-roam-capf` (a `completion-at-point-functions` entry, so it
works through `company`/the built-in completion UI) calling the same
`todo-refs` backend service the Web editor uses.
