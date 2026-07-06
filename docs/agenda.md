# Aaronnote Agenda

Aaronnote agenda is a server-backed task system for Markdown notes. The
Markdown line is the single source of truth: every edit writes back to the
original `@@todo` command, and both Web and Emacs render the same runtime
view-model.

## Syntax

```md
@@todo(state) [task text] {key: value, ...}
```

`state` is usually `todo`, `doing`, `blocked`, `done`, or `cancelled`. Omitting
the state means `todo`.

| Canonical key | Read aliases | Meaning |
|---|---|---|
| `ddl` | `due`, `deadline` | Deadline. |
| `sche` | `scheduled`, `start` | Scheduled/start date. |
| `prio` | `priority` | Priority `A` through `F`; missing priority sorts like `D`. |
| `repeat` | `rep`, `every` | Repeater: `+1w`, `++1w`, `.+3d`; bare `1w` means `+1w`. |
| `warn` | `lead` | Deadline warning lead time such as `3d`, `2w`, `1m`; default is 14 days. |
| `after` | `dep` | Dependency references, separated with `&`. |
| `done` | - | Most recent completion date, written by the engine. |
| `log` | - | Completion history, `&` separated and capped by the engine. |

Date values are parsed by the runtime, so ISO dates, relative forms such as
`+3d`, and supported natural/CJK forms work in both clients. Multi-word values
must be quoted. The command parser is quote-aware, so commas and semicolons
inside `"..."` or `'...'` are preserved.

When a line already uses an alias such as `due`, patches keep that alias. Newly
added metadata uses canonical keys.

## Dependencies

Dependencies intentionally use text references instead of hidden ids:

```md
@@todo [write conclusion] {after: "draft theorem & [[Lemma Note]]::check proof"}
```

Reference matching is runtime-computed:

- Without a note prefix, the reference matches another todo in the same file.
- With `[[Note Title]]::`, the title is matched against note title/aliases.
- Todo text matching tries exact text, then a unique prefix, then a unique
  substring.
- Broken or ambiguous references become lints. They do not block a task.
- Resolved dependencies block `todo`/`doing` tasks until the target todo is
  `done` or `cancelled`; no file write is needed to unlock them.

Clients should not hand-write dependency refs when a target todo is selected.
They call `todo-dep-ref`; the runtime returns the shortest stable text ref to
append to `after`.

## Repeating Tasks

Completing a todo through `patch-todo` with `op: "complete"` runs the server
completion engine.

- Without `repeat`, the status becomes `done` and `done:` is written.
- With `repeat`, `ddl` and/or `sche` roll forward, status resets to `todo`,
  `done:` is updated, and `log:` is appended.
- `+N` moves once from the old date, `++N` moves until future, and `.+N` moves
  from the completion date.

## View Model

The shared API is:

```json
{
  "type": "agenda",
  "range": {"from": "2026-07-06", "to": "2026-07-12", "today": "2026-07-06"},
  "days": [{"date": "2026-07-06", "entries": []}],
  "todos": [],
  "lints": [],
  "logByDay": {"2026-07-06": 2},
  "stats": {"open": 0, "doing": 0, "done": 0, "cancelled": 0, "blocked": 0, "overdue": 0}
}
```

Day `entries` refer to todos by `todoId`; clients index `todos` by id and only
render. The runtime owns dependency resolution, effective blocked status,
urgency, warning/overdue/scheduled carry-forward buckets, repeat completion,
and log aggregation.

API/actions:

- `agenda --json '{"from":"2026-07-06","days":7}'`
- `patch-todo --json '{...}'`
- `todo-dep-ref --json '{"targetId":"...","sourceId":"..."}'`
- `update-todo` remains as a compatibility wrapper.

## Web

The first-class Web agenda is the full-screen agenda view opened from
Aaronnote. It supports week/list/month/log views, priority/deadline/scheduled/
repeat edits, dependency selection, marks, bulk status changes, lints, and
source jumps. All writes call `notes.patchTodo`.

Inline todo widgets are display-only: status, priority badge, repeat marker,
and dependency pill are read from the parsed command and never become a second
source of truth.

## Emacs

Entrypoints:

- `M-x my/aaronnote-roam-agenda`
- `M-x my/aaronnote-roam-agenda-calendar`
- `M-x my/aaronnote-roam-agenda-log`
- Aaronnote dispatch: `A` agenda, `L` agenda log.

Agenda mode keys:

| Key | Action |
|---|---|
| `j`/`k`, arrows | Move line. |
| `RET`/`TAB` | Visit todo source. |
| `t` | Set status. |
| `p` or `,` | Set priority. |
| `d` | Set deadline. |
| `s` | Set scheduled date. |
| `r` | Set repeater. |
| `a` | Add dependency via runtime-generated ref. |
| `m` / `u` | Mark or unmark. |
| `B` | Bulk operation for marked todos. |
| `f` / `b` | Next/previous range. |
| `.` | Return to today. |
| `v` | Cycle week/list/calendar/log. |
| `/` | Search current agenda projection. |
| `g` | Refresh from runtime. |
| `q` / `Esc` | Close. |

The Emacs client calls `agenda --json` and renders the returned view-model.
Its local scan is only a last-resort fallback when the runtime is unavailable.
