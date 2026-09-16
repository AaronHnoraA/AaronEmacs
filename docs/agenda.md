# Noema Agenda

Noema Agenda provides native Emacs and Emacs-hosted Web views over one
task/project/time-tracking service. Markdown commands and `.noema` WorkNode
metadata remain the native sources; edits write back with revision checks.
Org Agenda supplies presentation primitives without intermediate Org sources.

For the full DSL grammar (canonical keys/aliases, date/repeater/duration
syntax, diagnostics) and view-model shapes, see
[`site-lisp/noema/docs/agenda.md`](../site-lisp/noema/docs/agenda.md).
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
`"..."` or `'...'` are preserved. Imported ISO timestamps with `Z` or a
numeric offset are treated as local planning wall time, not as instants, so
`2026-07-07T23:30:00Z` stays in the July 7 agenda bucket.

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
otherwise to the shortest unique text ref. Concurrent mints reserve ids in
process before writing, so simultaneous dependency/clock actions do not
receive the same fresh id.

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
Malformed clock data is reported in lints without hiding time: multiple open
clocks, reversed spans (`to < from`, counted as zero), and overlapping spans
all remain visible in the agenda.

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

## Writes and Sync

Agenda writes and browser/editor saves share a per-file queue. The server
serializes the whole read/locate/write cycle for `patch-todo`, `create-todo`,
id minting, and clock in/out, so a UI patch cannot interleave with a save and
silently overwrite it. If an editor save uses an old `baseMtimeMs` after an
agenda write, the save returns `conflict: true` and the client should reload
or review before forcing an overwrite.

Agenda writes enqueue changed files for Roam DB sync but do not start an
automatic git commit or background DB rebuild. The queue is intentionally
drained by the next explicit sync/full rebuild command.

## Web

The Emacs-hosted Web agenda page (`/agenda`) provides week/list/month/log/
gantt/projects/clocktable/lints tabs, priority/deadline/scheduled/repeat
edits, template capture, dependency selection, clock in/out, marks, bulk
status changes, lints, Gantt drag-to-reschedule, and source jumps. Capture
shows the template, active scope, Markdown destination and prompted fields.
Failed writes keep the form open with its inputs and error; successful writes
close it. All writes call `notes.createTodo`/`notes.patchTodo`/`clockIn`/`clockOut`.

Opening Web Agenda from Emacs sends an explicit scope list: the resident Roam
knowledge vault (`~/Documents/Noema`) plus the one project entered through the
project lifecycle, when present. It never expands that request to every active
or historical lease. Opening `/agenda` directly defaults to the knowledge scope
only. With no project filter, the DAG tab projects only that one entered
project. Selecting a project in the header is a hard graph boundary across the
explicit scopes: unselected nodes and edges are removed before layout and DOM
construction rather than merely dimmed.

Inline todo widgets in the editor are display-only: status, priority badge,
repeat marker, and dependency pill are read from the parsed command and never
become a second source of truth.

The editor also completes `after:`/`blocks:`/`task:` values (same popup
mechanism as tag/roam/path completion), backed by `completions:todo-refs`.

## Emacs

`M-x my/noema-agenda` opens the native Agenda. It shares the active source scopes
and writers with the hosted Web view. Native keys include `v d/w/t` for views,
`c` capture, `t` complete, `s/d` schedule/deadline, `%` progress, `I/O` clock,
`v k` clock report, `R` retry deferred clock writes, and `K` keep saved source
state when resolving a pending clock request.

The default native view contains both the seven-day calendar and the open-task
section, so unscheduled Roam tasks remain visible even when the current week has
no dated entries. The Dashboard renders its last complete scoped Agenda snapshot
synchronously in the normal Dashboard build. Source events only mark that cache
dirty and refresh it while the Dashboard is visible; concurrent refreshes share
one request, there is no polling or partial post-render card rewrite, and
Chunlian is attached after the complete render.

The knowledge vault remains indexed. Explicit project navigation activates
one project; switching away, closing its Remote workspace, or running
`my/project-leave` releases it. Inactive projects are not scanned. Persisted
clock references survive exit and host restart; pending source writes wait for
re-entry. File notifications invalidate the index; there is no polling.

Hidden paths such as `.lake/` and standard dependency/build directories are
excluded before traversal. Additional relative glob exclusions use
`my/noema-agenda-exclude-patterns`, available through `M-x config-board` or
`config-set`; restart the host after changing them. For example,
`(config-set 'my/noema-agenda-exclude-patterns '("archive/**" "scratch.md"))`
excludes an archive directory and a specific file relative to each active root.

### Shared capture templates

Native `c` and Web New todo use the same host catalogue: Task, Deadline and
Appointment. `C-u c` chooses a different destination file. The default is
`inbox.md` in the selected active scope. Appointments require a scheduled start
and a later end. The native date prompt uses Org's calendar picker; source
storage remains native Markdown. Failed native captures retain their draft in
the Agenda buffer; press `c` again to review and retry. Drafts are not saved to
disk and do not survive closing the Agenda buffer.

Customize `my/noema-agenda-capture-templates` through `config-board` or
`config-set`, then restart the Noema host. Nil restores the built-ins; a custom
list replaces them. For example:

```elisp
(config-set 'my/noema-agenda-capture-templates
 '(((id . "review") (key . "r") (name . "Review")
    (file . "daily/%Y-%m-%d.md") (scope . "selected")
    (fields . ["text" "ddl" "prio"])
    (required . ["ddl"])
    (defaults . ((prio . "A") (effort . "30m") (repeat . "+1w"))))))
```

`scope` is `selected` or `knowledge`; templates never activate projects.
`fields` must include `text`; other fields are `status`, `sche`, `ddl`, `end`,
`prio`, `effort`, `repeat`, `project`, `tags`, and `warn`. `defaults` can include
unprompted fields. Paths support `%Y`, `%m`, `%d` using the host's local date.
Catalogue revisions reject a stale form after a template change or date-path
rollover. Reopen capture to load the current catalogue; native drafts prefill
the new prompts. Exclusions, scope containment and modified-buffer protection
still apply. Templates are data, with no executable hooks or Org conversion.
Headless hosts accept the same profile array as JSON in
`NOEMA_AGENDA_CAPTURE_TEMPLATES`.

Native `RET` resolves live Markdown tasks from the current editor snapshot,
including unsaved edits, before navigating. Code examples are excluded.
Ambiguous duplicate source text after edits requires stable task IDs. Switching
away or starting a newer visit suppresses a delayed navigation response.

Project identity and client-host placement use the Remote framework. Sources
without client placement use a workspace-owned target process for discovery,
versioned reads/writes and file notifications. Leaving the scope closes that
process; gateway loss clears stale tasks and workspace recovery restores only
active sources. The real gateway path is tested with a logical local target;
live SSH parity still needs verification.

### Apple 集成进度

EventKit helper 已提供 macOS 14+ 的原生协议与 Emacs 客户端进程生命周期。
`make agenda-apple-test` 构建并验证辅助进程、日期和资源释放；测试不请求权限，
不读写个人提醒或日历。开发时可运行 `my/noema-agenda-apple-enable` 显式申请
Reminders 或 Calendar 权限，`my/noema-agenda-apple-disable` 释放辅助进程。

原生 Agenda 的 `P` 显式选择 Reminders 或 Calendar 及目标列表，`v a` 或
`M-x my/noema-agenda-attention` 打开全局关注；Web 提供 Promote 和 Global
attention。只有显式提升才创建 Apple 条目。Calendar 需要 `s` 开始、`E` 结束，
deadline 不自动变成日历占用。启用 helper 不会提升现有任务。

全局关注的 `g` 只读持久绑定，`R` 同步/重试选中条目，`s/a` 选择保留源/使用
Apple，`RET` 明确进入源项目。`d` 删除绑定的 Apple 条目并保留源，`F` 仅解除
绑定，两边都保留。项目未进入时只保存轻量回执；进入后才检查源版本及未保存
buffer 并回写。冲突保留双方；连接不可用时标明正在展示已保存回执。

已用临时 Apple 协议替身验证真实 host、Go 和 Emacs 网关闭环，以及重启/退出/
回写冲突。尚未读写个人 EventKit 条目或验证设备同步；跨时区改动、重复小时和
一个任务在同一日历的多个时间块仍有待完成。

### Roam 入口

- `M-x my/noema-roam-agenda` — native agenda (dispatch key `A`).
- `M-x my/noema-roam-agenda-web` — hosted Web agenda.
- `M-x my/noema-roam-agenda-calendar` — month calendar view.
- `M-x my/noema-roam-agenda-log` — completion log view.
- `M-x my/noema-roam-agenda-gantt` — Gantt view.
- `M-x my/noema-roam-agenda-projects` — project rollup view.
- `M-x my/noema-roam-agenda-clock` — clocktable view.
- `M-x my/noema-roam-agenda-lints` — lint view.
- `M-x my/noema-roam-agenda-search` — agenda with a search query.
- `M-x my/noema-roam-jump-file-todo` — jump to a todo in the current file
  (dispatch key `F`); this one stays local to Emacs (a `completing-read` over
  the current buffer's todos), not a web redirect.
- Full Roam DB rebuild is dispatch key `Z`; `F` is reserved for current-file
  todos across the roam dispatchers.

`after:`/`blocks:`/`task:` values also complete locally, via
`my/noema-roam-capf` (a `completion-at-point-functions` entry, so it
works through `company`/the built-in completion UI) calling the same
`todo-refs` backend service the Web editor uses.


### Markdown examples

Fenced/indented code, inline code, escaped commands and metadata summaries are
excluded from native task and clock discovery. A task moved into code cannot
be changed using an old Agenda selection. Capture into an unclosed code fence
is rejected before saving: close the fence or select another inbox file.
Tasks in ordinary proof-environment prose remain supported, and inline code
inside a task title is preserved verbatim.


### Roam task entry points

`my/noema-roam-todos` retains the Roam task list and reads native records from
Knowledge plus explicitly active projects. Empty results do not launch a CLI
or trigger a regex scan. Completion, metadata and dependency edits use the
same scope, revision and modified-buffer checks as Noema Agenda; source errors
cannot fall through to a second local writer.

`my/noema-roam-jump-file-todo` (`F`) asynchronously parses the current Markdown
buffer, including unsaved edits. This read-only snapshot does not scan other
files, enter a project or publish unsaved tasks to the index. Requests superseded
by another invocation, buffer edits or buffer closure are discarded. Switching
away suppresses the late selection prompt. Source jumps retain exact positions
through Unicode and repeated identical task text. Plain `TODO` prose and ordinary
checkboxes do not create native tasks; use `@@todo` / `@@itodo`.
