# Comments (C79)

A comment is a side comment of the note, and a comment is a task. The page
has no comment rail, threads or replies: tasks are listed and handed to agents
in Emacs's collaboration panel (`H-t`), and an agent does a task by changing
the file through Noema's API. Code: `shared/scomment.mjs` (the one parser,
pinned by `shared/scomment-fixtures.json`), `pages/comments.ts`,
`server/Features/Agent/api.mjs`, `server/lib/agenda-comments.mjs`, and on the
Emacs side `lisp/tools/agent/collab.el` and `lisp/tools/agent/panel.el`.

## Source

```text
… the passage it is about @@scomment [the comment] {id: k3x9qa}
… @@scomment(done) [the comment] {id: k3x9qa}
```

- The comment is the ordinary command grammar (`@@name(switch) [body] {key:
  value; …}`, [syntax.md](syntax.md)). The body is the comment, written right
  after the passage it is about. `(done)` uses the planning word for a
  finished task. `id` is six base36 characters, like a planning id.
- A side comment without an id is still a comment, as it always was. It is
  given an id the first time it becomes a task (handed to an agent, or marked
  done). Adding the id, or marking a comment done or open again, changes only
  the head and the attributes. The comment's text stays exactly as written.
- The editor draws a side comment as its card beside the text. A finished one
  stays, faded and ticked. HTML and LaTeX export are unchanged: a done
  comment exports like any side comment.

## Tasks

- The folder's hidden `.noema-tasks.jsonl` records only who took a comment:
  `{"v":1,"op":"assign","id","note","name","session","at"}`, one line per
  handover. A file that cannot hold a side comment (code) keeps its task in
  the same journal: `{"op":"task","id","note","text","line","quote"}`; a
  later `{"op":"task","id","done":true}` finishes it.
- Handing a comment to an agent sends one prompt: the comment, its id, the
  file, the line and the passage before it. The first task of a native
  session also carries Noema's tools guide. The agent changes the file with
  `agent:document:edit` (or `propose`, when the person should judge) and then
  marks the comment done with `agent:document:comment {file, id}`. Emacs
  collects nothing from the chat; the person reads it when they want to.
- `agent:document:comment` also adds a comment after a unique passage
  (`{file, after, text}`), for a question to the person.

## Interaction

- In a page, select the passage and press `⌘⌥M` (or Comment on the selection
  toolbar). One input opens at the selection; Enter writes
  ` @@scomment [text] {id: …}` right after the passage as one undoable edit,
  Shift-Enter adds a line, Escape cancels. In a plain buffer, `C-c A d` does
  the same for the region or the line.
- A send that names a place of a document (C76) becomes a comment task there,
  handed to the chosen session at once.
- Every change to a note — resolving, adding an id, a comment from a send —
  goes through `agent:document:edit`, so an open page follows; without a host
  Emacs edits the file or its buffer.
- Emacs hears that a saved note's comments changed by comparing their heads
  on the `saved` event; agents' comment writes announce `comments-changed`.
  Nothing polls.

## Agenda (C77)

Open side comments are tasks of the note's Agenda scope, tagged `comment`, at
their line. A comment may end in DSL attributes (`Prove it {ddl: fri; prio:
A}`), so it is searched, filtered and scheduled like an `@@todo`. Marking it
done writes `@@scomment(done)`; setting it back to todo removes `(done)`.
Other patches, clocks and dependencies are refused.

## Tests

`C79 *` in `tests/scomment.test.ts`, `tests/scomment-input.test.ts` and
`tests/agent-api.test.ts`; `C77 *` in `tests/agenda-comments.test.ts`;
`agent-collab-c79-*`, `agent-collab-c76-*` and `agent-panel-c79-*` in Emacs.
