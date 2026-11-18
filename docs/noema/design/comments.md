# Comments (C70, C80)

Select text in a note and comment on it. The text is highlighted and the
comment is a task: Emacs's collaboration panel (`H-t`) hands it to an agent,
which changes the file through Noema's API and marks the comment done. The page
has no comment rail: a comment's card pops up above its text. Code:
`shared/comment-anchors.mjs`, `server/lib/note-comments.mjs`,
`server/Features/Comments/api.mjs`, `pages/comments.ts`, and on the Emacs side
`lisp/tools/agent/collab.el` and `lisp/tools/agent/panel.el`.

Side comments (`@@scomment`) are a different thing: notes for the reader,
shown beside the text. They are not tasks.

## Source and storage

- **Anchor in the note.** `{==commented text==}{>>#c-ID<<}` (CriticMarkup's
  highlight plus a comment mark that holds only the comment's id). One line,
  no nesting. The editor hides the markers and marks the text with
  `.noema-comment-anchor[data-comment-id]`. The commented text may hold math,
  which keeps rendering inside the highlight; only the markers must lie outside
  code and math. A marker shows only while the caret is inside the marker
  itself. HTML rendering and the Wiki index keep only the text.
- **Task data beside the note.** Each folder has one hidden
  `.noema-comments.jsonl`, an append-only journal of the comments of the notes
  in that folder: `thread` (quote, author, text), `assign` (the agent it was
  handed to), `resolve`, `reopen`, `edit`, `delete`, `move`. A comment is
  folded from its events on read. The Noema host and Emacs both only append.
  There are no replies: the work is the change to the file.
- **Cache.** The host keeps each folder's fold in memory, keyed by the
  journal's size and mtime. Every write announces `comments-changed` to open
  pages and Emacs; nothing polls.
- **Moves.** A rename through Noema appends `move` events; a move to another
  folder carries the comment's events into that folder's journal.

## Interaction

- Select text within one line and press `⌘⌥M`, or Comment on the selection
  toolbar. A draft card opens above the selection; Enter stores the comment
  first and then wraps the text in its anchor, so an anchor never points
  nowhere. Shift-Enter breaks a line, Escape cancels.
- The caret in a highlight, or the pointer resting on it, pops the card up
  above the text (below it when there is no room). The card shows the author,
  the time, the comment, and who it was handed to; ✓ marks it done (↺
  reopens), ⋮ edits or deletes it. Leaving the highlight closes the card.
- A done comment keeps a faint dashed underline; the shown one is highlighted
  more strongly.

## Agents

Handing a comment to an agent (the panel's `a`) sends one prompt: the request,
the comment id, the file, the line and the quote. The first task of a native
session also carries Noema's tools guide. The agent changes the document with
`agent:document:edit` (or `propose`, when the person should judge) and marks
the comment done with `agent:document:comment {file, id}`; `resolve: false`
reopens it. It may also comment on text it names (`{file, from, to, oldText,
text}`) to ask the person something. Emacs collects nothing from the chat.

## Agenda (C77)

Open comments are tasks of the note's Agenda scope, tagged `comment`, at their
anchor's line. A comment may end in DSL attributes (`Prove it {ddl: fri; prio:
A}`). Done resolves it, todo reopens it.

## Tests

`C70 *` and `C80 *` in `tests/note-comments.test.ts`,
`tests/comment-anchor-editor.test.ts` and `tests/comment-popover.test.ts`;
`C77 *` in `tests/agenda-comments.test.ts`; `agent-collab-c70-*` and
`agent-collab-c73-*` in Emacs; `scripts/check-comments.mjs` in WebKit (no
rail, the card above on caret and hover, math in a highlight, a draft wraps
the selection).
