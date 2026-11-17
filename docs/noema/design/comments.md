# Comments (C70)

Noema pages take comments the way Google Docs does: you select text, add a
comment, and it sits in a rail beside the text. The note's collaborator (an
agent bound to the file, C65) answers in the same card. This document is the
contract; the code is `shared/comment-anchors.mjs`,
`server/lib/note-comments.mjs`, `server/Features/Comments/api.mjs`,
`pages/comments.ts` and, on the Emacs side, `lisp/tools/agent/collab.el`.

## Source and storage

- **Anchor in the note.** `{==commented text==}{>>#c-ID<<}` (CriticMarkup's
  highlight plus a comment mark that holds only the thread id). One line, no
  nesting. The editor hides the markers and marks the text with
  `.noema-comment-anchor[data-comment-id]`. A marker shows only while the
  caret is inside the marker itself, so editing the commented text never
  reveals syntax. HTML rendering and the Wiki index keep only the text. The
  LaTeX export is left as it is.
- **Threads beside the note.** Each folder has one hidden
  `.noema-comments.jsonl`, an append-only journal of the threads of the notes
  in that folder. One event per line: `thread`, `reply`, `resolve`, `reopen`,
  `edit`, `delete`, `assign`, `move`. A thread is folded from its events on
  read. The Noema host (page actions) and Emacs (agent replies, `C-c A d`)
  both only append, so neither loses the other's work. A note does not need
  a Project for this.
- **Cache.** The host keeps each folder's fold in memory, keyed by the
  journal's size and mtime, and reads the file again only when it changed.
  Every write announces `comments-changed` to open pages; nothing polls. When
  Emacs writes to the journal, it calls `comments:changed` for the same
  announcement.
- **Moves.** A rename through Noema appends `move` events. A move to another
  folder carries the thread's events into that folder's journal and deletes
  them from the old one. Because the anchor carries the id, a thread can be
  matched to its note again.

## Interaction

- Select text, then press the rail's "+" beside the selection, the selection
  toolbar's Comment button, or `⌘⌥M` (`H-M-m`, forwarded to Emacs and back as
  the page command `add-comment`). A draft card opens level with the text.
  Submitting creates the thread first and then wraps the text in its anchor,
  so an anchor never points nowhere. The caret then rests after the text.
- Cards sit level with their text and never overlap. The active card keeps
  its place and is shifted out with its reply box open; cards pushed above
  the page top are laid out downward instead. Clicking the highlighted text
  activates its card; clicking a card activates its text.
- A card shows the quoted text, then each message with avatar, name and time.
  Its header has ✓ Resolve and ⋮ Edit / Delete. A reply of your own can be
  deleted. Inactive long threads fold to the first and last message.
  "*name* is replying…" shows while the assignee owes an answer.
- A resolved thread leaves the rail and its highlight fades. "Resolved (N)"
  brings resolved threads back with Reopen. A thread whose text was deleted
  stays, at the end of the rail, with its quote struck through.
- With comments the note narrows to give the rail its column. The paragraph
  layout re-measures for the new width.

## The collaborator answers

A person's comment or reply is handed by the host to Emacs (`noema.event`
`comment-asked`), unless the API call carries `ask: false`. Emacs asks the
thread's assignee, or else the note's most recent live collaborator. It
records it as assignee (`assign`) and sends the question with the quote, the
file and the line through the collaborator's thread turn queue (C67). The
reply of that turn, read from the ACP stream (`+agent-acp-turn-reply`), is
appended to the thread as the agent's message. Without a live collaborator,
the comment stays for the person. The agent's own logic does not change: it
only receives a prompt and answers.

## Tests

`C70 *` in `tests/note-comments.test.ts`, `tests/comment-anchor-editor.test.ts`
and `tests/comment-rail.test.ts`; `agent-collab-c70-*` in Emacs;
`scripts/check-comments.mjs` in WebKit (the card level with its text, active
on click, a new comment wraps the selection, the text clear of the rail).

## Not yet

- @-mentioning a particular collaborator (the assignee is the note's
  collaborator).
- Inline suggestions in the Google style. Proposals remain `ediff` cards.
- Collaborator chips in the page header.
- Comments in plain Emacs buffers are threads without an in-text anchor.
