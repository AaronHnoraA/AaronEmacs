# Comments (C70, C80)

Select text in a note and use the toolbar's Comment action. The text is
highlighted and the comment appears in Emacs's collaboration panel (`H-t`).
Hand it to an agent there to have it change the file through Noema's API and
mark the comment done. The page
has no comment rail: a comment's card pops up above its text. Code:
`shared/comment-anchors.mjs`, `server/lib/note-comments.mjs`,
`server/Features/Comments/api.mjs`, `pages/comments.ts`, and on the Emacs side
`lisp/tools/agent/collab.el` and `lisp/tools/agent/panel.el`.

Side comments (`@@scomment`) appear beside the text. Private comments
(`@@comment`) appear in the editor. Both live in Markdown source without
a collaboration record. Questions an agent leaves on text stay on the page.

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
  in that folder: `thread` (quote, author, text), `assign` (the agent and
  dispatch attempt), `fail`, `archive`, `resolve`, `reopen`, `edit`, `delete`,
  `move`. A comment is
  folded from its events on read. The Noema host and Emacs both only append.
  There are no replies: the work is the change to the file.
- **Identity.** A lookup or mutation requires the owning note as well as its
  comment id. A sibling note cannot read or change the thread by id. New ids
  stay unique across the folder's shared journal.
- **Cache.** The host keeps each folder's fold in memory, keyed by the
  journal's size and mtime. Every write announces `comments-changed` to open
  pages and Emacs; nothing polls.
- **Migration helper.** `moveThreads` appends rename events or carries a note's
  events to another folder's journal. Destination id collisions are refused
  before either journal is changed. This is a preflight check, not a transaction
  across folders or external writers. The helper is not yet wired into ordinary
  file moves, so automatic comment migration on moving a note is not provided.

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
- **Clear finished** in Tasks (`C`) archives Done and Failed tasks in the
  current scope and filter. The journal and source anchors remain available.
  Cleared comments show **Cleared** on the page. **Reopen** returns a cleared
  selection comment to To do.

## Agents

Handing a comment to an agent (the panel's `a`) sends one prompt: the request,
the comment id, the file, the line and the quote. The first task of a native
session also carries Noema's tools guide. The agent changes the document with
`agent:document:edit` (or `propose`, when the person should judge) and marks
the comment done with `agent:document:comment {file, id}`; `resolve: false`
reopens it. It may also comment on text it names (`{file, from, to, oldText,
text}`) to ask the person something. Emacs collects nothing from the chat.
The terminal ACP receipt records a task failure for its own dispatch attempt.

Quotes of up to 1200 characters travel complete, including whitespace and CJK
text. Longer quotes are explicitly labelled excerpts requiring
`agent:document:read` before editing. A quote may have become stale; `oldText`
still guards the current source. Agents preserve `{==text==}{>>#id<<}` and edit
the inner text. Translation, polishing and shortening requests authorize direct
editing; explicit review requests and additional changes requiring judgement
use proposals.

## Agenda (C77)

Open, unarchived selection comments are tasks of the note's Agenda scope, tagged `comment`, at their
anchor's line. A comment may end in DSL attributes (`Prove it {ddl: fri; prio:
A}`). Done resolves it, todo reopens it.

## Tests

`C70 *` and `C80 *` in `tests/note-comments.test.ts`,
`tests/comment-anchor-editor.test.ts` and `tests/comment-popover.test.ts`;
`C77 *` in `tests/agenda-comments.test.ts`; `agent-collab-c70-*` and
`agent-collab-c73-*` in Emacs; `scripts/check-comments.mjs` in WebKit (no
rail, the card above on caret and hover, math in a highlight, a draft wraps
the selection).
