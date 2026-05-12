#import "/_typst/note.typ": *
#show: note-entry

#metadata((
  kind: "note",
  id: "{{timestamp}}-{{slug_title}}",
  title: "{{title}}",
  date: "{{date}}",
  tags: ("decision", "record", "draft"),
  aliases: (),
)) <note>

= {{title}}

== Decision
{{cursor}}

== Context
-

== Options
#table(
  columns: 3,
  [Option], [Pros], [Cons],
  [], [], [],
)

== Consequences
- Positive:
- Negative:
- Follow-up:

== Revisit
-
