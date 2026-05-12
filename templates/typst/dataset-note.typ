#import "/_typst/note.typ": *
#show: note-entry

#metadata((
  kind: "note",
  id: "{{timestamp}}-{{slug_title}}",
  title: "{{title}}",
  date: "{{date}}",
  tags: ("dataset", "research", "draft"),
  aliases: (),
)) <note>

= {{title}}

== Source
{{cursor}}

== Schema
#table(
  columns: 3,
  [Field], [Type], [Meaning],
  [], [], [],
)

== Access
- Location:
- License:
- Refresh cadence:

== Quality Notes
-

== Useful Queries
-
