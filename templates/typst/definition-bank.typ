#import "/_typst/note.typ": *
#show: note-entry

#metadata((
  kind: "note",
  id: "{{timestamp}}-{{slug_title}}",
  title: "{{title}}",
  date: "{{date}}",
  tags: ("definition", "bank", "draft"),
  aliases: (),
)) <note>

= {{title}}

== Definitions
{{cursor}}

#definition[

]

== Notation
#table(
  columns: 2,
  [Symbol], [Meaning],
  [], [],
)

== Examples
-

== Sources
-
