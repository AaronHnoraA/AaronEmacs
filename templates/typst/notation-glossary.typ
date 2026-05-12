#import "/_typst/note.typ": *
#show: note-entry

#metadata((
  kind: "note",
  id: "{{timestamp}}-{{slug_title}}",
  title: "{{title}}",
  date: "{{date}}",
  tags: ("notation", "glossary", "draft"),
  aliases: (),
)) <note>

= {{title}}

== Glossary
{{cursor}}

#table(
  columns: 3,
  [Notation], [Meaning], [Scope],
  [], [], [],
)

== Conflicts
-

== Sources
-
