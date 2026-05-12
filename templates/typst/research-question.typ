#import "/_typst/note.typ": *
#show: note-entry

#metadata((
  kind: "note",
  id: "{{timestamp}}-{{slug_title}}",
  title: "{{title}}",
  date: "{{date}}",
  tags: ("question", "research", "draft"),
  aliases: (),
)) <note>

= {{title}}

== Question
{{cursor}}

== Motivation
- Why this matters:
- Nearby work:

== Known
-

== Unknown
-

== Possible Routes
#table(
  columns: 3,
  [Route], [Why plausible], [Blocker],
  [], [], [],
)

== Reading List
- #todo[]

== Experiments / Proof Attempts
- #todo[]

== Status
Open / parked / solved:
