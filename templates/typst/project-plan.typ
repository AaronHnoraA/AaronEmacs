#import "/_typst/note.typ": *
#show: note-entry

#metadata((
  kind: "note",
  id: "{{timestamp}}-{{slug_title}}",
  title: "{{title}}",
  date: "{{date}}",
  tags: ("project", "plan", "draft"),
  aliases: (),
)) <note>

= {{title}}

== Outcome
{{cursor}}

== Why
- User / audience:
- Problem:
- Success means:

== Scope
=== In
-

=== Out
-

== Milestones
#table(
  columns: 3,
  [Milestone], [Done when], [Date],
  [], [], [],
)

== Risks
#table(
  columns: 2,
  [Risk], [Mitigation],
  [], [],
)

== Tasks
- #todo[]

== Notes
