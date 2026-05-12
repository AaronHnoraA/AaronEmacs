#import "/_typst/note.typ": *
#show: note-entry

#metadata((
  kind: "note",
  id: "{{timestamp}}-{{slug_title}}",
  title: "{{title}}",
  date: "{{date}}",
  tags: ("reviewer-response", "writing", "draft"),
  aliases: (),
)) <note>

= {{title}}

== Summary Of Changes
{{cursor}}

== Reviewer Map
#table(
  columns: 4,
  [Reviewer], [Comment], [Response], [Manuscript change],
  [], [], [], [],
)

== Response Drafts
=== Reviewer 1
-

=== Reviewer 2
-

== Remaining Risks
-
