#set document(title: "{{title}}", author: "{{author}}", date: datetime.today())
#set page(paper: "a4", margin: (x: 2.35cm, y: 2.45cm), numbering: "1", number-align: center)
#set text(font: "New Computer Modern", size: 11pt, lang: "en")
#set par(justify: true, leading: 0.65em)
#set heading(numbering: "1.1")

#let point-color = rgb("#7a2e2e")
#let response-color = rgb("#235f46")
#let new-color = rgb("#1d4ed8")
#let rule = rgb("#d6d3cb")

#let point(body) = block(width: 100%, stroke: (left: 2pt + point-color, rest: 0.45pt + rule), inset: 9pt, radius: 2pt)[
  #text(weight: "bold", fill: point-color)[Reviewer comment]
  #v(0.35em)
  #body
]

#let response(body) = block(width: 100%, stroke: (left: 2pt + response-color, rest: 0.45pt + rule), inset: 9pt, radius: 2pt)[
  #text(weight: "bold", fill: response-color)[Response]
  #v(0.35em)
  #body
]

#let new(body) = text(fill: new-color, body)

#align(center)[
  #text(size: 21pt, weight: "bold")[{{title}}]
  #v(0.3em)
  #text(size: 10.5pt)[{{author}} · {{date}}]
]

#v(1em)

= Opening
{{cursor}}

We thank the reviewers for their careful reading and constructive feedback.

= Change Summary
#figure(
  table(
    columns: 4,
    [Reviewer], [Concern], [Response], [Manuscript change],
    [], [], [], [],
  ),
  caption: [Response map],
  kind: table,
)

= Reviewer 1
== Comment R1.1
#point[

]

== Response R1.1
#response[

]

== Manuscript Change
#new[

]

= Reviewer 2
== Comment R2.1
#point[

]

== Response R2.1
#response[

]

== Manuscript Change
#new[

]

= Global Edits
-

= Remaining Risks
- TODO:
