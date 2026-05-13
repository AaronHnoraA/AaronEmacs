#set document(title: "{{title}}", author: "{{author}}", date: datetime.today())
#set page(paper: "a4", margin: (x: 2.35cm, y: 2.45cm), numbering: "1", number-align: center)
#set text(font: "New Computer Modern", size: 10.8pt, lang: "en")
#set par(justify: true, leading: 0.62em)
#set heading(numbering: "1.1")

#let accent = rgb("#315f45")
#let soft = rgb("#eef6f1")
#let rule = rgb("#c8dccf")

#show heading.where(level: 1): it => {
  v(1em)
  text(size: 15.5pt, weight: "bold", fill: accent, it.body)
  v(0.25em)
  line(length: 100%, stroke: 0.8pt + accent)
  v(0.7em)
}

#let proposal-box(title, body) = block(width: 100%, fill: soft, stroke: 0.55pt + rule, radius: 3pt, inset: 9pt)[
  #text(weight: "bold", fill: accent)[#title]
  #v(0.4em)
  #body
]

#align(center)[
  #text(size: 22pt, weight: "bold", fill: accent)[{{title}}]
  #v(0.35em)
  #text(size: 10.5pt)[{{author}} · {{date}}]
]

#v(1em)

= Project Summary
{{cursor}}

#proposal-box("One paragraph pitch")[
  State the problem, the proposed contribution, why now, and why this team.
]

= Specific Aims
1.
2.
3.

= Significance
-

= Innovation
-

= Approach
== Work Package 1
- Objective:
- Method:
- Deliverable:

== Work Package 2
- Objective:
- Method:
- Deliverable:

= Timeline
#figure(
  table(
    columns: 4,
    [Milestone], [Month], [Output], [Risk],
    [], [], [], [],
  ),
  caption: [Milestones],
  kind: table,
)

= Budget Justification
#figure(
  table(
    columns: 3,
    [Item], [Amount], [Justification],
    [], [], [],
  ),
  caption: [Budget draft],
  kind: table,
)

= Broader Impacts
-

= Submission Checklist
- TODO:
