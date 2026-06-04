#set document(title: "{{title}}", author: "{{author}}", date: datetime.today())
#set page(paper: "a4", margin: (x: 2.4cm, y: 2.5cm), numbering: "1", number-align: center)
#set text(font: "New Computer Modern", size: 11pt, lang: "en")
#set par(justify: true, leading: 0.65em)
#set heading(numbering: "1.1")

#let accent = rgb("#245f8f")
#let soft = rgb("#eef5fb")
#let rule = rgb("#cdd8e4")

#show heading.where(level: 1): it => {
  v(1em)
  text(size: 16pt, weight: "bold", fill: accent, it.body)
  v(0.25em)
  line(length: 100%, stroke: 0.9pt + accent)
  v(0.8em)
}

#let callout(title, body) = block(width: 100%, fill: soft, stroke: 0.6pt + rule, radius: 3pt, inset: 10pt)[
  #text(weight: "bold", fill: accent)[#title]
  #v(0.45em)
  #body
]

#align(center)[
  #text(size: 22pt, weight: "bold", fill: accent)[{{title}}]
  #v(0.35em)
  #text(size: 10.5pt)[{{author}} · {{date}}]
]

#v(1em)

= Executive Summary
{{cursor}}

#callout("Summary")[
  State the main result, current status, and the decision or feedback needed.
]

= Progress
- Completed:
- In progress:
- Blocked:

= Technical Work
== Method
-

== Implementation
-

== Evaluation
-

= Results
#figure(
  table(
    columns: 4,
    [Metric], [Baseline], [Current], [Notes],
    [], [], [], [],
  ),
  caption: [Key measurements],
  kind: table,
)

= Issues And Mitigations
#figure(
  table(
    columns: 3,
    [Issue], [Impact], [Mitigation],
    [], [], [],
  ),
  caption: [Open issues],
  kind: table,
)

= Next Steps
- TODO:

= References
-
