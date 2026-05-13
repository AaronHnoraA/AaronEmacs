#set document(title: "{{title}}", author: "{{author}}", date: datetime.today())
#set page(paper: "a4", margin: (x: 2.35cm, y: 2.45cm), numbering: "1", number-align: center)
#set text(font: "New Computer Modern", size: 10.8pt, lang: "en")
#set par(justify: true, leading: 0.62em)
#set heading(numbering: "1.1")

#let accent = rgb("#2d5c88")
#let soft = rgb("#eef4fa")
#let rule = rgb("#cdd8e4")

#let protocol-box(title, body) = block(width: 100%, fill: soft, stroke: 0.55pt + rule, radius: 3pt, inset: 9pt)[
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

= Abstract
{{cursor}}

= Research Question
#protocol-box("Question")[

]

= Hypothesis
-

= Setup
#figure(
  table(
    columns: 3,
    [Component], [Version / Setting], [Reason],
    [], [], [],
  ),
  caption: [Experimental setup],
  kind: table,
)

= Protocol
1.
2.
3.

= Results
#figure(
  table(
    columns: 5,
    [Run], [Condition], [Metric], [Value], [Notes],
    [], [], [], [], [],
  ),
  caption: [Raw result summary],
  kind: table,
)

= Analysis
-

= Threats To Validity
-

= Conclusion
-

= Follow-Up
- TODO:
