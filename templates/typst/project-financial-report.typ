#set document(title: "{{title}}", author: "{{author}}", date: datetime.today())
#set page(paper: "a4", margin: (x: 2.2cm, y: 2.4cm), numbering: "1", number-align: center)
#set text(font: "New Computer Modern", size: 10.5pt, lang: "en")
#set par(justify: true, leading: 0.6em)
#set heading(numbering: "1.1")

#let primary = rgb("#1f4e79")
#let accent = rgb("#5d8f3f")
#let warning = rgb("#a3332d")
#let soft = rgb("#f3f7fa")
#let border = rgb("#d9e2ec")
#let currency = "USD"

#let money(value) = currency + " " + str(value)
#let section-card(title, body) = block(width: 100%, inset: 10pt, stroke: 0.7pt + border, radius: 3pt, fill: soft)[
  #text(weight: "bold", fill: primary)[#title]
  #v(0.45em)
  #body
]

#show heading.where(level: 1): it => {
  v(1em)
  text(size: 16pt, weight: "bold", fill: primary, it.body)
  v(0.2em)
  line(length: 100%, stroke: 1pt + primary)
  v(0.7em)
}

#align(center)[
  #text(size: 23pt, weight: "bold", fill: primary)[{{title}}]
  #v(0.35em)
  #text(size: 11pt)[Prepared by {{author}} · {{date}}]
]

#v(1em)

= Executive Summary
{{cursor}}

#grid(
  columns: (1fr, 1fr, 1fr),
  gutter: 0.8em,
  section-card("Budget", [#text(size: 16pt, weight: "bold")[#money(0)]\ Baseline]),
  section-card("Actual", [#text(size: 16pt, weight: "bold")[#money(0)]\ Posted spend]),
  section-card("Remaining", [#text(size: 16pt, weight: "bold", fill: accent)[#money(0)]\ Available]),
)

= Budget Snapshot
#figure(
  table(
    columns: (1.6fr, 1fr, 1fr, 1fr, 1.6fr),
    [Category], [Budget], [Actual], [Variance], [Status],
    [Personnel], [], [], [], [],
    [Contractors], [], [], [], [],
    [Cloud / Tools], [], [], [], [],
    [Travel], [], [], [], [],
    [Procurement], [], [], [], [],
    [*Total*], [], [], [], [],
  ),
  caption: [Budget versus actuals],
  kind: table,
)

= Key Drivers
1.
2.
3.

= Forecast
#figure(
  table(
    columns: 4,
    [Category], [Next period], [Quarter], [Assumption],
    [], [], [], [],
  ),
  caption: [Forecast assumptions],
  kind: table,
)

= Risks
#figure(
  table(
    columns: 4,
    [Risk], [Impact], [Mitigation], [Owner],
    [], [], [], [],
  ),
  caption: [Financial risks],
  kind: table,
)

= Decisions Needed
- TODO:
