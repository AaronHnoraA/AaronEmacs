#set document(title: "{{title}}", author: "{{author}}", date: datetime.today())
#set page(
  paper: "a4",
  margin: (inside: 2.8cm, outside: 2.2cm, y: 2.6cm),
  numbering: "1",
  number-align: center,
  header: context {
    if counter(page).get().first() > 1 {
      align(center, text(size: 9pt, fill: gray)[{{title}}])
      line(length: 100%, stroke: 0.45pt + gray.lighten(45%))
    }
  },
)
#set text(font: "New Computer Modern", size: 11pt, lang: "en")
#set par(justify: true, leading: 0.68em)
#set heading(numbering: "1.1")
#set math.equation(numbering: "(1)")

#let accent = rgb("#2f6f62")
#let theorem(body) = block(width: 100%, stroke: (left: 2pt + accent, rest: 0.45pt + gray.lighten(60%)), inset: 10pt, radius: 2pt)[
  #text(weight: "bold", fill: accent)[Theorem]
  #v(0.35em)
  #body
]
#let definition(body) = block(width: 100%, fill: rgb("#eef7f4"), stroke: 0.45pt + rgb("#c5ddd5"), inset: 10pt, radius: 2pt)[
  #text(weight: "bold", fill: accent)[Definition]
  #v(0.35em)
  #body
]

#align(center)[
  #v(1.2cm)
  #text(size: 26pt, weight: "bold", fill: accent)[{{title}}]
  #v(0.5cm)
  #text(size: 12pt)[{{author}}]
  #v(0.35cm)
  #text(size: 10pt)[{{date}}]
]

#pagebreak()
#outline(title: [Contents], depth: 2)
#pagebreak()

= Preface
{{cursor}}

= Reading Contract
- Audience:
- Prerequisites:
- Main promise:
- What this text will not cover:

= Chapter Map
#figure(
  table(
    columns: 4,
    [Chapter], [Purpose], [Key objects], [Status],
    [1], [], [], [],
    [2], [], [], [],
    [3], [], [], [],
  ),
  caption: [Longform outline],
  kind: table,
)

= Notation
-

= Chapter 1
== Goal
-

== Definitions
#definition[

]

== Main Result
#theorem[

]

== Proof Strategy
-

== Examples
-

= Appendix Plan
-
