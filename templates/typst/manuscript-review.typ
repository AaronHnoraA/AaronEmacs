#set document(title: "{{title}}", author: "{{author}}", date: datetime.today())
#set page(paper: "a4", margin: (x: 2.35cm, y: 2.45cm), numbering: "1", number-align: center)
#set text(font: "New Computer Modern", size: 10.8pt, lang: "en")
#set par(justify: true, leading: 0.62em)
#set heading(numbering: "1.1")

#let accent = rgb("#6a3d9a")
#let soft = rgb("#f3eef8")
#let rule = rgb("#d7c7e8")

#show heading.where(level: 1): it => {
  v(1em)
  text(size: 15.5pt, weight: "bold", fill: accent, it.body)
  v(0.25em)
  line(length: 100%, stroke: 0.8pt + accent)
  v(0.7em)
}

#let review-box(title, body) = block(width: 100%, fill: soft, stroke: 0.55pt + rule, radius: 3pt, inset: 9pt)[
  #text(weight: "bold", fill: accent)[#title]
  #v(0.4em)
  #body
]

#align(center)[
  #text(size: 21pt, weight: "bold", fill: accent)[Academic Paper Review Report]
  #v(0.3em)
  #text(size: 10.5pt)[Reviewer: {{author}} · {{date}}]
]

#v(1em)

= Paper Information
#figure(
  table(
    columns: (1fr, 2fr),
    [Title], [{{title}}],
    [Venue / Journal], [],
    [Manuscript ID], [],
    [Recommendation], [],
  ),
  caption: [Manuscript metadata],
  kind: table,
)

= Review Summary
{{cursor}}

#review-box("Overall assessment")[
  Summarize the paper's contribution, the strongest reason to accept, and the strongest reason to reject or request revision.
]

= Scorecard
#figure(
  table(
    columns: 3,
    [Criterion], [Score], [Reason],
    [Novelty], [/10], [],
    [Technical quality], [/10], [],
    [Evidence], [/10], [],
    [Clarity], [/10], [],
    [Reproducibility], [/10], [],
  ),
  caption: [Review scorecard],
  kind: table,
)

= Major Comments
1.
2.
3.

= Minor Comments
-

= Questions For The Authors
-

= Confidential Notes
-
