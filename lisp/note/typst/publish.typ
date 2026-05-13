// publish.typ --- Typst style for public note PDFs

#import "/_typst/note.typ": *

#let publish-paper = rgb("ddd0b6")
#let publish-ink = rgb("23180f")
#let publish-accent = rgb("684a2c")
#let publish-accent-soft = rgb("efe0c6")
#let publish-rule = rgb("9d7a51")
#let note-link-fill = rgb("1d3f66")

#let publish-page-background = [
  #place(top + left)[
    #rect(width: 100%, height: 100%, fill: publish-paper)
  ]
  #place(top + left, dx: 1.1em, dy: 1.1em)[
    #rect(
      width: 100% - 2.2em,
      height: 100% - 2.2em,
      stroke: 0.45pt + rgb("c7a982"),
      radius: 3pt,
    )
  ]
  #place(top + left, dx: 2.0em, dy: 2.0em)[
    #rect(
      width: 34%,
      height: 0.42em,
      fill: rgb("c9ad83"),
      radius: 1pt,
    )
  ]
  #place(bottom + right, dx: -2.0em, dy: -2.0em)[
    #rect(
      width: 28%,
      height: 0.34em,
      fill: rgb("c9ad83"),
      radius: 1pt,
    )
  ]
]

#let publish-theme = note-theme.with(
  page-fill: publish-paper,
  page-margin: (x: 5.8em, y: 4.9em),
  page-header: context align(right)[
    #text(fill: rgb("6b553b"), size: 0.78em)[#counter(page).display()]
  ],
  page-footer: context align(center)[
    #text(fill: rgb("7a6041"), size: 0.72em)[Aaron He / Typst Note]
  ],
  text-size: 11.4pt,
  text-fill: publish-ink,
  par-leading: 0.76em,
  table-stroke: 0.65pt + publish-rule,
  table-cell-size: 0.94em,
  raw-size: 0.9em,
  heading1-v-before: 0.62em,
  heading1-fill: publish-accent-soft,
  heading1-stroke: (left: 2pt + publish-accent, rest: 0.55pt + rgb("d5bd98")),
  heading1-radius: 2pt,
  heading1-inset: (x: 0.82em, y: 0.55em),
  heading1-text-fill: publish-accent,
  heading1-text-size: 1.24em,
  heading2-style: "rule",
  heading2-v-before: 0.45em,
  heading2-rule-stroke: 0.65pt + rgb("b49367"),
  heading2-rule-text-fill: rgb("4b3827"),
)

#let publish-cover(
  title: none,
  date: none,
  collection: none,
  tags: (),
  summary: none,
  path: none,
) = {
  block(
    width: 100%,
    fill: rgb("efe3cc"),
    stroke: (left: 3pt + publish-accent, rest: 0.55pt + rgb("b49367")),
    radius: 2pt,
    inset: (x: 1.1em, y: 1.0em),
    breakable: false,
  )[
    #text(fill: rgb("735639"), size: 0.78em, weight: "semibold")[PUBLIC NOTE]
    #v(0.5em)
    #text(fill: publish-accent, size: 2.05em, weight: "bold")[#if title != none { title } else { "Untitled" }]
    #if summary != none and summary != "" {
      v(0.75em)
      text(fill: rgb("4b3827"), size: 0.96em)[#summary]
    }
    #v(0.9em)
    #grid(
      columns: (auto, 1fr),
      gutter: 0.5em,
      row-gutter: 0.28em,
      text(fill: rgb("735639"), size: 0.76em, weight: "semibold")[Collection],
      text(fill: publish-ink, size: 0.86em)[#if collection != none { collection } else { "Note" }],
      text(fill: rgb("735639"), size: 0.76em, weight: "semibold")[Date],
      text(fill: publish-ink, size: 0.86em)[#if date != none { date } else { "Undated" }],
      text(fill: rgb("735639"), size: 0.76em, weight: "semibold")[Path],
      text(fill: rgb("5c4a36"), size: 0.78em)[#if path != none { path } else { "" }],
    )
    #if tags.len() > 0 {
      v(0.7em)
      for tag in tags {
        box(
          fill: rgb("f7ecd7"),
          stroke: 0.45pt + rgb("c2a37a"),
          radius: 1.5pt,
          inset: (x: 0.42em, y: 0.12em),
        )[
          #text(fill: rgb("684a2c"), size: 0.76em)[#tag]
        ]
        h(0.28em)
      }
    }
  ]
  v(0.9em)
}

#let publish-toc-wrapper(it) = block(
  width: 100%,
  fill: rgb("efe3cc"),
  stroke: 0.55pt + rgb("b49367"),
  radius: 2pt,
  inset: 0.8em,
)[#it]

#let publish-entry(
  title: none,
  date: none,
  collection: none,
  tags: (),
  summary: none,
  path: none,
  toc: true,
  body,
) = {
  set page(paper: "a4", background: publish-page-background)
  show: publish-theme
  publish-cover(
    title: title,
    date: date,
    collection: collection,
    tags: tags,
    summary: summary,
    path: path,
  )
  context {
    if not note-include-active.get() and toc {
      publish-toc-wrapper(outline(title: [Contents], depth: 2))
    }
  }
  body
}

#let note-entry(toc: true, body) = {
  publish-entry(toc: toc, body)
}
