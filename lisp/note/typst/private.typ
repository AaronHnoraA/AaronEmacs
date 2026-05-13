// private.typ --- sealed placeholder for private published notes

#let private-paper = rgb("efe4cd")
#let private-ink = rgb("21160d")
#let private-muted = rgb("6f5a43")
#let private-rule = rgb("8a6846")
#let private-seal = rgb("8c281f")

#let private-note(id: "", collection: "Private", date: "Undated") = {
  set page(
    paper: "a4",
    fill: rgb("6f4120"),
    margin: (x: 5.6em, y: 5.2em),
  )
  set text(
    font: (
      "New Computer Modern",
      "Libertinus Serif",
      "FZLiuGongQuanKaiShuJF",
    ),
    fill: private-ink,
    lang: "en",
  )
  set par(leading: 0.72em)

  place(top + left)[
    #text(fill: rgb("eadbc1"), size: 0.72em, tracking: 0.08em)[RESTRICTED FOLIO]
  ]
  place(top + right)[
    #text(fill: rgb("eadbc1"), size: 0.72em, tracking: 0.08em)[SEALED ACCESS]
  ]

  v(1fr)
  align(center)[
    #block(
      width: 84%,
      fill: private-paper,
      stroke: 0.8pt + rgb("b99b72"),
      radius: 4pt,
      inset: (x: 2.2em, y: 2em),
    )[
      #text(fill: private-muted, size: 0.78em, tracking: 0.14em)[ADMINISTRATIVE NOTICE]
      #v(0.8em)
      #text(size: 2.25em, weight: "bold")[Sealed Document]
      #v(0.75em)
      #line(length: 56%, stroke: 0.7pt + private-rule)
      #v(1.1em)
      #block(
        width: 100%,
        fill: rgb("f6eddd"),
        stroke: (left: 2.4pt + private-seal, rest: 0.45pt + rgb("ceb38e")),
        radius: 3pt,
        inset: (x: 1.1em, y: 0.9em),
      )[
        #text(size: 1.15em, weight: "semibold")[This file has been sealed by the administrator.]
        #v(0.5em)
        #text(fill: private-muted)[
          The original note is private and is not included in this public distribution.
          If access is required, contact the site administrator through a private channel.
        ]
      ]
      #v(1.1em)
      #grid(
        columns: (auto, 1fr),
        gutter: 0.8em,
        row-gutter: 0.35em,
        text(fill: private-muted, weight: "semibold")[Collection],
        text(collection),
        text(fill: private-muted, weight: "semibold")[Date],
        text(date),
        text(fill: private-muted, weight: "semibold")[Record],
        text(id),
      )
      #v(1.15em)
      #align(right)[
        #rotate(10deg)[
          #block(
            stroke: 1.1pt + private-seal,
            radius: 2pt,
            inset: (x: 0.8em, y: 0.38em),
          )[
            #text(fill: private-seal, weight: "bold", tracking: 0.14em)[SEALED]
          ]
        ]
      ]
    ]
  ]
  v(1fr)
}
