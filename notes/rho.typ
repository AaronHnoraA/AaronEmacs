// rho.typ --- Rho-style Typst notes
//
// Typst replacement for latex/rho.cls.

#import "/_typst/note.typ": *

#let rho-color = rgb("1f4d2b")
#let rho-soft = rgb("e7f0e9")
#let rho-code-bg = rgb("f8f8f8")
#let rho-code-comment = rgb("878787")
#let rho-code-key = rgb("2938b0")
#let rho-code-string = rgb("b83838")
#let rho-gray = rgb("808080")

#let rho-page-ref() = context [
  #counter(page).display() - #counter(page).final().first()
]

#let rhostart(letter) = text(fill: rho-color, weight: "bold", size: 27pt)[#letter]

#let inlinecode(body) = box(
  fill: rgb("e6e6e6"),
  stroke: 0.4pt + rgb("b8b8b8"),
  radius: 1pt,
  inset: (x: 2pt, y: 1pt),
  baseline: 15%,
)[#text(font: "Menlo", size: 0.86em, fill: rho-color)[#body]]

#let rho-abstract(abstract: none, keywords: none) = {
  if abstract != none or keywords != none {
    block(
      fill: rho-soft,
      radius: 3pt,
      inset: 6pt,
      breakable: true,
    )[
      #text(fill: rho-color, font: "Helvetica Neue", size: 9pt, weight: "bold")[Abstract]
      #v(0.5em)
      #if abstract != none {
        text(font: "Helvetica Neue", size: 8.5pt)[#abstract]
      }
      #if keywords != none {
        v(10pt)
        text(font: "Helvetica Neue", size: 7.8pt, style: "italic")[
          #strong[Keywords:] #keywords
        ]
      }
    ]
  }
}

#let rho-title(
  title: "",
  author: "",
  dates: none,
  doctype: none,
  abstract: none,
  keywords: none,
) = {
  if doctype != none {
    text(font: "Helvetica Neue", weight: "bold")[#doctype]
    parbreak()
  }
  text(font: "Helvetica Neue", fill: rho-color, weight: "bold", size: 18pt)[#title]
  parbreak()
  text(font: "Helvetica Neue", size: 10pt)[#author]
  if dates != none {
    parbreak()
    text(font: "Helvetica Neue", size: 7pt)[#dates]
  }
  v(18pt)
  rho-abstract(abstract: abstract, keywords: keywords)
  v(18pt)
}

#let rho-theme(
  title: none,
  journal: none,
  journal-name: none,
  day: none,
  vol: none,
  no: none,
  body,
) = {
  set page(
    margin: (left: 1.25cm, right: 1.25cm, top: 2cm, bottom: 2cm),
    header-ascent: 0.75cm,
    header: context grid(
      columns: (1fr, 1fr),
      text(font: "Helvetica Neue", size: 8pt)[#if journal != none { journal }],
      align(right)[#text(font: "Helvetica Neue", size: 8pt)[#if title != none { title }]],
    ),
    footer: context {
      if counter(page).get().first() == 1 {
        grid(
          columns: (1fr, 1fr),
          text(font: "Helvetica Neue", size: 8pt)[
            #if journal-name != none { strong(journal-name) }
            #if day != none { h(8pt); day }
            #if vol != none { h(8pt); [Vol. #vol] }
            #if no != none { h(8pt); [No. #no] }
          ],
          align(right)[#text(font: "Helvetica Neue", size: 8pt)[
            #if journal != none { journal; h(8pt) }
            #strong[#rho-page-ref()]
          ]],
        )
      } else {
        align(center)[#text(font: "Helvetica Neue", size: 8pt)[#rho-page-ref()]]
      }
    },
  )
  set text(font: ("Times New Roman", "Songti SC", "New Computer Modern"), size: 10pt)
  set par(justify: true)
  show heading.where(level: 1): it => block[
    #v(1.8em)
    #text(font: "Helvetica Neue", fill: rho-color, size: 1.16em, weight: "bold")[
      #upper(it.body)
    ]
    #v(5pt)
  ]
  show heading.where(level: 2): set text(font: "Helvetica Neue", weight: "bold")
  show heading.where(level: 3): set text(font: "Helvetica Neue", weight: "bold", style: "italic", size: 0.92em)
  show table.cell: set text(font: "Helvetica Neue", size: 0.9em)
  show raw: it => block(
    fill: rho-code-bg,
    stroke: 0.6pt + rho-gray,
    radius: 1pt,
    inset: 0.62em,
    breakable: true,
  )[
    #text(font: "Menlo", size: 0.86em)[#it]
  ]
  body
}

#let figref(label) = link(label)[Figure #ref(label)]
#let figsref(label) = link(label)[Figures #ref(label)]
#let tabref(label) = link(label)[Table #ref(label)]
#let coderef(label) = link(label)[Code #ref(label)]
#let equref(label) = link(label)[Equation #ref(label)]
