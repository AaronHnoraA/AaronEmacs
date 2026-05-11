// aleph-notas.typ --- Aleph-style Typst notes
//
// Typst replacement for latex/aleph-notas.cls.

#import "/_typst/note.typ": *

#let aleph-color = rgb("2f4f91")
#let aleph-text = rgb("2f4f91")
#let aleph-soft = rgb("ece8f8")

#let aleph-header(
  institution: "Alephsub0",
  career: none,
  subject: none,
  topic: none,
  author: "",
  date: none,
  logo-one: none,
  logo-two: none,
) = {
  block(width: 100%)[
    #grid(
      columns: (auto, 1fr),
      gutter: 0.9em,
      if logo-one != none or logo-two != none {
        grid(
          columns: (auto, auto),
          gutter: 0.2em,
          if logo-one != none { image(logo-one, width: 10%) },
          if logo-two != none { image(logo-two, width: 10%) },
        )
      },
      block(stroke: (left: 1.2pt + aleph-color), inset: (left: 0.9em))[
        #text(fill: aleph-text, weight: "bold")[
          #smallcaps(institution)
          #if career != none or subject != none {
            linebreak()
            if career != none { career }
            if career != none and subject != none { [ #sym.bullet.c ] }
            if subject != none { subject }
          }
          #if topic != none {
            linebreak()
            smallcaps(topic)
          }
          #linebreak()
          #emph(author)
          #if date != none { [ #sym.bullet.c #date] }
        ]
      ],
    )
  ]
}

#let aleph-theme(author-short: none, topic-short: none, body) = {
  set page(
    width: 160mm,
    height: 240mm,
    margin: (inside: 1.7cm, outside: 1.7cm, top: 2cm, bottom: 2cm),
    header-ascent: 5mm,
    header: context grid(
      columns: (1fr, 1fr),
      text(weight: "bold")[#if topic-short != none { topic-short }],
      align(right)[#text(weight: "bold")[#if author-short != none { author-short }]],
    ),
    footer: context align(center)[#counter(page).display()],
  )
  set text(font: ("New Computer Modern", "Songti SC", "Libertinus Serif"), size: 10pt)
  set par(leading: 0.62em)
  show heading.where(level: 1): it => block[
    #v(0.8em)
    #align(center)[
      #line(length: 100%, stroke: 0.7pt + aleph-color)
      #text(fill: aleph-color, weight: "bold", size: 1.16em)[#it]
      #line(length: 100%, stroke: 0.7pt + aleph-color)
    ]
    #v(0.5em)
  ]
  show heading.where(level: 2): set text(fill: aleph-color, weight: "bold")
  show heading.where(level: 3): set text(fill: aleph-color, weight: "bold")
  body
}

#let teo(title: none, body) = note-card(
  if title == none { "Teorema" } else { "Teorema: " + title },
  "2f4f91",
  "ece8f8",
  "",
  body,
)
#let defi(title: none, body) = note-card(
  if title == none { "Definición" } else { "Definición: " + title },
  "2f4f91",
  "ece8f8",
  "",
  body,
)
#let axioma(title: none, body) = note-card(
  if title == none { "Axioma" } else { "Axioma: " + title },
  "2f4f91",
  "ece8f8",
  "",
  body,
)
#let prop(body) = note-card("Proposición", "2f4f91", "ece8f8", "", body)
#let cor(body) = note-card("Corolario", "2f4f91", "ece8f8", "", body)
#let lem(body) = note-card("Lema", "2f4f91", "ece8f8", "", body)
#let ejer(body) = note-card("Ejercicio", "2f4f91", "ece8f8", "", body)
#let ejem(body) = example(body)
#let obs(body) = remark(body)
#let advertencia(body) = note-card("Advertencia", "8a6418", "fff0bf", "!", body)
#let comentarios(body) = note-card("Sugerencias metodológicas", "2f4f91", "f7f5fb", "", body)
#let codigo(body) = block(
  stroke: (left: 3pt + aleph-color),
  inset: (left: 0.8em),
  breakable: true,
)[#text(size: 0.9em)[#body]]
