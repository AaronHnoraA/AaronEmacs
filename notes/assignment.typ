// assignment.typ --- Assignment-style Typst notes
//
// Typst replacement for latex/assignment.cls.

#import "/_typst/note.typ": *

#let assignment-accent = rgb("2f5f42")
#let assignment-rule = rgb("9aa79a")
#let assignment-code-bg = rgb("f2f2fa")
#let assignment-code-frame = rgb("bfc0d9")

#let maybe-link(url, body) = {
  if url == none or url == "" { body } else { link(url)[#body] }
}

#let assignment-page-ref() = context [
  #counter(page).display() / #counter(page).final().first()
]

#let assignment-first-page-left(institution, course-code, term) = text(size: 8.5pt)[
  #stack(
    dir: ttb,
    spacing: 1pt,
    [#institution],
    align(right)[#text(font: "Helvetica Neue")[#course-code]],
    [#term],
  )
]

#let assignment-first-page-center(title, date) = align(center)[
  #text(size: 28pt, weight: "bold")[#title]
  #linebreak()
  #text(size: 8.5pt)[#date]
]

#let assignment-first-page-right(name, author-url, student-id, affiliation) = text(size: 8.5pt)[
  #align(right)[
    #text(font: "Helvetica Neue")[#affiliation]
    #v(4pt)
    #grid(
      columns: (auto, 3cm),
      gutter: 0.35em,
      align(right)[Name:],
      align(right)[#maybe-link(author-url, name)],
      align(right)[Student ID:],
      align(right)[#student-id],
    )
  ]
]

#let assignment-theme(
  institution: "____",
  course-code: "____",
  term: "____",
  title: "____",
  date: "____",
  student-name: "____",
  author-url: none,
  student-id: "____",
  affiliation: "UNSW",
  toc: true,
  body,
) = {
  set page(
    paper: "a4",
    margin: (top: 0.78in, bottom: 0.86in, left: 0.62in, right: 0.62in),
    header-ascent: 16pt,
    header: context {
      if counter(page).get().first() == 1 {
        grid(
          columns: (1fr, 2fr, 1fr),
          gutter: 1em,
          assignment-first-page-left(institution, course-code, term),
          assignment-first-page-center(title, date),
          assignment-first-page-right(student-name, author-url, student-id, affiliation),
        )
      } else {
        grid(
          columns: (1fr, 1fr, 1fr),
          maybe-link(author-url, student-name),
          align(center)[#title],
          align(right)[#student-id],
        )
      }
    },
    footer: align(right)[#text(size: 8pt)[#assignment-page-ref()]],
  )
  set text(
    font: ("New Computer Modern", "Songti SC", "Libertinus Serif"),
    size: 10pt,
  )
  set par(first-line-indent: 1.2em, spacing: 0.18em, leading: 0.56em)
  set list(spacing: 0.15em)
  show heading.where(level: 1): set text(size: 1.22em, weight: "bold")
  show heading.where(level: 2): set text(size: 1em, weight: "bold")
  show heading.where(level: 3): set text(size: 1em, weight: "bold", style: "italic")
  show raw: it => block(
    fill: assignment-code-bg,
    stroke: 0.7pt + assignment-code-frame,
    radius: 1pt,
    inset: 0.65em,
    breakable: true,
  )[
    #text(font: "Menlo", size: 0.86em)[#it]
  ]
  v(0.35em)
  if toc {
    text(size: 8.5pt)[#outline(title: [Contents], depth: 2)]
    v(0.7em)
    line(length: 100%, stroke: 0.65pt + assignment-rule)
    v(0.8em)
  }
  body
}

#let problem(title: none, body) = note-card(
  if title == none { "Problem" } else { "Problem: " + title },
  "2f5f42",
  "eef6ee",
  "",
  body,
)

#let answer(body) = note-card("Answer", "335f91", "e4edf8", "□", body)
#let sol(body) = solution(body)
#let pf(body) = proof(body)

#let abs(body) = $ lr(| #body |) $
#let norm(body) = $ lr(|| #body ||) $
#let re = math.op("Re")
#let im = math.op("Im")
#let sgn = math.op("sgn")
#let sinc = math.op("sinc")
#let rect = math.op("rect")
#let tr = math.op("Tr")
#let res = math.op("Res")
