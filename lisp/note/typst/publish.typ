// publish.typ --- Typst style for public note PDFs

#import "/_typst/note.typ": *

#let publish-paper = note-paper
#let publish-ink = note-ink
#let publish-accent = note-accent
#let publish-accent-soft = note-accent-soft
#let publish-rule = note-rule
#let note-link-fill = rgb("1d3f66")

#let publish-theme = note-theme.with(
  page-fill: publish-paper,
  page-margin: (left: 2.3cm, right: 4.8cm, top: 2.4cm, bottom: 2.8cm),
  page-header: context align(right)[
    #text(fill: rgb("626262"), size: 0.78em)[#counter(page).display()]
  ],
  page-footer: context align(center)[
    #text(fill: rgb("626262"), size: 0.72em)[Aaron He / Typst Note]
  ],
  text-size: 11.2pt,
  text-fill: publish-ink,
  par-leading: 0.64em,
  table-stroke: 0.65pt + publish-rule,
  table-cell-size: 0.94em,
  raw-size: 0.9em,
  heading1-v-before: 0.9em,
  heading1-fill: none,
  heading1-stroke: (bottom: 0.65pt + publish-rule),
  heading1-radius: 0pt,
  heading1-inset: (bottom: 0.28em),
  heading1-text-fill: publish-ink,
  heading1-text-size: 1.28em,
  heading2-style: "rule",
  heading2-v-before: 0.5em,
  heading2-rule-stroke: 0.65pt + publish-rule,
  heading2-rule-text-fill: publish-ink,
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
    stroke: (bottom: 0.75pt + publish-rule),
    inset: (bottom: 1.0em),
    breakable: false,
  )[
    #align(center)[
      #text(fill: rgb("5a5a5a"), size: 0.78em, weight: "semibold")[PUBLIC NOTE]
    ]
    #v(0.5em)
    #align(center)[
      #text(fill: publish-ink, size: 2.05em, weight: "bold")[#if title != none { title } else { "Untitled" }]
    ]
    #if summary != none and summary != "" {
      v(0.75em)
      align(center)[#block(width: 82%)[#text(fill: rgb("444444"), size: 0.96em)[#summary]]]
    }
    #v(0.9em)
    #align(center)[
      #text(fill: rgb("555555"), size: 0.86em)[
        #if collection != none { collection } else { "Note" }
        #h(0.45em)
        #text(fill: rgb("9a9a9a"))[|]
        #h(0.45em)
        #if date != none { date } else { "Undated" }
      ]
    ]
    #if path != none and path != "" {
      v(0.35em)
      align(center)[#text(fill: rgb("666666"), size: 0.72em)[#path]]
    }
    #if tags.len() > 0 {
      v(0.7em)
      align(center)[
        #text(fill: rgb("666666"), size: 0.76em)[#tags.join(" · ")]
      ]
    }
  ]
  v(0.9em)
}

#let publish-toc-wrapper(it) = block(
  width: 100%,
  stroke: (top: 0.65pt + publish-rule, bottom: 0.65pt + publish-rule),
  inset: (y: 0.75em),
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
  set page(paper: "a4")
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

#let publish-meta-value(meta, key, override: none, default: none) = {
  if override != none {
    override
  } else {
    meta.at(key, default: default)
  }
}

#let note-entry(
  toc: true,
  title: none,
  date: none,
  collection: none,
  tags: none,
  summary: none,
  path: none,
  body,
) = context {
  let notes = query(<note>)
  let meta = if notes.len() > 0 { notes.first().value } else { (:) }
  publish-entry(
    title: publish-meta-value(meta, "title", override: title, default: none),
    date: publish-meta-value(meta, "date", override: date, default: none),
    collection: publish-meta-value(meta, "collection", override: collection, default: none),
    tags: publish-meta-value(meta, "tags", override: tags, default: ()),
    summary: publish-meta-value(meta, "summary", override: summary, default: none),
    path: publish-meta-value(meta, "path", override: path, default: none),
    toc: toc,
    body,
  )
}
