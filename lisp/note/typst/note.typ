// note.typ --- Default Typst note style
//
// Typst replacement for latex/default.cls plus the old generated note helper.

#import "/_typst/math.typ": *

#let note-accent = rgb("3c71b7")
#let note-accent-soft = rgb("e6eef8")
#let note-rule = rgb("c8c1b4")
#let note-paper = rgb("e7e5df")
#let note-ink = rgb("29251f")
#let note-link-fill = rgb("1d4ed8")
#let note-zotero-fill = rgb("6d3b8a")
#let note-zotero-soft = rgb("f0e7f7")
#let note-zotero-rule = rgb("cab6dc")

#let note-include-active = state("my-note-include-active", false)
#let note-path(id) = "/_typst/notes/" + id + ".typ"
#let note-import-path(id) = note-path(id)
#let note-include(id) = {
  note-include-active.update(true)
  include(note-path(id))
  note-include-active.update(false)
}
#let note-transclude(id) = note-include(id)

#let note-body-font = (
  "New Computer Modern",
  "FZLiuGongQuanKaiShuJF",
  "Libertinus Serif",
  "New Computer Modern",
)
#let note-heading-font = (
  "Excalifont",
  "FZLiuGongQuanKaiShuJF",
  "New Computer Modern",
)
#let note-code-font = "Menlo"
#let note-math-font = ("GFS Neohellenic Math",)

#let note-theme(
  body,
  page-fill: note-paper,
  page-margin: (x: 6.5em, y: 5.5em),
  page-header: none,
  page-footer: context align(center)[
    #text(fill: note-accent, size: 0.82em)[#counter(page).display()]
  ],
  text-size: 12pt,
  text-fill: note-ink,
  par-leading: 0.72em,
  table-stroke: 0.65pt + note-rule,
  table-cell-size: 0.95em,
  raw-size: 0.92em,
  heading1-v-before: 0.55em,
  heading1-fill: note-accent-soft,
  heading1-stroke: 0.7pt + rgb("bfd0e5"),
  heading1-radius: 3pt,
  heading1-inset: (x: 0.76em, y: 0.48em),
  heading1-text-fill: note-accent,
  heading1-text-size: 1.22em,
  heading2-style: "marker",
  heading2-v-before: 0.35em,
  heading2-marker-fill: note-accent,
  heading2-marker-text-fill: rgb("4a5d72"),
  heading2-rule-stroke: 0.65pt + note-rule,
  heading2-rule-text-fill: note-ink,
) = {
  set page(
    fill: page-fill,
    margin: page-margin,
    header: page-header,
    footer: page-footer,
  )
  set text(
    font: note-body-font,
    size: text-size,
    fill: text-fill,
    lang: "en",
  )
  set par(leading: par-leading, justify: true)
  set table(
    stroke: table-stroke,
    inset: (x: 0.62em, y: 0.48em),
  )
  show heading: set text(font: note-heading-font, weight: "bold")
  show heading.where(level: 1): it => block[
    #v(heading1-v-before)
    #block(
      width: 100%,
      fill: heading1-fill,
      stroke: heading1-stroke,
      radius: heading1-radius,
      inset: heading1-inset,
    )[
      #text(fill: heading1-text-fill, size: heading1-text-size, weight: "bold")[#it]
    ]
    #v(0.18em)
  ]
  show heading.where(level: 2): it => block[
    #v(heading2-v-before)
    #if heading2-style == "rule" {
      block(
        width: 100%,
        stroke: (bottom: heading2-rule-stroke),
        inset: (bottom: 0.2em),
      )[
        #text(fill: heading2-rule-text-fill, weight: "semibold")[#it]
      ]
    } else {
      grid(
        columns: (0.28em, 1fr),
        gutter: 0.56em,
        rect(width: 0.28em, height: 1.05em, fill: heading2-marker-fill, radius: 1pt),
        text(fill: heading2-marker-text-fill, weight: "semibold")[#it],
      )
    }
  ]
  show raw: set text(font: note-code-font, size: raw-size)
  show math.equation: set text(font: note-math-font)
  show table.cell: set text(size: table-cell-size)
  body
}

#let note-entry-with(
  body,
  toc: true,
  theme: note-theme,
  toc-title: [目录],
  toc-depth: 2,
  toc-wrapper: it => it,
) = {
  show: theme
  context {
    if not note-include-active.get() and toc {
      toc-wrapper(outline(title: toc-title, depth: toc-depth))
    }
  }
  body
}

#let note-entry(toc: true, body) = note-entry-with(body, toc: toc)

#let note-card(title, accent, tint, marker, body) = {
  block(
    width: 100%,
    fill: rgb(tint),
    stroke: (left: 1.8pt + rgb(accent), rest: 0.35pt + rgb("d8d1c4")),
    radius: 2pt,
    inset: (x: 0.86em, y: 0.62em),
    breakable: true,
  )[
    #text(fill: rgb(accent), weight: "semibold", size: 0.9em)[#title]
    #v(0.28em)
    #{
      show math.equation: set text(font: note-math-font)
      body
    }
    #if marker != "" {
      v(0.28em)
      align(right)[
        #text(fill: rgb(accent), size: 0.78em)[#marker]
      ]
    }
  ]
}

#let definition(body) = note-card("📘 定义", "8a6418", "f8ecd0", "◇", body)
#let theorem(body) = note-card("📐 定理", "2f6f42", "e5f3df", "♥", body)
#let lemma(body) = note-card("🪜 引理", "335f91", "e4edf8", "⋄", body)
#let corollary(body) = note-card("🔎 推论", "5a4f91", "ece8f8", "⇒", body)
#let cor(body) = corollary(body)
#let proposition(body) = note-card("📌 命题", "7a4b2d", "f3e6dc", "♠", body)
#let prop(body) = proposition(body)
#let property(body) = proposition(body)
#let proof(body) = note-card("✍️ 证明", "267386", "e1f2f4", "∎", body)
#let example(body) = note-card("🧪 例子", "80623a", "f1e5d4", "◦", body)
#let remark(body) = note-card("💬 备注", "5f6c7b", "e9ece8", "✦", body)
#let summary(body) = note-card("🧾 摘要", "476f78", "e3f1ee", "SUM", body)
#let question(body) = note-card("❓ 问题", "8a6418", "f8ecd0", "?", body)
#let problem(body) = question(body)
#let solution(body) = note-card("✅ 解法", "2f6f42", "e5f3df", "✓", body)
#let important(body) = note-card("⚡ 重点", "9b3b37", "f4dfdc", "!", body)
#let warning(body) = note-card("⚠️ 警告", "9b3b37", "f4dfdc", "▲", body)
#let tip(body) = note-card("💡 提示", "26735f", "e1f1ea", "✧", body)
#let info(body) = note-card("ℹ️ 信息", "335f91", "e4edf8", "i", body)

#let note-comment-box(body, accent: "5f6c7b", tint: "eceae4", title: "批注") = block(
  width: 100%,
  fill: rgb(tint),
  stroke: (left: 1.2pt + rgb(accent), rest: 0.35pt + rgb("d8d1c4")),
  radius: 2pt,
  inset: (x: 0.68em, y: 0.46em),
  breakable: true,
)[
  #text(fill: rgb(accent), weight: "semibold", size: 0.78em)[#title]
  #h(0.35em)
  #text(fill: rgb("3f3932"), size: 0.9em)[#body]
]

#let comment(body) = note-comment-box(body)
#let annotation(body) = note-comment-box(body, accent: "7a4b2d", tint: "f3e6dc", title: "注")
#let note-side-width = 12em
#let note-margin-width = 5.4em
#let note-floating-side(body, dx: 24em, width: note-side-width, title: "侧注") = block(
  width: 100%,
  height: 0pt,
  above: 0pt,
  below: 0pt,
)[
  #place(top + left, dx: dx, dy: -0.15em)[
    #block(width: width)[
      #note-comment-box(body, accent: "335f91", tint: "e4edf8", title: title)
    ]
  ]
]
#let marginal(body) = note-floating-side(body, dx: -6.1em, width: note-margin-width, title: "旁注")
#let margin-note(body) = marginal(body)
#let mnote(body) = marginal(body)
#let sidecomment(body) = note-floating-side(body, dx: 24em, title: "侧注")
#let sidenote(body) = sidecomment(body)
#let fn(body) = footnote(body)
#let foot(body) = footnote(body)
#let topmark(body) = super(body)
#let bottommark(body) = sub(body)
#let supmark(body) = super(body)
#let submark(body) = sub(body)

#let note(..args) = {
  let pos = args.pos()
  if pos.len() == 2 {
    text(fill: note-link-fill, underline(pos.at(1)))
  } else if pos.len() == 1 {
    note-card("📝 笔记", "26735f", "e1f1ea", "✎", pos.at(0))
  }
}

#let zoterolink(url, body) = link(url)[
  #box(
    fill: note-zotero-soft,
    stroke: 0.55pt + note-zotero-rule,
    inset: (x: 0.42em, y: 0.08em),
    outset: (y: 0.12em),
    radius: 2pt,
    baseline: 0.04em,
  )[
    #text(fill: note-zotero-fill, weight: "bold", size: 0.72em)[ZOTERO]
    #h(0.35em)
    #text(fill: note-link-fill)[#body]
  ]
]

#let bib(path: "/references/references.bib", title: [References], style: "apa") = {
  v(0.8em)
  block(
    width: 100%,
    fill: rgb("f4f0e8"),
    stroke: 0.45pt + rgb("d2c8b8"),
    radius: 2pt,
    inset: (x: 0.82em, y: 0.6em),
    breakable: true,
  )[
    #bibliography(path, title: title, style: style)
  ]
}

#let references = bib

// --- Status chips (inline, e.g. `- #todo[Write proof of Lemma 3]`).
// Scanned by `my/note-agenda` on the Emacs side; keep the call form
// `#<state>[body]` so the regex stays stable.
#let task-tag(name, accent, tint) = box(
  fill: rgb(tint),
  stroke: 0.6pt + rgb(accent),
  inset: (x: 0.5em, y: 0.06em),
  outset: (y: 0.18em),
  radius: 999pt,
  baseline: 0.05em,
)[#text(
    fill: rgb(accent),
    weight: "bold",
    size: 0.72em,
    tracking: 0.06em,
  )[#upper(name)]]

#let task-muted = rgb("8a8077")
#let task-meta-chip(name, value, accent, tint) = {
  if value != none and value != "" {
    box(
      fill: rgb(tint),
      stroke: 0.45pt + rgb(accent),
      inset: (x: 0.38em, y: 0.04em),
      outset: (y: 0.16em),
      radius: 999pt,
      baseline: 0.04em,
    )[
      #text(fill: rgb(accent), weight: "semibold", size: 0.68em)[#upper(name)]
      #h(0.24em)
      #text(fill: rgb(accent), size: 0.68em)[#value]
    ]
    h(0.12em)
  }
}

#let task-repeat-value(repeat, recurrence) = {
  if repeat != none and repeat != "" {
    repeat
  } else {
    recurrence
  }
}

#let task-body(body, muted: false) = {
  if muted {
    text(fill: task-muted)[#strike[#body]]
  } else {
    body
  }
}

#let task-line(
  name,
  accent,
  tint,
  body,
  due: none,
  scheduled: none,
  priority: none,
  repeat: none,
  recurrence: none,
  muted: false,
) = [
  #task-tag(name, accent, tint)
  #task-meta-chip("p", priority, "7a4b2d", "f3e6dc")
  #task-meta-chip("due", due, "9b3b37", "f4dfdc")
  #task-meta-chip("sched", scheduled, "335f91", "e4edf8")
  #task-meta-chip("rep", task-repeat-value(repeat, recurrence), "5f6c7b", "eceae4")
  #task-body(body, muted: muted)
]

#let todo(due: none, scheduled: none, priority: none, repeat: none, recurrence: none, body) = task-line(
  "todo", "9b3b37", "f4dfdc", body,
  due: due, scheduled: scheduled, priority: priority, repeat: repeat, recurrence: recurrence,
)
#let doing(due: none, scheduled: none, priority: none, repeat: none, recurrence: none, body) = task-line(
  "doing", "9b6017", "fbe9c8", body,
  due: due, scheduled: scheduled, priority: priority, repeat: repeat, recurrence: recurrence,
)
#let waiting(due: none, scheduled: none, priority: none, repeat: none, recurrence: none, body) = task-line(
  "waiting", "5f6c7b", "eceae4", body,
  due: due, scheduled: scheduled, priority: priority, repeat: repeat, recurrence: recurrence,
)
#let done(due: none, scheduled: none, priority: none, repeat: none, recurrence: none, body) = task-line(
  "done", "2f6f42", "e5f3df", body,
  due: due, scheduled: scheduled, priority: priority, repeat: repeat, recurrence: recurrence,
  muted: true,
)
#let cancelled(due: none, scheduled: none, priority: none, repeat: none, recurrence: none, body) = task-line(
  "cancelled", "8a8077", "ece8e0", body,
  due: due, scheduled: scheduled, priority: priority, repeat: repeat, recurrence: recurrence,
  muted: true,
)
