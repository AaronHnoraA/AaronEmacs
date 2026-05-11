// note.typ --- Default Typst note style
//
// Typst replacement for latex/default.cls plus the old generated note helper.

#let note-accent = rgb("3c71b7")
#let note-accent-soft = rgb("e6eef8")
#let note-rule = rgb("c8c1b4")
#let note-paper = rgb("e7e5df")
#let note-ink = rgb("29251f")
#let note-link-fill = rgb("1d4ed8")

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

#let note-theme(body) = {
  set page(
    fill: note-paper,
    margin: (x: 6.5em, y: 5.5em),
    footer: context align(center)[
      #text(fill: note-accent, size: 0.82em)[#counter(page).display()]
    ],
  )
  set text(
    font: note-body-font,
    size: 12pt,
    fill: note-ink,
    lang: "en",
  )
  set par(leading: 0.72em, justify: true)
  set table(
    stroke: 0.65pt + note-rule,
    inset: (x: 0.62em, y: 0.48em),
  )
  show heading: set text(font: note-heading-font, weight: "bold")
  show heading.where(level: 1): it => block[
    #v(0.55em)
    #block(
      width: 100%,
      fill: note-accent-soft,
      stroke: 0.7pt + rgb("bfd0e5"),
      radius: 3pt,
      inset: (x: 0.76em, y: 0.48em),
    )[
      #text(fill: note-accent, size: 1.22em, weight: "bold")[#it]
    ]
    #v(0.18em)
  ]
  show heading.where(level: 2): it => block[
    #v(0.35em)
    #grid(
      columns: (0.28em, 1fr),
      gutter: 0.56em,
      rect(width: 0.28em, height: 1.05em, fill: note-accent, radius: 1pt),
      text(fill: rgb("4a5d72"), weight: "semibold")[#it],
    )
  ]
  show raw: set text(font: note-code-font, size: 0.92em)
  show math.equation: set text(font: note-math-font)
  show table.cell: set text(size: 0.95em)
  body
}

#let note-entry(toc: true, body) = {
  show: note-theme
  context {
    if not note-include-active.get() and toc {
      outline(title: [目录], depth: 2)
    }
  }
  body
}

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
#let summary(body) = note-card("🧾 摘要", "476f78", "e3f1ee", "☰", body)
#let question(body) = note-card("❓ 问题", "8a6418", "f8ecd0", "?", body)
#let problem(body) = question(body)
#let solution(body) = note-card("✅ 解法", "2f6f42", "e5f3df", "✓", body)
#let important(body) = note-card("⚡ 重点", "9b3b37", "f4dfdc", "!", body)
#let warning(body) = note-card("⚠️ 警告", "9b3b37", "f4dfdc", "▲", body)
#let tip(body) = note-card("💡 提示", "26735f", "e1f1ea", "✧", body)
#let info(body) = note-card("ℹ️ 信息", "335f91", "e4edf8", "i", body)

#let note(..args) = {
  let pos = args.pos()
  if pos.len() == 2 {
    text(fill: note-link-fill, underline(pos.at(1)))
  } else if pos.len() == 1 {
    note-card("📝 笔记", "26735f", "e1f1ea", "✎", pos.at(0))
  }
}
