// note.typ --- Default Typst note style
//
// Typst replacement for latex/default.cls plus the old generated note helper.

#import "/_typst/math.typ": *
#import "/_typst/extension.typ": *

#let note-accent = rgb("253858")
#let note-accent-soft = rgb("f5f6f8")
#let note-rule = rgb("c9c9c9")
#let note-paper = rgb("fbfaf7")
#let note-ink = rgb("1f1f1f")
#let note-link-fill = rgb("1d4ed8")
#let note-zotero-fill = rgb("6d3b8a")
#let note-zotero-soft = rgb("f0e7f7")
#let note-zotero-rule = rgb("cab6dc")
#let note-visible-equation-numbering(n) = numbering("(1)", n)
#let note-hidden-equation-numbering(n) = []
#let note-equation(tag, body) = grid(
  columns: (1fr, auto),
  gutter: 0.86em,
  align: (center, horizon),
  [
    #body
    #label(tag)
  ],
  text(fill: rgb("6f2f2b"), size: 0.92em)[(#tag)],
)

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
  "New Computer Modern",
  "FZLiuGongQuanKaiShuJF",
  "Libertinus Serif",
)
#let note-section-heading-font = (
  "Excalifont",
  "FZLiuGongQuanKaiShuJF",
  "New Computer Modern",
)
#let note-code-font = "Menlo"
#let note-math-font = ("GFS Neohellenic Math",)

#let note-theme(
  body,
  page-fill: note-paper,
  page-margin: (left: 2.3cm, right: 4.8cm, top: 2.4cm, bottom: 2.8cm),
  page-header: none,
  page-footer: context align(center)[
    #text(fill: rgb("626262"), size: 0.82em)[#counter(page).display()]
  ],
  text-size: 11.2pt,
  text-fill: note-ink,
  par-leading: 0.64em,
  table-stroke: 0.65pt + note-rule,
  table-cell-size: 0.95em,
  raw-size: 0.92em,
  heading1-v-before: 0.9em,
  heading1-fill: none,
  heading1-stroke: (bottom: 0.65pt + note-rule),
  heading1-radius: 0pt,
  heading1-inset: (bottom: 0.28em),
  heading1-text-fill: note-ink,
  heading1-text-size: 1.28em,
  heading2-style: "marker",
  heading2-v-before: 0.5em,
  heading2-marker-fill: note-accent,
  heading2-marker-text-fill: rgb("4a5d72"),
  heading2-rule-stroke: 0.65pt + note-rule,
  heading2-rule-text-fill: note-ink,
  heading3-v-before: 0.42em,
  heading3-text-size: 1.03em,
  heading4-v-before: 0.34em,
  heading4-text-size: 0.98em,
  heading5-v-before: 0.25em,
  heading5-text-size: 0.94em,
) = {
  show: note-extensions
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
  show heading: set text(font: note-section-heading-font, weight: "bold")
  show heading.where(level: 1): it => block[
    #v(heading1-v-before)
    #block(
      width: 100%,
      fill: heading1-fill,
      stroke: heading1-stroke,
      radius: heading1-radius,
      inset: heading1-inset,
    )[
      #text(font: note-section-heading-font, fill: heading1-text-fill, size: heading1-text-size, weight: "bold")[#it]
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
        #text(font: note-section-heading-font, fill: heading2-rule-text-fill, weight: "semibold")[#it]
      ]
    } else {
      grid(
        columns: (0.28em, 1fr),
        gutter: 0.56em,
        rect(width: 0.28em, height: 1.05em, fill: heading2-marker-fill, radius: 1pt),
        text(font: note-section-heading-font, fill: heading2-marker-text-fill, weight: "semibold")[#it],
      )
    }
  ]
  show heading.where(level: 3): it => block[
    #v(heading3-v-before)
    #text(font: note-section-heading-font, fill: note-ink, size: heading3-text-size, weight: "semibold")[#it]
    #v(0.08em)
  ]
  show heading.where(level: 4): it => block[
    #v(heading4-v-before)
    #text(font: note-section-heading-font, fill: rgb("3d3d3d"), size: heading4-text-size, weight: "semibold", style: "italic")[#it]
    #v(0.04em)
  ]
  show heading.where(level: 5): it => block[
    #v(heading5-v-before)
    #text(fill: rgb("4d4d4d"), size: heading5-text-size, weight: "medium", style: "italic")[#it]
    #h(0.35em)
  ]
  show heading.where(level: 6): it => block[
    #v(0.2em)
    #text(fill: rgb("5a5a5a"), size: 0.9em, weight: "medium", style: "italic")[#it]
    #h(0.32em)
  ]
  show raw: set text(font: note-code-font, size: raw-size)
  set math.equation(numbering: note-hidden-equation-numbering)
  show math.equation: set text(font: note-math-font)
  show ref: it => link(it.target)[
    #text(fill: rgb("8f3430"), weight: "semibold")[#str(it.target)]
  ]
  show table.cell: set text(size: table-cell-size)
  body
}

#let note_resolved_title(title) = {
  if title != none {
    title
  } else {
    let notes = query(<note>)
    let meta = if notes.len() > 0 { notes.first().value } else { (:) }
    let metadata-title = meta.at("title", default: none)
    let metadata-id = meta.at("id", default: none)
    let input-title = sys.inputs.at("note-title", default: none)
    if metadata-title != none and metadata-title != "" {
      metadata-title
    } else if input-title != none and input-title != "" {
      input-title
    } else if metadata-id != none and metadata-id != "" {
      metadata-id
    } else {
      "Untitled Note"
    }
  }
}

#let note_metadata_value(meta, key, override: none, default: none) = {
  if override != none {
    override
  } else {
    meta.at(key, default: default)
  }
}

#let note_has_text(value) = value != none and value != ""

#let note_compile_date() = datetime.today().display("[year]-[month]-[day]")

#let note_meta_line(date) = {
  if note_has_text(date) {
    align(center)[#text(fill: rgb("6d675b"), size: 0.82em)[#date]]
  }
}

#let note_tags_line(tags) = {
  if tags != none and tags.len() > 0 {
    align(center)[
      #text(fill: rgb("777166"), size: 0.74em)[#tags.join(" / ")]
    ]
  }
}

#let note_title_block(
  title: none,
  date: none,
  collection: none,
  tags: none,
  path: none,
  summary: none,
) = {
  let notes = query(<note>)
  let meta = if notes.len() > 0 { notes.first().value } else { (:) }
  let resolved-date = note_metadata_value(meta, "date", override: date, default: note_compile_date())
  let resolved-tags = note_metadata_value(meta, "tags", override: tags, default: ())

  block(
  width: 100%,
  below: 1.1em,
  stroke: (bottom: 0.65pt + note-rule),
  inset: (bottom: 0.85em),
  breakable: false,
  )[
    #align(center)[
	      #text(font: note-heading-font, size: 1.86em, weight: "bold")[#note_resolved_title(title)]
    ]
    #if note_has_text(resolved-date) or (resolved-tags != none and resolved-tags.len() > 0) {
      v(0.52em)
      note_meta_line(resolved-date)
      if resolved-tags != none and resolved-tags.len() > 0 {
        v(0.24em)
        note_tags_line(resolved-tags)
      }
    }
  ]
}

#let note-entry-with(
  body,
  toc: true,
  title: none,
  date: none,
  collection: none,
  tags: none,
  path: none,
  summary: none,
  theme: note-theme,
  toc-title: [Contents],
  toc-depth: 2,
  toc-wrapper: it => it,
) = {
  show: theme
  context {
    if not note-include-active.get() and toc {
      note_title_block(
        title: title,
        date: date,
        collection: collection,
        tags: tags,
        path: path,
        summary: summary,
      )
      toc-wrapper(outline(title: toc-title, depth: toc-depth))
    }
  }
  body
}

#let note-entry(
  toc: true,
  title: none,
  date: none,
  collection: none,
  tags: none,
  path: none,
  summary: none,
  body,
) = note-entry-with(
  body,
  toc: toc,
  title: title,
  date: date,
  collection: collection,
  tags: tags,
  path: path,
  summary: summary,
)

#let note-card(title, accent, tint, marker, body) = {
  block(
    width: 100%,
    stroke: (left: 1.45pt + rgb(accent)),
    inset: (left: 0.82em, right: 0.2em, top: 0.36em, bottom: 0.36em),
    breakable: true,
  )[
    #text(font: note-section-heading-font, fill: rgb(accent), weight: "semibold", size: 0.95em)[#title]
    #v(0.2em)
	    #{
	      set math.equation(numbering: note-hidden-equation-numbering)
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

#let note-card-title(base, title) = {
  if title == none {
    base
  } else {
    [#base (#title).]
  }
}
#let note-math-block(base-title, accent, tint, marker, args) = {
  let pos = args.pos()
  if pos.len() == 1 {
    note-card(note-card-title(base-title, none), accent, tint, marker, pos.at(0))
  } else if pos.len() == 2 {
    note-card(note-card-title(base-title, pos.at(0)), accent, tint, marker, pos.at(1))
  } else {
    panic("note block expects body or title plus body")
  }
}

#let definition(..args) = note-math-block("📘 Def.", "7a5a16", "f8ecd0", "◇", args)
#let theorem(..args) = note-math-block("📐 Thm.", "2f5f3e", "e5f3df", "♥", args)
#let lemma(..args) = note-math-block("🪜 Lem.", "315a86", "e4edf8", "⋄", args)
#let corollary(..args) = note-math-block("🔎 Cor.", "574d86", "ece8f8", "⇒", args)
#let cor(..args) = note-math-block("🔎 Cor.", "574d86", "ece8f8", "⇒", args)
#let proposition(..args) = note-math-block("📌 Prop.", "76482a", "f3e6dc", "♠", args)
#let prop(..args) = note-math-block("📌 Prop.", "76482a", "f3e6dc", "♠", args)
#let property(..args) = note-math-block("📌 Prop.", "76482a", "f3e6dc", "♠", args)
#let proof(..args) = note-math-block("✍️ Pf.", "2d6c7d", "e1f2f4", "∎", args)
#let example(..args) = note-math-block("🧪 Ex.", "765d37", "f1e5d4", "◦", args)
#let remark(..args) = note-math-block("💬 Rem.", "596777", "e9ece8", "✦", args)
#let summary(..args) = note-math-block("🧾 Sum.", "3f6870", "e3f1ee", "SUM", args)
#let question(..args) = note-math-block("❓ Q.", "7a5a16", "f8ecd0", "?", args)
#let problem(..args) = note-math-block("❓ Prob.", "7a5a16", "f8ecd0", "?", args)
#let solution(..args) = note-math-block("✅ Sol.", "2f5f3e", "e5f3df", "✓", args)
#let important(..args) = note-math-block("⚡ Imp.", "8f3430", "f4dfdc", "!", args)
#let warning(..args) = note-math-block("⚠️ Warn.", "8f3430", "f4dfdc", "▲", args)
#let tip(..args) = note-math-block("💡 Tip.", "286b59", "e1f1ea", "✧", args)
#let info(..args) = note-math-block("ℹ️ Info.", "315a86", "e4edf8", "i", args)
#let claim(..args) = note-math-block("📎 Claim.", "2f5f3e", "e5f3df", "◊", args)
#let hypothesis(..args) = note-math-block("🔬 Hyp.", "5b586f", "ece8f8", "H", args)
#let construction(..args) = note-math-block("🧱 Const.", "76482a", "f3e6dc", "□", args)
#let conjecture(..args) = note-math-block("🔭 Conj.", "574d86", "ece8f8", "?", args)
#let assumption(..args) = note-math-block("📋 Assump.", "596777", "e9ece8", "A", args)
#let observation(..args) = note-math-block("👁 Obs.", "315a86", "e4edf8", "◦", args)
#let fact(..args) = note-math-block("🧩 Fact.", "315a86", "e4edf8", "◦", args)
#let openproblem(..args) = note-math-block("❓ Open Prob.", "8f3430", "f4dfdc", "?", args)
#let openquestion(..args) = note-math-block("❓ Open Q.", "8f3430", "f4dfdc", "?", args)
#let groupaction(..args) = note-math-block("⚙️ Action.", "2d6c7d", "e1f2f4", "↷", args)

#let note-comment-box(body, accent: "5f6c7b", tint: "eceae4", title: "Comment") = block(
  width: 100%,
  fill: rgb(tint),
  stroke: (left: 1.2pt + rgb(accent), rest: 0.35pt + rgb("d8d1c4")),
  radius: 2pt,
  inset: (x: 0.68em, y: 0.46em),
  breakable: true,
)[
  #set par(justify: false)
  #text(fill: rgb(accent), weight: "semibold", size: 0.78em)[#title]
  #h(0.35em)
  #text(fill: rgb("3f3932"), size: 0.9em)[#body]
]

#let comment(body) = note-comment-box(body)
#let annotation(body) = note-comment-box(body, accent: "7a4b2d", tint: "f3e6dc", title: "Note")
#let draft-note-box(label, body, accent: "b0007a", tint: "f8e7f2") = note-comment-box(
  body,
  accent: accent,
  tint: tint,
  title: label,
)
#let note-side-width = 10.6em
#let note-margin-width = 5.4em
#let note-margin-dx = 100% + 1.1em
#let note-margin-line-dx = 100% - 1.15em
#let note-side-column-x = 16.0cm
#let note-draft-pin-counter = counter("note-draft-pin")
#let note-pinned-margin-note(pin, label, body, accent: "b0007a", tint: "f8e7f2") = {
  note-pinit-side-note(
    pin,
    draft-note-box(label, body, accent: accent, tint: tint),
    side: "right",
    column-x: note-side-column-x,
    width: note-side-width,
    stroke: 0.55pt + rgb(accent),
    arrow-fill: rgb(accent),
  )
}
#let note-margin-arrow(label, body, accent: "b0007a", tint: "f8e7f2") = {
  note-draft-pin-counter.step()
  context {
    let pin = "note-draft-" + str(note-draft-pin-counter.get().first())
    note-pin(pin)
    note-pinned-margin-note(pin, label, body, accent: accent, tint: tint)
  }
}
#let draft-note(label, body, pin: none, accent: "b0007a", tint: "f8e7f2") = {
  if pin == none {
    note-margin-arrow(label, body, accent: accent, tint: tint)
  } else {
    note-pinned-margin-note(pin, label, body, accent: accent, tint: tint)
  }
}
#let TODO(pin: none, body) = draft-note("TODO", body, pin: pin, accent: "8f3430", tint: "f4dfdc")
#let todo(pin: none, body) = TODO(pin: pin, body)
#let mee(pin: none, body) = draft-note("ME", body, pin: pin)
#let eme(pin: none, body) = draft-note("EM", body, pin: pin)
#let emtodo(pin: none, body) = draft-note("EM TODO", body, pin: pin, accent: "8f3430", tint: "f4dfdc")
#let pinit-todo(pin, body) = todo(pin: pin, body)
#let pintodo(pin, body) = pinit-todo(pin, body)
#let framed(body) = block(
  width: 100%,
  stroke: 0.65pt + note-rule,
  inset: (x: 0.78em, y: 0.58em),
  breakable: true,
)[body]
#let frame(body) = framed(body)
#let note-floating-side(
  body,
  dx: 24em,
  width: note-side-width,
  title: "Side Note",
  side: "right",
  accent: "335f91",
  tint: "e4edf8",
) = {
  note-draft-pin-counter.step()
  context {
    let pin = "note-side-" + str(note-draft-pin-counter.get().first())
    note-pin(pin)
    note-pinit-side-note(
      pin,
      note-comment-box(body, accent: accent, tint: tint, title: title),
      side: side,
      column-x: note-side-column-x,
      width: width,
      left-width: width,
      stroke: 0.55pt + rgb(accent),
      arrow-fill: rgb(accent),
    )
  }
}
#let marginal(body) = note-floating-side(
  body,
  dx: -6.1em,
  width: note-margin-width,
  title: "Margin",
  side: "left",
)
#let margin-note(body) = marginal(body)
#let mnote(body) = marginal(body)
#let sidecomment(body) = note-floating-side(body, dx: 24em, title: "Side Note", side: "right")
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

// --- Status chips (inline, e.g. `- #task-todo[Write proof of Lemma 3]`).
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

#let task-todo(due: none, scheduled: none, priority: none, repeat: none, recurrence: none, body) = task-line(
  "todo", "9b3b37", "f4dfdc", body,
  due: due, scheduled: scheduled, priority: priority, repeat: repeat, recurrence: recurrence,
)
#let task-doing(due: none, scheduled: none, priority: none, repeat: none, recurrence: none, body) = task-line(
  "doing", "9b6017", "fbe9c8", body,
  due: due, scheduled: scheduled, priority: priority, repeat: repeat, recurrence: recurrence,
)
#let task-waiting(due: none, scheduled: none, priority: none, repeat: none, recurrence: none, body) = task-line(
  "waiting", "5f6c7b", "eceae4", body,
  due: due, scheduled: scheduled, priority: priority, repeat: repeat, recurrence: recurrence,
)
#let task-done(due: none, scheduled: none, priority: none, repeat: none, recurrence: none, body) = task-line(
  "done", "2f6f42", "e5f3df", body,
  due: due, scheduled: scheduled, priority: priority, repeat: repeat, recurrence: recurrence,
  muted: true,
)
#let task-cancelled(due: none, scheduled: none, priority: none, repeat: none, recurrence: none, body) = task-line(
  "cancelled", "8a8077", "ece8e0", body,
  due: due, scheduled: scheduled, priority: priority, repeat: repeat, recurrence: recurrence,
  muted: true,
)
#let doing(pin: none, body) = draft-note("DOING", body, pin: pin, accent: "9b6017", tint: "fbe9c8")
#let waiting(pin: none, body) = draft-note("WAITING", body, pin: pin, accent: "5f6c7b", tint: "eceae4")
#let done(pin: none, body) = draft-note("DONE", body, pin: pin, accent: "2f6f42", tint: "e5f3df")
#let cancelled(pin: none, body) = draft-note("CANCELLED", body, pin: pin, accent: "8a8077", tint: "ece8e0")
#let pinit-doing(pin, body) = doing(pin: pin, body)
#let pinit-waiting(pin, body) = waiting(pin: pin, body)
#let pinit-done(pin, body) = done(pin: pin, body)
#let pinit-cancelled(pin, body) = cancelled(pin: pin, body)
