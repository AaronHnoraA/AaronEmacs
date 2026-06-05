// note-code.typ --- Embed tagged source regions in Typst notes
//
// Lean may omit `path` when `note-path` is configured.  Other languages must
// name a project-root-relative source path explicitly.

#let lean = "lean"

#let _note-code-clean-path(path) = {
  let clean = path.trim().replace(regex("^/+"), "")
  "/" + clean
}

#let _note-code-lean-path(note-path) = {
  let clean = note-path.trim().replace(regex("^/+"), "")
  let stem = clean.replace(regex("\.[^.]+$"), "")
  "/.lean/" + stem + ".lean"
}

#let _note-code-marker-tag(line) = {
  let aaron = "@aaronnote "
  let generic = "@note-code "
  if line.contains(aaron) {
    line.split(aaron).last().trim()
  } else if line.contains(generic) {
    line.split(generic).last().trim()
  } else {
    none
  }
}

#let _note-code-region(source, tag) = {
  let lines = source.split("\n")
  let start = none
  let end = lines.len()

  for (index, line) in lines.enumerate() {
    let marker = _note-code-marker-tag(line)
    if start == none and marker == tag {
      start = index + 1
    } else if start != none and marker != none {
      end = index
      break
    }
  }

  if start == none {
    panic("note-code tag `" + tag + "` was not found")
  }
  lines.slice(start, end).join("\n").trim()
}

#let note-code(
  lang: lean,
  path: none,
  note-path: none,
  tag: none,
  body,
) = {
  let language = str(lang)
  let region-tag = if tag != none { str(tag) } else { body.text.trim() }
  let source-path = if path != none {
    _note-code-clean-path(str(path))
  } else if language == lean and note-path != none {
    _note-code-lean-path(str(note-path))
  } else if language == lean {
    panic("note-code Lean mirror requires note-path configuration")
  } else {
    panic("note-code path is required for language `" + language + "`")
  }

  raw(
    _note-code-region(read(source-path), region-tag),
    block: true,
    lang: language,
  )
}
