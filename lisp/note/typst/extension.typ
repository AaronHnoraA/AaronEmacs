// extension.typ --- Optional third-party Typst extensions for notes
//
// Keep package imports and package-specific configuration here.  The core
// note style should depend only on the small `note-extensions' entry point and
// on stable aliases defined below.

#import "@preview/codly:1.3.0": *
#import "@preview/codly-languages:0.1.10": *
#import "@preview/algorithmic:1.0.7" as algorithmic
#import "@preview/fletcher:0.5.8" as fletcher
#import "@preview/finite:0.5.1" as finite
#import "@preview/quill:0.7.2" as quill
#import "@preview/cetz:0.5.2" as cetz
#import "@preview/pinit:0.2.2" as pinit

#let note-code-languages = codly-languages

#let note-extensions(body) = {
  show: codly-init.with()
  codly(
    languages: note-code-languages,
    display-icon: true,
    radius: 2pt,
    stroke: 0.45pt + rgb("c8c1b4"),
    zebra-fill: rgb("f3f0ea"),
  )
  body
}

// Pseudocode.  `algorithmic' remains available for the full DSL, while these
// aliases cover the common entry points.
#let pseudo = algorithmic.algorithm
#let pseudo-figure = algorithmic.algorithm-figure
#let pseudo-style = algorithmic.style-algorithm

// Diagrams and automata.
#let diagram = fletcher.diagram
#let node = fletcher.node
#let edge = fletcher.edge
#let automaton = finite.automaton

// Quantum circuits.
#let quantum-circuit = quill.quantum-circuit
#let qgate = quill.gate
#let qctrl = quill.ctrl
#let qtarg = quill.targ
#let qlstick = quill.lstick
#let qrstick = quill.rstick
#let quill-help = quill.help

// General drawing.  Use `note-canvas' for small inline scientific diagrams;
// for advanced drawings, import `cetz.draw' locally inside the canvas body.
#let note-canvas = cetz.canvas

// Relative placement by pins.  These are useful for lightweight annotations in
// notes and for slides that import the note extension explicitly.
#let note-pin = pinit.pin
#let note-pinit = pinit.pinit
#let note-pinit-highlight = pinit.pinit-highlight
#let note-pinit-place = pinit.pinit-place
#let note-pinit-arrow = pinit.pinit-arrow
#let note-pinit-line = pinit.pinit-line
#let note-simple-arrow = pinit.simple-arrow
#let note-absolute-place = pinit.absolute-place
#let note-pinit-capsule(
  body,
  width: 12em,
  fill: rgb("fbfaf7"),
  stroke: 0.55pt + rgb("5f6c7b"),
) = block(
  width: width,
  fill: fill,
  stroke: stroke,
  radius: 999pt,
  inset: (x: 0.78em, y: 0.42em),
  breakable: false,
)[
  #text(size: 0.86em, fill: rgb("3f3932"))[#body]
]
#let note-pinit-side-note(
  pin-name,
  body,
  side: "right",
  column-x: 16.0cm,
  left-column-x: 0.7cm,
  line-target-x: none,
  width: 12em,
  left-width: 5.4em,
  pin-dx: 2pt,
  pin-dy: 4pt,
  body-dy: -2pt,
  line-gap: 4pt,
  stroke: 0.55pt + rgb("5f6c7b"),
  arrow-fill: rgb("5f6c7b"),
  draw-line: true,
) = {
  note-pinit(
    pin-name,
    callback: pos => {
      let body-x = if side == "left" { left-column-x } else { column-x }
      let body-width = if side == "left" { left-width } else { width }
      let pin-x = pos.x + pin-dx
      let pin-y = pos.y + pin-dy
      let target-x = if line-target-x == none {
        if side == "left" { left-column-x + 1.65cm } else { column-x - line-gap }
      } else {
        line-target-x
      }
      if draw-line {
        let arrow-end-x = if side == "left" {
          -1 * calc.max(0pt, pin-x - target-x)
        } else {
          calc.max(0pt, target-x - pin-x)
        }
        note-absolute-place(dx: pin-x, dy: pin-y)[
          #note-simple-arrow(
            start: (0pt, 0pt),
            end: (arrow-end-x, body-dy + 10pt - pin-dy),
            fill: arrow-fill,
            thickness: 0.045em,
            arrow-width: 5,
            arrow-height: 5,
            inset: 0.7,
          )
        ]
      }
      note-absolute-place(dx: body-x, dy: pos.y + body-dy)[
        #block(width: body-width)[#body]
      ]
    },
  )
}
#let note-pinit-point-to(
  pin-name,
  body,
  pin-dx: 2pt,
  pin-dy: 4pt,
  body-dx: 0.45em,
  body-dy: 0pt,
  offset-dx: 18em,
  offset-dy: -2pt,
  double: false,
  side: "right",
  column-x: 16.0cm,
  left-column-x: 0.7cm,
  line-target-x: none,
  capsule: true,
  capsule-width: 12em,
  capsule-fill: rgb("fbfaf7"),
  capsule-stroke: 0.55pt + rgb("5f6c7b"),
  stroke: 0.55pt + rgb("5f6c7b"),
  arrow-fill: rgb("5f6c7b"),
  ..args,
) = note-pinit-side-note(
  pin-name,
  if capsule {
    note-pinit-capsule(
      body,
      width: capsule-width,
      fill: capsule-fill,
      stroke: capsule-stroke,
    )
  } else {
    body
  },
  side: side,
  column-x: column-x,
  left-column-x: left-column-x,
  line-target-x: line-target-x,
  width: capsule-width,
  pin-dx: pin-dx,
  pin-dy: pin-dy,
  body-dy: offset-dy + body-dy,
  stroke: stroke,
  arrow-fill: arrow-fill,
)
#let note-pinit-point-from(
  pin-name,
  body,
  pin-dx: 2pt,
  pin-dy: 4pt,
  body-dx: 0.45em,
  body-dy: 0pt,
  offset-dx: 18em,
  offset-dy: -2pt,
  double: false,
  side: "right",
  column-x: 16.0cm,
  left-column-x: 0.7cm,
  line-target-x: none,
  capsule: true,
  capsule-width: 12em,
  capsule-fill: rgb("fbfaf7"),
  capsule-stroke: 0.55pt + rgb("5f6c7b"),
  stroke: 0.55pt + rgb("5f6c7b"),
  arrow-fill: rgb("5f6c7b"),
  ..args,
) = note-pinit-side-note(
  pin-name,
  if capsule {
    note-pinit-capsule(
      body,
      width: capsule-width,
      fill: capsule-fill,
      stroke: capsule-stroke,
    )
  } else {
    body
  },
  side: side,
  column-x: column-x,
  left-column-x: left-column-x,
  line-target-x: line-target-x,
  width: capsule-width,
  pin-dx: pin-dx,
  pin-dy: pin-dy,
  body-dy: offset-dy + body-dy,
  stroke: stroke,
  arrow-fill: arrow-fill,
)
