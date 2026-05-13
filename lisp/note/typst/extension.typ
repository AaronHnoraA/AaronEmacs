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
