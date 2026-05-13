#import "@preview/touying:0.7.3": *
#import themes.simple: *

#set document(title: "{{title}}", author: "{{author}}", date: datetime.today())
#show: simple-theme.with(aspect-ratio: "16-9")

= {{title}}

{{author}} \
{{date}}

== Problem

{{cursor}}

- What is the audience currently missing?
- Why does it matter now?

#pause

- What should they understand after this talk?

== Core Idea

- Claim:
- Construction:
- Intuition:

== Evidence

- Theorem / experiment:
- Example:
- Limitation:

== Diagram

#slide[
  #import "@preview/fletcher:0.5.8" as fletcher: diagram, edge

  #diagram(cell-size: 15mm, $
    A edge(f, ->) & B \
    C edge(g, ->) & D
  $)
]

== Takeaway

- What should be remembered?
- What should happen next?
