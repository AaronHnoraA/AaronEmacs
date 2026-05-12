#import "@preview/touying:0.7.3": *
#import themes.simple: *

#let note-slide-theme = simple-theme.with(aspect-ratio: "16-9")

#let note-title-slide(title, subtitle: none, author: none, date: datetime.today().display()) = {
  title-slide[
    = #title
    #if subtitle != none [#subtitle]
    #if author != none [#author]
    #date
  ]
}

#let section-slide(title) = slide[
  = #title
]

#let agenda(items) = slide[
  = Agenda

  #for item in items [
    - #item
  ]
]

#show: note-slide-theme
