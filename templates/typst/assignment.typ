#import "/_typst/assignment.typ": *

#show: body => assignment-theme(
  institution: "UNSW",
  course-code: "COURSE",
  term: "TERM",
  title: "{{title}}",
  date: "{{date}}",
  student-name: "{{author}}",
  author-url: none,
  student-id: "z0000000",
  affiliation: "UNSW",
)[
  #body
]

= {{title}}

{{cursor}}
