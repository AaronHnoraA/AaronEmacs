#import "/_typst/note.typ": *
#show: note-entry

#metadata((
  kind: "note",
  id: "{{timestamp}}-{{slug_title}}",
  title: "{{title}}",
  date: "{{date}}",
  tags: ("math", "tcs", "proof", "sketch", "draft"),
  aliases: (),
)) <note>

= {{title}}

== Statement
{{cursor}}

#theorem[

]

== Intuition
- What should be true:
- Why it should be true:
- What could go wrong:

== Proof Skeleton
1. Reduce to:
2. Show invariant:
3. Apply:
4. Conclude:

== Key Lemmas
- Lemma A:
- Lemma B:
- Lemma C:

== Full Proof Placeholder
#proof[

]

== Reader Check
- Does every parameter have a quantifier?
- Is the error probability stated?
- Are all asymptotic regimes explicit?
