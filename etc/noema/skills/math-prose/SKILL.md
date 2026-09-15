---
name: math-prose
description: Draft, revise, and audit precise English mathematical prose for research papers, theses, technical reports, and proofs while preserving supplied formulas, notation, assumptions, and claim strength by default. Use when writing or improving notation definitions, equation-adjacent prose, derivations, assumptions, mappings, constructions, inequalities, optimization problems, dynamical systems, theorem statements, proofs, or mathematical interpretations, especially when prose is repetitive, overuses forms of "be," confuses define/denote/given by, or uses inference verbs without sufficient logical support.
---

# Math Prose

Write mathematical prose by matching language to the mathematical action being
performed. Improve variety through semantic accuracy and information structure,
not synonym rotation.

## Core Workflow

1. **Protect the mathematical contract.** Record every symbol, equation,
   operator, sign, index, quantifier, condition, unit, citation, and claim
   strength that must remain unchanged. Do not silently repair mathematics.
   If a requested relation is unspecified, retain a placeholder or flag the
   gap instead of instantiating a convenient model. Do not infer unspoken
   linear or affine structure from convexity: call the nearest-point map onto
   a general closed convex set a metric projection, even when its metric comes
   from an inner product; reserve orthogonal projection for a linear or affine
   subspace with the corresponding geometry.
2. **Classify the local mathematical behavior.** Assign one primary behavior
   from [references/behavior-taxonomy.md](references/behavior-taxonomy.md) to
   each equation-adjacent sentence. Add a secondary behavior only when the
   sentence genuinely performs two actions.
3. **Identify the discourse position.** Decide whether the prose introduces a
   display, defines symbols in a `where` clause, advances a derivation,
   interprets a result, states a theorem, or closes an argument. Read
   [references/equation-discourse.md](references/equation-discourse.md) for
   equation and paragraph patterns.
4. **Select a construction by meaning.** Choose verbs and grammatical forms
   licensed by the behavior. Retain `is` or `are` for an identity, property,
   membership, or state when that is the clearest relation.
5. **Check logical force.** Read
   [references/proof-and-claim-language.md](references/proof-and-claim-language.md)
   when using `implies`, `deduce`, `therefore`, `equivalently`, `prove`,
   `guarantee`, or related result language.
6. **Revise structure before vocabulary.** Group compatible definitions,
   change equation-to-prose order, or split overloaded sentences before
   replacing repeated predicates.
7. **Run a local repetition gate.** Compare sentence frames in the current
   paragraph and neighboring equation-introduction units. Keep a repeated frame
   only when deliberate parallelism clarifies parallel mathematics. Otherwise,
   revise one occurrence by changing its discourse function, information order,
   equation placement, or sentence structure. Do not rotate synonyms, vary
   stable technical terms, or weaken precision merely to sound different. If no
   equally precise alternative exists, retain the repetition.
8. **Audit the result.** Verify first-use definitions, type declarations,
   denotation direction, equivalence, assumptions, edge cases, and preserved
   mathematics. Confirm that every stronger verb is justified by the visible
   argument or cited result.

## Reference Routing

- Read [references/behavior-taxonomy.md](references/behavior-taxonomy.md) for
  semantic behavior codes, recommended constructions, and misuse boundaries.
- Read [references/equation-discourse.md](references/equation-discourse.md) for
  definition blocks, displayed equations, derivation chains, dynamical systems,
  optimization problems, and post-equation interpretation.
- Read
  [references/proof-and-claim-language.md](references/proof-and-claim-language.md)
  for assumptions, theorem statements, proof moves, implication strength,
  existence, uniqueness, bounds, and convergence claims.
- Read [references/corpus-method.md](references/corpus-method.md) when collecting
  papers, annotating examples, extending the phrase inventory, or deciding
  whether a pattern is sufficiently supported.

## Non-Negotiable Distinctions

- Use `denote` to assign a symbol to an object. Keep the direction explicit: a
  symbol denotes an object, and an object is denoted by a symbol.
- Use `define` for a stipulation introduced by the manuscript.
- Use `given by` to supply an explicit expression for an object already under
  discussion. Do not use it as decorative variation for every definition.
- Use `write`, `express`, `rewrite`, or `recast` only when the displayed form is
  mathematically equivalent under the stated conditions.
- Use `obtain` or `derive` for a supported calculation. Use `deduce`, `imply`,
  `therefore`, or `it follows that` only for a valid logical consequence.
- Use `represent`, `encode`, `collect`, `measure`, and `parameterize` for actual
  mathematical or scientific roles. They do not establish equality by
  themselves.
- Preserve hypotheses, domains, codomains, admissible spaces, regularity,
  nonzero conditions, boundary cases, and approximation qualifiers.
- Do not vary terminology when the field requires one stable technical term.
- Do not fabricate a derivation, proof, citation, theorem, or empirical result.

## High-Risk Composition Checks

Run the relevant checks when several mathematical actions occur in one passage:

- Preserve nested quantifier order and state what each constant or witness may
  depend on.
- Keep convergence in distribution, in probability, almost surely, and with
  high probability distinct. A vanishing tail bound does not by itself imply
  almost-sure convergence.
- Make piecewise cases exhaustive and unambiguous. In proofs, expose the base
  case and inductive step, both directions of an equivalence, or the exact
  contradiction rather than naming the architecture alone.
- Distinguish an infimum from an attained minimum, stationarity from
  optimality, weak from strong duality, and KKT conclusions from the
  hypotheses that license them.
- Treat inverse images, `argmin`, and general projections as set-valued when
  existence or uniqueness is not established. Reserve orthogonal projection
  for the appropriate linear or affine setting.
- Keep theorem conclusions, finite empirical observations, and corrected or
  superseded proof evidence in separate evidence modes.
- Preserve initialization sources, sequential versus simultaneous update
  order, acceptance branches, tolerances, and stopping conditions in algorithm
  prose. Do not add an update or guarantee that was not supplied.
- In statistical inference, separate confidence coverage, realized intervals,
  p-values, prechosen test levels, type-I and type-II error probabilities, and
  power. Do not turn a frequentist probability into a posterior probability.
- Before interchanging limits, integrals, expectations, derivatives, or
  iterated integrals, name the theorem and verify the ambient measure-space,
  measurability, definedness of the displayed operations, and branch-specific
  domination, monotonicity, integrability, or regularity hypotheses. For a
  parameter-dependent integral, make any local parameter neighborhood and
  integrable spatial envelope uniform over that neighborhood explicit. Missing
  hypotheses show that the named theorem does not license the step; they do
  not by themselves prove the interchange impossible.
- For PDEs, retain the source's classical, strong, weak, distributional, or
  viscosity solution notion, including its function space, test class, trace
  interpretation, and sense of equality. Use `well posed` only when existence,
  uniqueness, and continuous dependence are all stated in named topologies.
- In numerical analysis, separate problem conditioning from algorithmic
  stability, forward from backward error, a priori from a posteriori
  estimates, and proved rates from finite-run observed slopes. Keep
  discretization, algebraic-solve, and rounding errors distinct. If only these
  categories are supplied, keep the prose symbolic or conditional; do not
  instantiate a canonical linear system, finite-element estimate, norm,
  perturbation model, or estimator formula that the input did not provide.

## Revision Modes

### Draft from mathematics

Infer the prose architecture from the supplied equations and author notes.
State the role of an object before dense manipulation when the reader needs that
orientation. Define each nonstandard symbol at or before first semantic use.

### Revise existing prose

Preserve the author's mathematical content and notation. Diagnose each change
as one of: semantic correction, logical-strength correction, first-use repair,
structural variation, local de-duplication, or grammar repair. Avoid broad
rewrites when a local repair is sufficient.

### Audit without rewriting

Report ambiguous definitions, missing types, unsupported inference markers,
false equivalence, unclear antecedents, repeated sentence frames, and orphaned
equations. Treat repetition counts as review signals rather than automatic
errors.

## Corpus Extension

Follow [references/corpus-method.md](references/corpus-method.md). Store source
metadata, local observations, and synthesized patterns as distinct records.
Consult [references/corpora/math-core.jsonl](references/corpora/math-core.jsonl)
as the transferable evidence base, not as a phrase list to copy mechanically.
Use `research-article` observations to learn publication-state compression and
`textbook` observations to learn fuller definition, construction, derivation,
and proof exposition. Compress textbook scaffolding before producing manuscript
prose, and do not mistake free access for permission to redistribute source text.
When a user asks for concrete words or phrase inventory, return behavior-tagged
`pattern.constructions` with their relevant boundaries, not observation metadata
or source quotations.
On a `domain/<field>` branch, load the matching domain corpus only when the task
belongs to that field. Domain patterns supplement rather than replace the core.
Run `python3 scripts/validate_corpus.py <core.jsonl> [domain.jsonl]` before
accepting corpus changes. Keep copyrighted source text out of the public corpus
by default. Use [references/core-evaluation.md](references/core-evaluation.md)
as the forward-test protocol when core patterns or the writing workflow change.
For a high-risk pattern boundary, add a validated `boundary_cases` entry with a
counterexample, misuse, or near-synonym challenge and link it to source
observations, forward-test evaluations, or both.

## Output

Return clean manuscript-ready prose by default. When the source relation is
ambiguous, preserve it and flag the ambiguity instead of choosing a stronger
mathematical claim. Provide a short change rationale only when requested or
when the distinction affects mathematical meaning.
