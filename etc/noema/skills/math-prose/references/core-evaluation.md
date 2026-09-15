# Core Corpus Evaluation

## Purpose

This document records the manual part of the mathematical-core readiness gate.
The JSONL validator checks corpus structure and coverage; these forward tests
check whether the skill preserves mathematical meaning when it actually writes.

## Protocol

On 2026-07-21, three independent forward-test runs received short mathematical
writing tasks without a model answer. Each run had to revise the prose and then
audit its own output for formula preservation, notation, scope, conditions, and
claim strength. A case passes only if the revision:

1. preserves every supplied formula and symbol;
2. selects language matching the mathematical behavior;
3. retains assumptions, direction, frame, and approximation status; and
4. introduces no unsupported existence, uniqueness, convexity, stability, or
   equivalence claim.

## Cases and results

| Area | Blind input behavior | Required distinction | Result |
|---|---|---|---|
| Definition | Define a residual from `r(x):=Ax-b` with stated dimensions | stipulative definition versus denotation or ordinary equality | PASS |
| Type and mapping | Describe `R∈SO(3)`, `v_I=Rv_B`, and `v_B=R^Tv_I` | mapping direction, domain convention, and inverse relation | PASS |
| Proof claim | Revise a local Lyapunov derivative bound followed by a global-stability overclaim | local consequence versus unsupported global theorem | PASS |
| Derivation | Explain `exp(hA)=I+hA+O(h^2)` and a discrete update | asymptotic expansion versus exact equality | PASS |
| Optimization | State `arg min` with data, decision variable, objective, and constraint | mathematical roles without invented convexity or existence | PASS |
| Computation | Initialize `R_c(0)=R(0)` and propagate a matrix recurrence | measured-state initialization versus fixed default; ordered update | PASS |

The runs correctly used definition, denotation, property, consequence,
approximation, optimization, initialization, and update language. In particular,
they rejected the unsupported global-stability claim and retained the
second-order remainder before presenting the first-order approximation.

## Local repetition regression cases

The following cases were added after a real revision exposed the same repair
template in adjacent equation introductions. They were manually reviewed in the
same editing session, so they are regression checks rather than new independent-
agent evidence.

| Case | Input condition | Required behavior | Review |
|---|---|---|---|
| Unintended duplicate frame | Two nearby tuple introductions both use `collects ... in the ordered tuple` | Distinguish assembly of known measurements from stipulative definition by changing information structure, not by rotating synonyms | PASS (manual) |
| Intentional parallelism | Two inverse maps or symmetric assumptions use matched sentence frames | Retain the parallel form when it makes the mathematical correspondence easier to see | PASS (manual) |
| Precision fallback | Repeated wording carries a stable property or technical term and no equally precise alternative is available | Keep the wording or group the statements; do not introduce a vague substitute solely for variety | PASS (manual) |

Repeat these checks whenever the revision workflow or equation-discourse rules
change. A successful revision must detect the local frame repetition, explain
whether it is functional, and preserve the mathematical behavior and claim
strength of every changed sentence.

## Readiness decision

The core passes the current targeted forward-test gate for declarations,
constructions, transformations, proof claims, optimization, dynamics,
convergence, comparison, and bounded interpretation. This is not evidence that
every mathematical subfield or house style has been exhausted. Repeat the
evaluation when the taxonomy, core patterns, or writing workflow changes
materially.

On 2026-07-21, 16 further observations were checked against five existing
anchors and added without changing any recommended construction, boundary, or
workflow rule. The added evidence completed independent, cross-disciplinary
support for the nine previously provisional patterns and introduced explicit
coverage of piecewise displays, an additional `where`-clause role, and a proof
calculation step. Structural validation and the existing validator unit tests
were rerun; this evidence update is not counted as a new independent-agent
forward test.

## 2026-07-22 concrete-frame expansion tests

After 29 concrete frames were added across 19 behaviors, eight fresh task
sequences exercised the updated Skill without model answers or intended frames.
The first construction run invented an unsupplied harmonicity relation, and the
first dynamics run invented an unsupplied feedback model. Two intermediate
construction reruns fixed the missing relation but mislabeled a metric
projection onto a general closed convex set as orthogonal. All failures were
preserved; the missing-relation and projection guards were strengthened, and
the final reruns passed.

| Area | Blind input behavior | Required distinction | Result |
|---|---|---|---|
| Construction and representation, pre-fix | Quotient by bilinearity relations, metric projection, additive error decomposition, and an unspecified harmonicity calculation | preserve missing mathematics instead of inventing a conditional-expectation identity, one-step relation, and notation | FAIL: invented the omitted relations and symbols |
| Construction, first two repair attempts | The same facts with the harmonicity inputs still omitted | retain a symbol-free conditional statement and distinguish a convex-set metric projection from a subspace orthogonal projection | FAIL: missing mathematics was preserved, but both runs mislabeled the projection |
| Construction, final rerun | The same quotient, projection, decomposition, and omitted harmonicity inputs | use metric-projection language and retain a conditional, symbol-free conclusion | PASS |
| Transformation and inference | One-way reduction, termwise integration without differentiation permission, Galerkin and inexact surrogates, a uniform constant, and conditional uniqueness | sufficiency versus equivalence, operation-specific asymptotic permission, no invented numerical guarantee, quantifier order, and uniqueness without existence | PASS |
| Dynamics and interpretation, pre-fix | Semigroup generator, unspecified feedback-induced law, terminal backward initialization, almost-sure random limit, invariant-set convergence, dependency, comparison, and scalar-only scope | preserve missing mathematics instead of instantiating a convenient model | FAIL: invented an open-loop equation, feedback law, and initial state |
| Missing-relation rerun | The same semigroup, omitted feedback equation, terminal initialization, and qualified convergence facts | retain a placeholder and all supplied qualifiers without inventing symbols | PASS |
| Revised C1 and F2 frames | Exact substitution plus term collection, followed by a normative likelihood-principle statement | calculation rather than proof force; prescriptive `should depend only on` rather than factual dependence | PASS after refusing the first under-specified input and receiving the formulas |

These are scoped functional checks of the newly expanded behaviors. They do not
formally verify every construction frame or establish universal stylistic
coverage. The verbatim prompts, outputs, self-audits, and pre-fix failure are
preserved in the
[round-four artifact record](core-evaluation-round4-artifacts.md).

## 2026-07-23 boundary-composition tests

The fifth round targeted distinctions that are easy to preserve in isolation
but easy to lose in longer mathematical passages. Three fresh writing runs
received seven tasks without model answers; three different reviewers audited
the resulting text against the mathematical contracts.

| Case | Composition under test | Required boundary | Result |
|---|---|---|---|
| CF5-1 | Nested quantifiers, a vanishing tail bound, and three convergence modes | preserve constant dependence; infer convergence in probability only; keep distributional and almost-sure claims separate | PASS |
| CF5-2 | Piecewise definition, induction, equivalence, and contradiction | exhaustive cases; base and step; both directions; explicit contradiction; no invented calculation | PASS |
| CF5-3 | Infimum, stationarity, weak duality, and KKT | distinguish attainment and minimality; retain convexity and Slater hypotheses; add no existence or uniqueness | PASS |
| CF5-4 | Set-valued inverse, `argmin`, metric projection, and subspace projection | retain empty or multivalued cases; state uniqueness hypotheses; reserve orthogonal terminology | PASS |
| CF5-5 | Formal theorem, four-run benchmark, and publisher erratum | scope the theorem to its assumptions, the experiment to the runs, and the invalid proof step as superseded | PASS |
| CF5-6 | Carried initialization, sequential updates, inexact solve, and accept-or-retain branch | preserve state source and update order; invent no counter update, rate, or global guarantee | PASS |
| CF5-7 | Quotient-induced map, special-case relation, and one-instance comparison | verify representative independence; avoid false equivalence and universal superiority | PASS |

All exact outputs passed independent review. Two initial audit inputs, for
CF5-2 and CF5-5, accidentally abbreviated formulas that the writing outputs had
preserved; those audit-input failures were marked invalid and the exact outputs
were re-audited successfully. This correction remains visible in the
[round-five artifact record](core-evaluation-round5-artifacts.md).

The round supports the newly evidence-linked high-risk boundaries and long-chain
composition checks. It does not establish completeness across every structure,
subfield, or domain overlay.

## 2026-07-24 inference and analysis boundary tests

The sixth round targeted four newly expanded areas. Four fresh writing runs
received mathematical contracts without model answers, and independent
reviewers audited the resulting prose.

| Case | Area | Required boundary | Result |
|---|---|---|---|
| CF6-1 | Frequentist inference | separate procedure coverage, realized interval, p-value, prespecified level, and power at a named alternative | PASS |
| CF6-2 | Measure convergence and analytic interchange | separate a.e. and \(L^1\) convergence; state complete theorem hypotheses; distinguish an unavailable interchange license from impossibility | PASS after two rule repairs and a strictly blind rerun |
| CF6-3 | PDE solution classes | retain weak formulation, trace semantics, local a.e. regularity, time topology, and continuous dependence before well-posedness | PASS |
| CF6-4 | Numerical analysis | separate conditioning and stability, forward and backward error, a priori and a posteriori claims, error sources, and observed versus proved rates without inventing a canonical setting | PASS after a formula-supplied strictly blind rerun |

The initial CF6-2 output omitted measure-space and measurability details and
overstated the consequence of missing Tonelli/Fubini hypotheses. Its first
repair still omitted the product domain and parameter-uniform derivative
envelope. Both failures were retained, the corresponding guards were
strengthened, and the final strictly blind output passed exact-text review.
One intended rerun was discarded because its writer voluntarily disclosed
opening this evaluation record. A later abbreviated review input was also
marked invalid and replaced by review of the exact output.

The initial CF6-4 output preserved the requested distinctions but invented a
linear-system perturbation model and finite-element formulas that its
category-level prompt had not supplied. The new guard requires symbolic or
conditional prose when those inputs are missing. A strictly blind rerun
received the exact formulas and preserved them without adding another setting;
an abbreviated first re-audit was marked invalid, and exact contract-to-output
review passed.

The prompts, outputs, self-audits, failures, repair history, and independent
reviews are preserved in the
[round-six artifact record](core-evaluation-round6-artifacts.md).
