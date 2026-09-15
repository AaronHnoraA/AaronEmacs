# Mathematical Behavior Taxonomy

## Contents

- [How to classify](#how-to-classify)
- [A. Declaration and scope](#a-declaration-and-scope)
- [B. Representation and construction](#b-representation-and-construction)
- [C. Transformation and equivalence](#c-transformation-and-equivalence)
- [D. Inference and results](#d-inference-and-results)
- [E. Dynamics and computation](#e-dynamics-and-computation)
- [F. Interpretation and comparison](#f-interpretation-and-comparison)
- [Local variation rules](#local-variation-rules)

## How to classify

Classify what the sentence *does mathematically*, not which verb it happens to
contain. Assign one primary code. Add a secondary code only if removing either
action would make the sentence incomplete.

For every case, record:

1. the mathematical relation;
2. the participating objects and their types;
3. the conditions under which the relation holds;
4. whether the statement is a stipulation, equality, approximation,
   transformation, inference, theorem, or interpretation;
5. the discourse position relative to the equation.

## A. Declaration and scope

| Code | Behavior | Licensed constructions | Boundary |
|---|---|---|---|
| `A0` | State identity, property, or status | `$x$ is ...`, `$x$ equals ...`, `$f$ is continuous`, `$A$ is symmetric` | A form of `be` is often optimal. Do not force an active verb into a property statement. |
| `A1` | Assign a symbol or name | `let $x$ denote ...`, `denote ... by $x$`, `write $x$ for ...` | Naming does not define the object's internal formula. Keep denotation direction correct. |
| `A2` | Stipulate a definition | `define $x:=...$`, `define $F$ by ...`, `$x$ is defined as ...` | Use only when the manuscript creates or adopts the definition. For a statistical quantity or PDE solution notion, state its probability model or function space, test class, regularity, and sense of equality. |
| `A3` | Declare type, space, or membership | `$x\in\mathcal X$`, `$x$ lies in ...`, `$F:\mathcal X\to\mathcal Y$`, `$S:\mathcal X\rightrightarrows\mathcal U$` | Give the actual space or signature and distinguish single-valued from set-valued maps. A physical description is not a type declaration. |
| `A4` | Introduce data or assumptions | `given ...`, `suppose ...`, `assume ...`, `for a prescribed ...`, `under ...` | `Given` introduces available data or conditions. It is not a synonym for `defined`. |
| `A5` | Establish scope or quantification | `for every ...`, `there exists ...`, `fix ...`, `choose ...`, `for $i=1,\ldots,n$` | Preserve quantifier order and state dependencies, such as whether $C(\alpha)$ is uniform in $u$, $n$, or a mesh parameter $h$. `Choose` may require a preceding existence result. |

## B. Representation and construction

| Code | Behavior | Licensed constructions | Boundary |
|---|---|---|---|
| `B1` | Supply an explicit formula | `$x$ is given by ...`, `$x$ takes the form ...`, `$x$ can be written as ...` | The object is already identified. `Can be written as` asserts an admissible representation, usually an equivalence. Piecewise formulas need exhaustive, nonambiguous cases. |
| `B2` | Construct or assemble an object | `construct`, `form`, `assemble`, `augment`, `stack` | Name the operation that actually creates the object and preserve ordering. |
| `B3` | Map or transform | `maps ... to ...`, `transforms`, `pushes forward`, `pulls back`, `associates ... with ...` | State direction, domain, codomain, and frame when relevant. For quotient-induced maps, verify representative independence; for set-valued inverses, retain set-valuedness. Use trace-space boundary values only under an applicable trace theorem; otherwise retain the supplied Sobolev-space or weak formulation. |
| `B4` | Decompose or factor | `decompose ... into`, `factor`, `split`, `separate`, `expand in ...` | The pieces and reconstruction relation must be valid and complete. In an error decomposition, preserve each term's computational or discretization source. |
| `B5` | Normalize or project | `normalize`, `project onto`, `restrict to`, `embed in` | State nonzero, feasibility, existence, uniqueness, and singular-case conditions. Distinguish a general metric projection from an orthogonal projection onto a subspace. |
| `B6` | Parameterize or index | `parameterize by`, `index by`, `enumerate`, `associate the parameter ...` | Distinguish a parameterization from a mere dependency or label. |

## C. Transformation and equivalence

| Code | Behavior | Licensed constructions | Boundary |
|---|---|---|---|
| `C1` | Substitute or eliminate | `substituting ... into ... gives`, `eliminating ... yields`, `upon substitution` | Show or cite the relation that licenses the substitution. Preserve side conditions. |
| `C2` | Rewrite an equivalent relation | `rewrite`, `express`, `recast`, `equivalently`, `in component form` | Use only for genuine equivalence on the stated domain. One-way implication is not equivalence. Tonelli/Fubini license interchange under their nonnegative or absolute-integrability branches; otherwise require a separate argument. |
| `C3` | Reduce or specialize | `reduces to`, `specializes to`, `recovers ... as a special case`, `setting ... gives` | State the limiting assumption, parameter choice, or restricted domain. |
| `C4` | Approximate or linearize | `approximate`, `linearize about`, `retain terms through order ...`, `is asymptotic to` | Preserve order, regime, remainder, and operating point. Never rewrite approximation as equality. |
| `C5` | Discretize or relax | `discretize`, `relax`, `convexify`, `sample`, `replace ... with ...` | State what property may be lost or preserved and whether the new problem is equivalent. Keep consistency, stability, convergence, discretization error, and algebraic-solve error distinct. |

## D. Inference and results

| Code | Behavior | Licensed constructions | Boundary |
|---|---|---|---|
| `D1` | Obtain by calculation | `obtain`, `derive`, `compute`, `evaluate`, `integration gives` | A reproducible calculation or cited formula must support the result. Name the theorem and hypotheses before interchanging an integral with a limit or derivative. |
| `D2` | Draw a logical consequence | `implies`, `hence`, `therefore`, `we deduce`, `it follows that` | Require a valid implication from stated premises and retain the hypotheses that license it. These are not rhythm words. Preserve branch-specific limit-theorem assumptions and the exact scope of any regularity or convergence upgrade. |
| `D3` | Establish or prove | `show`, `establish`, `prove`, `demonstrate analytically` | Match the verb and architecture to the argument: induction needs both stages, equivalence both directions, cases full coverage, and contradiction an explicit conflict. Numerical evidence does not prove a theorem. |
| `D4` | State a bound or order relation | `is bounded by`, `does not exceed`, `dominates`, `is monotone`, `satisfies` | Preserve strictness, norm, probability level, uniformity, and constant dependence. A confidence level, test level, power, high-probability bound, a priori estimate, and a posteriori estimator carry different scopes. |
| `D5` | Assert existence or uniqueness | `there exists`, `admits`, `has a unique`, `is well posed` | Identify the domain and hypotheses. An infimum or displayed `argmin` does not establish attainment; construction may establish existence but not automatically uniqueness. `Well posed` also requires continuous dependence in named topologies. |

## E. Dynamics and computation

| Code | Behavior | Licensed constructions | Boundary |
|---|---|---|---|
| `E1` | Describe evolution or governing law | `evolves according to`, `obeys`, `is governed by`, `satisfies the dynamics` | Preserve continuous or discrete time, frame, and initial conditions. For generalized solutions, state the topology and time sense in which initial data are attained. |
| `E2` | Initialize or reset | `initialize at`, `set initially to`, `reset to`, `start from` | Distinguish a fixed default from state-dependent initialization. |
| `E3` | Update or recur | `update by`, `iterate`, `propagate`, `integrate`, `obeys the recurrence` | State update order, index, sampling interval, and simultaneous or sequential semantics. |
| `E4` | Optimize or select | `minimize`, `maximize`, `solve for`, `select`, `choose to satisfy` | State decision variables, objective, constraints, and existence assumptions. Separate feasibility, stationarity, attainment, optimality, duality, and KKT conclusions. |
| `E5` | Converge or stabilize | `converges to`, `approaches`, `remains invariant`, `is asymptotically stable` | Preserve topology, mode, rate, locality, probability, and stability definition. Do not conflate convergence in law, probability, almost surely, in measure, or in \(L^p\), and do not report an observed finite-run slope as a proved rate. |

## F. Interpretation and comparison

| Code | Behavior | Licensed constructions | Boundary |
|---|---|---|---|
| `F1` | Explain semantic or physical role | `represents`, `encodes`, `collects`, `measures`, `captures` | These verbs explain role. They do not assert equality, causation, or sufficiency. |
| `F2` | Explain dependency or sensitivity | `depends on`, `is parameterized by`, `varies with`, `is invariant under` | Separate functional dependence and predictive association from causal influence; retain the population and validation regime. |
| `F3` | Compare mathematical objects | `coincides with`, `differs from`, `dominates`, `is equivalent to`, `generalizes` | Name the comparison criterion, scope, and direction. Keep conditioning versus stability, forward versus backward error, and equation-specific PDE solution hierarchies distinct. |
| `F4` | Interpret a formula or result | `shows that`, `indicates`, `means that`, `corresponds to`, `reveals` | Do not strengthen a formal result beyond its assumptions or turn an illustration into proof. Keep asymptotic versus finite-sample and distributional or almost-everywhere versus pointwise claims distinct; mark invalidated proof steps as superseded. |

## Local variation rules

Use repetition as a diagnostic, not a prohibited-word count.

1. Inspect adjacent sentences when the same lead appears twice consecutively or
   three times in one paragraph.
2. Classify the behaviors before revising. Different behaviors usually warrant
   different predicates. Identical behaviors may be better grouped.
3. Vary information structure when useful: prose then display, display then
   interpretation, a `where` clause, a grouped definition with `respectively`,
   or a notation table.
4. Retain a stable term when variation would create a false distinction.
5. Recheck mathematical direction and claim strength after every stylistic
   change.

Mechanical:

> The matrix $R$ is the attitude. The vector $e_R$ is the attitude error. The
> control moment is $M$.

Semantically structured:

> Let $R\in\mathrm{SO}(3)$ denote the attitude. The relative rotation defines
> the error vector $e_R\in\mathbb R^3$. Substitution of $e_R$ into the control
> law then gives the moment command $M$.

Prefer the second form only when `defines`, `substitution`, and `gives` match the
actual mathematics.
