# Equation and Paragraph Discourse

Use equation placement to control emphasis and reduce mechanical definition
sequences. Each display should have a reason to appear and a clear connection to
the surrounding argument.

## Contents

- [Before a display](#before-a-display)
- [After a display](#after-a-display)
- [Common architectures](#common-architectures)
- [Local variation and intentional parallelism](#local-variation-and-intentional-parallelism)
- [Paragraph rhythm](#paragraph-rhythm)
- [Anti-patterns](#anti-patterns)

## Before a display

Use the lead sentence to do one of four jobs:

- state the object being introduced;
- state the operation being performed;
- state the result being claimed;
- state the question the equation answers.

Avoid an empty lead such as `The equation is as follows.` Prefer a semantic
lead:

> Projecting the unconstrained vector onto the feasible set gives

> The closed-loop error evolves according to

> Define the energy functional by

## After a display

Do not merely translate every symbol back into words. Use the next sentence to:

- define nonstandard symbols or spaces;
- interpret the mathematical role;
- identify a condition or singular case;
- connect the result to the next operation;
- explain why the equation matters to the argument.

## Common architectures

### Local definition

> Let $x\in\mathcal X$ denote the state. Define the functional
> \[
> V(x):=\cdots .
> \]
> Here, ... .

Use `denote` for the name and `define` for the functional. Do not merge the two
actions if that obscures the type or scope.

### Explicit formula for an existing object

> The coefficient is given by
> \[
> c=\cdots,
> \]
> where ... .

Use this pattern when the coefficient has already been introduced by role.

### Grouped definitions

Group objects when they share scope and their correspondence is unambiguous:

> Define the position and velocity errors, respectively, by
> \[
> e_x=\cdots,\qquad e_v=\cdots .
> \]

Do not use `respectively` across a long sentence, mixed types, or a nonparallel
list.

### Derivation chain

Use connective phrases to identify the actual operation:

> Substituting (3) into (5) and collecting like terms yields
> \[
> \cdots .
> \]
> Applying the bound in Lemma 1 then gives ... .

Avoid repeating `we have` between every aligned equation. An aligned display may
carry algebraic continuity, while prose marks only the non-obvious operation or
logical transition.

### Mapping or transformation

State direction before consequences:

> Let $T:\mathcal X\to\mathcal Y$ map the original coordinates to the reduced
> state. Applying $T$ to the dynamics gives ... .

For frames and coordinate transforms, state what the map acts on and in which
direction. A matrix label alone is insufficient when conventions differ.

For a set-valued map \(S:\mathcal X\rightrightarrows\mathcal U\), define images,
inverse images, and the graph as sets. Before defining a map on equivalence
classes, verify that the proposed value is independent of the representative.

### Dynamical system

Separate the governing law, initial condition, and interpretation:

> The state evolves according to
> \[
> \dot x=f(x,u),\qquad x(0)=x_0.
> \]
> The first relation specifies the vector field, while the second fixes the
> initial state.

Do not write `is initialized as` when the code or model actually initializes
from a measured state, a distribution, or a previous iterate.

### Weak or generalized PDE formulation

Introduce the solution notion before the display, then make the function space,
test class, and sense of equality visible:

> Seek \(u\in H^1_0(U)\) such that
> \[
> a(u,v)=\ell(v)\qquad\text{for every }v\in H^1_0(U).
> \]

Do not replace a Sobolev trace condition by pointwise boundary equality unless
the required representative or regularity is available. Likewise, do not
rotate among `classical`, `strong`, `weak`, `distributional`, and `viscosity`
as stylistic variants. If a regularity theorem upgrades one notion, state its
hypotheses and whether the conclusion is interior, up to the boundary, almost
everywhere, or pointwise.

For an evolution equation, separate the governing relation from the topology
in which the initial datum is attained. Essential boundedness in time and
continuity into the state space support different statements at an individual
time.

### Optimization problem

Identify all mathematical roles:

> Given the data $D$, estimate $\theta$ by solving
> \[
> \min_{\theta\in\Theta} J(\theta;D)
> \quad\text{subject to}\quad g(\theta)\le 0.
> \]

Distinguish the decision variable, admissible set, objective, data, and
constraints. `The optimal parameter is given by` is too strong unless a
solution exists and the displayed expression actually characterizes it.
Likewise, distinguish the optimal value from an optimizer: an infimum need not
be attained, and an `argmin` may be empty or contain several points.
Stationarity alone does not imply optimality outside its stated hypotheses;
KKT conclusions must retain the convexity, constraint-qualification, and other
conditions supplied by the relevant theorem.

### Numerical approximation and error statement

Introduce the continuous problem, discrete space, and approximation parameter
before reporting an error:

> Let \(u_h\in V_h\) denote the discrete solution on a shape-regular mesh.
> Under the stated consistency, stability, approximation, and regularity
> assumptions, the a priori estimate gives ...

Say whether the discrete problem is assumed to be solved exactly. If the
linear or nonlinear solve is inexact, keep its algebraic error separate from
the continuous-to-discrete error. Distinguish a theoretical a priori rate from
a computable a posteriori estimator, and distinguish both from an empirical
slope observed over finitely many refinements or iterations.

For finite-precision computations, keep problem conditioning separate from
algorithmic stability and keep forward, backward, residual, and rounding errors
named by their actual comparison objects.

Do not manufacture a familiar model to make a qualitative prompt look
concrete. If no linear system, finite-element space, norm, estimator inequality,
oscillation term, or error decomposition is supplied, retain placeholders or
state the distinctions without formulas.

### Piecewise or case definition

State whether the cases define an object or report a consequence. Verify that
the cases cover the intended domain and clarify boundary overlap. If a boundary
point satisfies more than one condition, say whether the corresponding values
agree or make the cases disjoint.

### Algorithm with conditional acceptance

State the initialization source before the recurrence. Preserve sequential
dependencies when one update uses a newly computed quantity, and distinguish a
candidate from the accepted state:

> First compute \(y^{k+1}=F(x^k)\), and then form the candidate
> \(x^{k+1}=G(y^{k+1})\). Accept the candidate if the residual test passes;
> otherwise retain \(x^k\).

Carry over supplied tolerances, counters, stopping conditions, and inexact-solve
qualifiers. Do not invent an omitted state update or convergence guarantee.

### Equation followed by interpretation

Use a strength-matched interpretation:

- an identity `shows` an exact relation;
- an inequality `bounds` a quantity;
- an approximation `suggests` behavior only in its regime;
- a simulation `indicates` an observed trend;
- a theorem `establishes` its stated conclusion under its hypotheses.

Keep empirical comparisons equally scoped. A lower runtime or error on a named
dataset and finite set of runs is an observation about that setup, not a proof
of universal superiority.

## Local variation and intentional parallelism

Before finalizing a paragraph, compare its sentence frames with the neighboring
equation-introduction units. Inspect the subject role, predicate, complement,
and equation position as a complete frame rather than counting repeated words
alone.

Use the repetition deliberately:

1. If matched wording exposes a genuine mathematical correspondence, retain
   the parallel structure.
2. If nearby sentences perform different mathematical behaviors, give each a
   construction licensed by its own behavior.
3. If they perform the same behavior but the repetition adds no structure,
   revise one by changing the information order, grammatical subject, equation
   placement, or grouping.
4. If no equally precise alternative exists, retain the repeated wording.

Do not select a backup phrase merely because it is different. In particular,
do not replace a stable technical term, change a definition into an
interpretation, or strengthen a descriptive relation to an inference.

For example, avoid applying the same repair template to two adjacent tuple
introductions. A locally varied version may read:

> Together, $p$ and $v$ form the measured-state tuple
> \[
> s=(p,v).
> \]
> Define the corresponding reference tuple by
> \[
> s_r=(p_r,v_r,a_r).
> \]

The first sentence assembles already identified measurements, whereas the
second stipulates the reference tuple. By contrast, matched frames may be
preferable when introducing two inverse maps or stating genuinely symmetric
assumptions.

## Paragraph rhythm

A mathematical paragraph often follows this sequence:

1. purpose or available data;
2. formal object or operation;
3. displayed relation;
4. symbol and condition clarification;
5. consequence or interpretation;
6. connection to the next step.

Not every paragraph needs all six moves. Preserve the logical order even when
compressing for a venue limit.

## Anti-patterns

- orphaned displays with no stated role;
- a `where` sentence that defines only physical meaning but omits type or space;
- repeated `is` sentences that actually mix naming, construction, inference,
  and interpretation;
- the same repair template applied twice in a local block without checking
  whether the mathematics is intentionally parallel;
- forced synonym changes that obscure a stable definition or technical term;
- `equivalently` between one-way implications;
- an after-equation sentence that overclaims what the equation proves;
- several single-sentence paragraphs created only to vary sentence openings;
- decorative `thus`, `therefore`, or `hence` without a logical dependency.
