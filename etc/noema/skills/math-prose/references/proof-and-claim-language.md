# Proof and Claim Language

Match linguistic strength to the mathematical support. A polished sentence
cannot repair a missing hypothesis or invalid implication.

## Strength ladder

From weaker description to stronger mathematical commitment:

1. `is consistent with`, `suggests`, `indicates`;
2. `can be obtained`, `calculation gives`, `we derive`;
3. `satisfies`, `implies`, `it follows that`;
4. `establishes`, `shows`, `proves`;
5. `is equivalent to`, `if and only if`, `guarantees`.

These are not interchangeable style variants. Select a level from the actual
evidence and argument.

## Assumptions and scope

- Put assumptions before the result that depends on them.
- Preserve `for all`, `there exists`, `almost surely`, `with probability`,
  `locally`, `uniformly`, and other scope markers.
- Read nested quantifiers from left to right. In
  \(\forall\alpha\,\exists C(\alpha)\,\forall u\,\forall n\), the constant may
  depend on \(\alpha\), but it is uniform in \(u\) and \(n\).
- Distinguish an adopted modeling assumption from a proved property.
- State regularity, nonzero, rank, compactness, convexity, and boundary
  conditions when they license a later operation.

## Common proof moves

| Move | Useful constructions | Requirement |
|---|---|---|
| Fix an arbitrary object | `fix`, `let ... be arbitrary` | Keep its space and scope visible. |
| Invoke a result | `by Lemma 2`, `applying Theorem 1` | Check that every hypothesis is satisfied. |
| Construct a witness | `choose`, `construct`, `set` | Explain why the witness is admissible. |
| Transform an expression | `substituting`, `rearranging`, `integrating`, `taking norms` | Preserve equality or inequality direction and side conditions. |
| Establish a bound | `using ... gives`, `is bounded by`, `applying ... yields` | State the norm, constants, and dependence. |
| Split cases | `consider`, `distinguish`, `if ... whereas ...` | Cover the whole domain without overlap ambiguity. |
| Contradiction | `suppose for contradiction`, `contradicting ...` | Identify the contradicted statement. |
| Conclude | `therefore`, `hence`, `which proves` | The conclusion must match the stated claim exactly. |

Expose the full architecture when it carries mathematical information:

- induction states the base case, fixes an arbitrary index, invokes the
  induction hypothesis, and proves the successor case;
- an `if and only if` proof gives both implications;
- a case proof covers the full domain without ambiguous overlap;
- contradiction names the assumed negation and the incompatible conclusions.

## Deduce, derive, obtain, and show

- Use `derive` when a sequence of mathematical operations produces a formula.
- Use `obtain` for a neutral result of calculation or construction.
- Use `deduce` when premises entail a conclusion. It is stronger than a local
  algebraic transition.
- Use `show` when the surrounding argument establishes a stated property or
  result. Do not use it for a plot that merely illustrates a trend.

## Equivalence and implication

- `A implies B` is directional.
- `A is equivalent to B` requires both directions on the stated domain.
- `A reduces to B` describes a restriction, limit, or parameter choice and is
  not automatically equivalence.
- `A can be written as B` normally asserts equality of representations. State
  approximation explicitly when equality does not hold.

## Existence and uniqueness

Separate these claims unless one theorem establishes both. A constructed
solution establishes existence only after admissibility is checked. A numerical
solver returning one output does not establish mathematical uniqueness.
Likewise, an infimum need not be attained, a stationary point need not be a
minimizer, and a displayed `argmin` may be empty or set-valued. State the
convexity and constraint qualifications that turn KKT conditions into necessary
or sufficient optimality claims.

## Bounds, convergence, and stability

Always preserve:

- the norm or metric;
- local versus global scope;
- deterministic, probabilistic, or almost-sure meaning;
- asymptotic, exponential, finite-time, or practical convergence;
- dependence of constants on parameters or initial data;
- invariant set, residual set, or limiting value;
- strict versus non-strict inequalities.

Use `converges` only when the limiting statement is supported. Use `remains
bounded` for boundedness and `approaches a neighborhood` for practical or
residual convergence.

Keep probability modes explicit:

- convergence in distribution does not generally imply convergence in
  probability;
- convergence in probability does not by itself imply almost-sure convergence,
  although every subsequence has a further almost-surely convergent
  subsequence;
- a tail bound whose failure probability tends to zero supports a
  high-probability or convergence-in-probability conclusion, not automatically
  an almost-sure one; an argument such as summability plus Borel--Cantelli is
  needed for the latter.

## Statistical inference

Keep the probability-bearing object and reference distribution explicit.

- A frequentist confidence level describes repeated-sampling coverage of a
  random interval procedure. After the data are observed, it does not assign a
  posterior probability to the fixed parameter unless a Bayesian model is
  separately supplied.
- A p-value is evaluated under a specified null model. Keep it distinct from a
  prechosen significance level, the probability of rejecting a true null, the
  probability of missing a specified alternative, and power under that
  alternative.
- `Statistically significant` does not mean that an effect is large,
  practically important, causal, or certain. Name the estimand, population,
  test direction, multiplicity adjustment, and validation regime that support
  the reported inference.
- State exactly what is consistent and which error probability, norm, or event
  tends to its limit. Model-selection consistency is neither finite-sample
  correctness nor estimator unbiasedness.
- An asymptotic distribution supplies a large-sample approximation. Do not
  report it as an exact finite-sample law without a separate result.

## Interchanging analytic operations

Treat every interchange as a theorem application, not as typographic
reordering.

- For monotone convergence, retain nonnegativity and monotone increase.
- For dominated convergence on the stated measure space, retain measurability,
  almost-everywhere convergence, and one measurable integrable envelope that
  dominates the whole sequence almost everywhere.
- For iterated integrals, name the two measure spaces and the product-space
  measurability assumptions. Distinguish the nonnegative Tonelli branch from
  the absolute-integrability condition used for signed functions; if discussing
  two existing iterated integrals outside those branches, first ensure that the
  displayed integrals are meaningful.
- For differentiation under an integral, state the parameter neighborhood and
  an integrable spatial envelope that bounds the derivative uniformly over
  that neighborhood, or give the other theorem-specific conditions actually
  used.
- If a theorem's hypotheses are unavailable, conclude only that the theorem
  does not license the interchange. Do not declare the operation impossible or
  insist on the original order unless a separate argument establishes that
  stronger claim.
- Almost-everywhere or in-measure convergence does not alone imply
  \(L^p\)-convergence or convergence of expectations. State the norm,
  domination, or uniform-integrability condition that supplies the upgrade.
- Tightness prevents probability mass from escaping; uniform integrability
  controls first-moment tails. Do not substitute one for the other.

## PDE solution concepts

Solution labels are mathematical contracts, not stylistic alternatives.

- Name the equation, spatial and temporal function spaces, test class, and
  whether the equation holds pointwise, almost everywhere, weakly,
  distributionally, through an integral formulation, or in the viscosity
  sense.
- A classical-to-generalized implication is normally one-way. A converse needs
  a regularity theorem with its operator, coefficient, data, interior or
  boundary, and topology assumptions intact.
- For Sobolev solutions on domains and spaces covered by an applicable trace
  theorem, formulate boundary data with its trace operator. Otherwise retain
  the supplied zero-trace closure space or weak boundary formulation.
  Pointwise boundary values require additional representative regularity.
- For evolution equations, state the topology in which the initial datum is
  attained. Essential boundedness in time does not by itself provide
  every-time equality in the state space.
- `Well posed` requires existence, uniqueness in a named solution class, and
  continuous dependence in stated data and solution topologies.

## Numerical error and rate claims

- Conditioning describes sensitivity of the mathematical problem; stability
  describes how an algorithm propagates data and rounding perturbations.
- Forward error compares computed and exact outputs. Backward error measures a
  perturbation of the input that makes the computed output exact. A small
  backward error gives a small forward error only through a conditioning
  result.
- An a priori estimate states a theoretical bound or rate and may involve
  exact-solution regularity. An a posteriori estimator must be computable from
  the discrete solution and data; state its reliability, efficiency, or
  effectivity result when available.
- Keep discretization, algebraic-solve, residual-evaluation, and rounding
  errors separate until an explicit total-error decomposition combines them.
- Consistency, stability, approximation, and convergence are separate
  properties. Invoke the applicable theorem rather than inferring convergence
  from one property alone.
- A fitted finite-run slope is empirical evidence. State a proved asymptotic or
  worst-case rate separately with its norm, constants, and hypotheses.
- Category-level facts do not license a canonical example. If the input omits
  the problem, norm, perturbation model, estimator bounds, or decomposition,
  use conditional or symbol-free prose rather than supplying a standard linear
  system or finite-element formula from background knowledge.

## Mathematical and empirical evidence

Keep evidence modes distinct:

- a proof establishes a formal claim under assumptions;
- a symbolic or numerical check verifies selected cases;
- a simulation demonstrates behavior in the tested setup;
- an experiment provides observations with measurement and sampling limits;
- a citation transfers only the claim actually supported by the cited source.

Do not use `prove`, `guarantee`, or `establish` for empirical evidence alone.
If an official erratum invalidates a proof step, retain the old step only as
historical or superseded evidence and cite the corrected argument as current.
