# Inference and Analysis Boundary Forward-Test Artifacts

Date: 2026-07-24

These artifacts preserve the task contracts, observed outputs, self-audits, and
independent review outcomes for the sixth forward-test round. Four fresh
writing runs received tasks without model answers. Independent reviewers then
audited the exact outputs for mathematical correctness, hypothesis retention,
and claim strength.

The statistics and PDE cases passed on their first reviews. The
measure-and-interchange case exposed two successive layers of missing
precision. The numerical-analysis output preserved the requested distinctions
but invented a canonical linear-system and finite-element setting that the
first task had not supplied. The failed outputs and rule repairs remain visible
below. One intended measure-theory rerun was discarded before use because its
writer reported opening `core-evaluation.md`, contrary to the blindness
requirement. The final strictly blind reruns passed audits of their exact text
against their exact contracts.

## CF6-1 — Frequentist inference — PASS

Task contract:

```text
Use the math-prose skill at /home/gao/math-prose to solve this fresh evaluation
task. Do not inspect prior evaluation artifacts or git history. Task: Draft one
concise manuscript-ready paragraph from these supplied facts. A frequentist
interval procedure has 95% repeated-sampling coverage; the realized interval
is [1.2, 2.0]. A right-sided test gives p=0.03, and the rejection threshold
alpha=0.05 was selected before seeing the data. Under a separately specified
alternative theta=2.5, the test has power 0.80. The estimated effect is 1.6
units. State only the licensed conclusions. Do not assign a posterior
probability to theta, do not call p the probability that H0 is true, do not
conflate p, alpha, type-I error, and power, and do not infer practical
importance or causality. Return the paragraph and a short self-audit.
```

Observed output:

> The effect estimate was 1.6 units, and the 95% confidence procedure, which
> has 95% repeated-sampling coverage, yielded the realized interval
> \([1.2,\,2.0]\). The right-sided test gave \(p=0.03\); because
> \(\alpha=0.05\) was prespecified, the null hypothesis was rejected at that
> level. Under the separately specified alternative \(\theta=2.5\), the test
> has power \(0.80\).

Self-audit:

> Coverage is attributed to the procedure, not to a posterior probability for
> \(\theta\); \(p\), \(\alpha\), and power remain distinct; power is tied only
> to \(\theta=2.5\); no practical-importance or causal claim is made.

Independent audit: **PASS**. The reviewer confirmed that the output separates
procedure coverage from the realized interval, uses the prespecified test
level, and states power only at the specified alternative.

## CF6-2 — Measure convergence and analytic interchange — PASS after repair

### Initial run — FAIL

Task contract:

```text
Use the math-prose skill at /home/gao/math-prose to solve this fresh evaluation
task. Do not inspect prior evaluation artifacts or git history. Task: Write
concise manuscript-ready English for all of the following. On (0,1),
fn=n 1_(0,1/n), so fn→0 almost everywhere but ||fn||_1=1. For another sequence
gn, assume gn→g almost everywhere and |gn|≤h with h∈L1; state the licensed
integral conclusion and name the theorem. For a signed function q on a product
space, no nonnegativity or absolute-integrability hypothesis is supplied, so
do not reverse its iterated integrals. For I(theta)=∫r(x,theta)dx, pointwise
existence of ∂theta r is supplied but no integrable local derivative bound, so
do not differentiate under the integral. Return manuscript-ready prose and a
short self-audit.
```

Observed output:

> For \(f_n(x)=n\mathbf 1_{(0,1/n)}(x)\), we have \(f_n(x)\to0\) for every
> \(x\in(0,1)\), whereas
> \[
> \|f_n\|_{L^1(0,1)}=\int_0^{1/n}n\,dx=1.
> \]
> Thus, almost-everywhere convergence alone does not imply
> \(L^1\)-convergence.
>
> If \(g_n\to g\) almost everywhere and \(|g_n|\le h\) with \(h\in L^1\),
> then the dominated convergence theorem yields
> \[
> \lim_{n\to\infty}\int g_n=\int g.
> \]
>
> Because \(q\) is signed and neither nonnegativity nor absolute integrability
> is assumed, Tonelli’s and Fubini’s theorems do not justify reversing its
> iterated integrals. The original order of integration must therefore be
> retained.
>
> Although \(\partial_\theta r(x,\theta)\) exists pointwise, this condition
> alone does not justify differentiation under the integral. Without a
> suitable local integrable bound on \(\partial_\theta r\), the identity
> \[
> I'(\theta)=\int \partial_\theta r(x,\theta)\,dx
> \]
> is not asserted.

Self-audit:

> The prose preserves almost-everywhere versus \(L^1\) convergence, names the
> theorem supporting the licensed limit interchange, and declines both
> unsupported analytic interchanges without claiming they are impossible
> under additional hypotheses.

Independent audit: **FAIL**. The reviewer found that the dominated-convergence
statement omitted the measure space, measurability, and almost-everywhere
status of the domination. The sentence requiring retention of the original
integration order was also stronger than the evidence: failure to verify
Tonelli or Fubini only removes those licenses and does not rule out another
argument.

The first repair added the ambient measure space, measurability, and
almost-everywhere envelope to the dominated-convergence rule. It also changed
the interchange guard to distinguish “the named theorem does not license this
step” from “the step is impossible.”

### First repair rerun — FAIL

Observed output:

> On \(\bigl((0,1),\mathcal B((0,1)),\lambda\bigr)\), define
> \[
> f_n(x)=n\mathbf 1_{(0,1/n)}(x).
> \]
> For each \(x\in(0,1)\), \(f_n(x)=0\) whenever \(n>1/x\); hence
> \(f_n\to0\) pointwise, and therefore almost everywhere. However,
> \[
> \|f_n\|_{L^1(0,1)}
> =\int_0^1 n\mathbf 1_{(0,1/n)}(x)\,dx=1
> \]
> for every \(n\), so \(f_n\) does not converge to \(0\) in \(L^1(0,1)\).
>
> Let \((X,\Sigma,\mu)\) be a measure space, and let \(g_n,g:X\to\mathbb R\)
> be measurable. Suppose that \(g_n\to g\) almost everywhere and that a
> measurable \(h\in L^1(X,\mu)\) satisfies \(|g_n|\le h\) almost everywhere
> for every \(n\). The dominated convergence theorem then gives
> \(g\in L^1(X,\mu)\),
> \[
> \|g_n-g\|_{L^1(X,\mu)}\to0,
> \qquad
> \int_X g_n\,d\mu\to\int_X g\,d\mu.
> \]
>
> For a signed measurable function \(q\), the absence of both nonnegativity and
> absolute integrability means that neither Tonelli’s theorem nor Fubini’s
> theorem licenses reversing its iterated integrals under the stated
> hypotheses. This observation identifies a missing justification; it does
> not show that reversal is impossible, since additional assumptions or
> another argument may still justify it.
>
> Likewise, pointwise existence of \(\partial_\theta r(\theta,x)\) alone does
> not justify
> \[
> \frac{d}{d\theta}\int_X r(\theta,x)\,d\mu(x)
> =
> \int_X\partial_\theta r(\theta,x)\,d\mu(x).
> \]
> Such an interchange requires further theorem-specific hypotheses, such as an
> appropriate local integrable bound on the derivative. Pointwise
> differentiability therefore supplies insufficient justification by itself,
> without precluding a valid argument under stronger assumptions.

Self-audit:

> The prose preserves both convergence modes, states all dominated-convergence
> hypotheses and conclusions, and treats the two unavailable interchanges as
> unjustified rather than impossible.

Independent audit: **FAIL**. The reviewer found that the \(q\) paragraph was
not self-contained because it omitted the product domain, underlying measures,
and meaningfulness of the iterated integrals. The differentiation paragraph
also failed to make the derivative envelope uniform over a parameter
neighborhood, and its final caveat was syntactically unclear.

The second repair added product-space and definedness checks for iterated
integrals. It also required a named parameter neighborhood and one integrable
spatial envelope uniform over that neighborhood for the dominated-derivative
branch.

One intended second-repair run was **DISCARDED** before its output was used.
The writer voluntarily reported opening `core-evaluation.md`, despite the
blindness instruction. It did not open a round artifact or prior exact output,
but the run is not counted.

### Strictly blind final rerun — PASS

Task contract:

```text
Read SKILL.md and the mathematical guidance it routes to, but do not open
references/core-evaluation.md, any core-evaluation-round*.md file, git history,
diffs, or prior outputs. On the Lebesgue measure space (0,1), state the a.e.
but not L1 convergence of fn=n 1_(0,1/n). State dominated convergence for
measurable gn→g a.e. under one measurable L1 envelope. Let two sigma-finite
measure spaces and a product-measurable q be given; its iterated integrals are
separately finite, but q is neither nonnegative nor absolutely integrable.
State only that Tonelli and Fubini do not license equality or reversal. For a
parameter-dependent integral, only pointwise existence of the derivative is
known, with no parameter neighborhood and no one integrable spatial envelope
uniform there. State that the usual dominated-derivative argument is
unavailable without declaring differentiation impossible.
```

Observed output:

> 1. On \((0,1)\) with Lebesgue measure, let
> \[
> f_n(x)=n\mathbf 1_{(0,1/n)}(x).
> \]
> For every \(x\in(0,1)\), \(f_n(x)=0\) for all sufficiently large \(n\), so
> \(f_n\to0\) almost everywhere. Nevertheless,
> \[
> \|f_n\|_{L^1(0,1)}=\int_0^{1/n}n\,dx=1
> \]
> for every \(n\); hence the convergence is not in \(L^1\).
>
> 2. Let \((S,\mathcal A,\lambda)\) be a measure space, and let \(g_n\) and
> \(g\) be measurable functions such that \(g_n\to g\) almost everywhere.
> Suppose a single measurable \(h\in L^1(\lambda)\) satisfies
> \(|g_n|\le h\) almost everywhere for every \(n\). The dominated convergence
> theorem then gives \(g\in L^1(\lambda)\),
> \[
> \|g_n-g\|_{L^1(\lambda)}\to0,
> \qquad
> \int_S g_n\,d\lambda\to\int_S g\,d\lambda.
> \]
>
> 3. Let \((X,\Sigma,\mu)\) and \((Y,\mathcal T,\nu)\) be sigma-finite measure
> spaces, and let \(q:X\times Y\to\mathbb R\) be product-measurable. Even if
> both iterated integrals are separately known to exist as finite quantities,
> the assumptions that \(q\) is neither nonnegative nor absolutely integrable
> fall outside the relevant hypotheses of Tonelli’s and Fubini’s theorems.
> Those theorems therefore do not license equating the iterated integrals or
> reversing their order. Any equality in the particular case requires a
> separate justification.
>
> 4. For
> \[
> I(\theta)=\int_X r(x,\theta)\,d\mu(x),
> \]
> pointwise existence of \(\partial_\theta r(x,\theta)\) does not by itself
> justify differentiating under the integral sign at \(\theta_0\). No
> neighborhood \(U\) of \(\theta_0\) and no single \(H\in L^1(\mu)\)
> satisfying
> \[
> |\partial_\theta r(x,\theta)|\le H(x)
> \]
> for almost every \(x\) and every \(\theta\in U\) have been supplied. Thus,
> the usual dominated-derivative argument is unavailable; another theorem or
> additional hypotheses may still justify the differentiation.

Self-audit:

> The prose preserves almost-everywhere versus \(L^1\) convergence, states the
> full dominated-convergence conclusions, limits the Tonelli/Fubini claim to
> lack of authorization, and treats missing derivative domination as a gap in
> one justification rather than proof of impossibility.

Independent audit: **PASS after corrected audit input**. The first audit input
abbreviated “\(g_n\) and \(g\) are measurable” and “for almost every \(x\) and
every \(\theta\in U\)” into ambiguous shorthand, then rejected those
abbreviations. The exact output above contains both requirements. Re-audit of
the exact text found all four claims correct, appropriately quantified, and
cautiously scoped.

## CF6-3 — PDE solution classes and well-posedness — PASS

Task contract:

```text
Use the math-prose skill at /home/gao/math-prose to solve this fresh evaluation
task. Do not inspect prior evaluation artifacts or git history. Task: Write
concise manuscript-ready English under this contract. Let u∈H^1_0(U) satisfy
a(u,v)=ell(v) for every v∈H^1_0(U); call it only a weak Dirichlet solution and
express the boundary condition through the Sobolev space or trace, not
pointwise. A separate interior regularity theorem yields u∈H^2_loc(U) and
Lu=f almost everywhere under stated ellipticity and coefficient assumptions;
do not call u C^2, classical, or regular up to the boundary. For an evolution
problem, v∈L∞_t H^s_x is only a weak H^s solution; no C^0_t H^s_x result is
supplied, so do not assert v(t0)=v0 as ordinary H^s equality at every time.
Finally, existence and uniqueness are known in a named class X, but continuous
dependence is not supplied; do not call the problem well posed. Return the
prose and a short self-audit.
```

Observed output:

> Let \(u\in H_0^1(U)\) satisfy
> \[
> a(u,\varphi)=\ell(\varphi)
> \qquad\text{for every }\varphi\in H_0^1(U).
> \]
> Then \(u\) is a weak Dirichlet solution, with the homogeneous boundary
> condition encoded by \(u\in H_0^1(U)\). Under the stated ellipticity and
> coefficient assumptions, a separate interior regularity theorem yields
> \(u\in H_{\mathrm{loc}}^2(U)\) and \(Lu=f\) almost everywhere in \(U\).
>
> For the evolution problem, \(v\in L^\infty(0,T;H^s)\) is only a weak
> \(H^s\) solution. This regularity alone neither provides a representative in
> \(C([0,T];H^s)\) nor justifies \(v(t_0)=v_0\) as an ordinary \(H^s\)
> equality; the initial datum must be interpreted in the sense specified by
> the weak formulation. Finally, the result establishes existence and
> uniqueness in the class \(X\), but not well-posedness, because continuous
> dependence has not been supplied.

Self-audit:

> The boundary condition is stated through \(H_0^1(U)\); the elliptic upgrade
> remains local and almost everywhere; no classical, boundary-regularity, or
> time-continuity claim is made; and well-posedness is not asserted.

Independent audit: **PASS**. The reviewer found no defect in the weak/strong
regularity distinction, time-trace limitation, or separation of uniqueness
from full well-posedness.

## CF6-4 — Numerical errors, estimators, and rates — PASS after repair

Task contract:

```text
Perform a fresh forward evaluation of the math-prose skill. Do not inspect
prior evaluation artifacts or git history. Draft manuscript-ready mathematical
prose satisfying this contract: the problem is ill-conditioned but the
algorithm is backward stable; a small backward error does not automatically
imply a small forward error without a conditioning argument; an a priori
O(h^p) bound is not the realized error; an a posteriori estimator eta_h has
separately stated reliability and efficiency bounds; the computed solution has
discretization, algebraic-solver, and rounding components that must remain
distinct; a finite log-log slope is observed evidence, not proof of the
asymptotic rate. Return only the prose and a concise self-audit.
```

Observed output:

> For the ill-conditioned system \(Ax=b\), the solver is backward stable: the
> computed solution \(\widehat{x}\) solves a nearby problem
> \((A+\Delta A)\widehat{x}=b+\Delta b\), with normwise backward error of order
> the unit roundoff. This property concerns the algorithm, whereas the large
> condition number \(\kappa(A)\) concerns the sensitivity of the problem.
> Consequently, a small backward error does not by itself imply a small
> forward error \(\|\widehat{x}-x\|/\|x\|\); that conclusion additionally
> requires a perturbation bound involving \(\kappa(A)\), and the bound may be
> uninformative when the problem is severely ill-conditioned.
>
> Let \(u_h\in V_h\) denote the exact discrete solution on a shape-regular
> mesh. Under the stated approximation and regularity assumptions, the
> a priori estimate
> \[
> \|u-u_h\|_V\le C_{\mathrm{ap}}h^p|u|_{H^{p+1}(\Omega)}
> \]
> gives an asymptotic upper bound, with \(C_{\mathrm{ap}}\) independent of
> \(h\); it neither equals nor determines the realized error on a particular
> mesh. By contrast, the computable a posteriori estimator \(\eta_h\) satisfies
> the reliability bound
> \[
> \|u-u_h\|_V\le C_{\mathrm{rel}}\eta_h
> \]
> and, separately, the efficiency bound
> \[
> \eta_h\le C_{\mathrm{eff}}
> \bigl(\|u-u_h\|_V+\operatorname{osc}_h\bigr),
> \]
> under their respective hypotheses. Reliability controls the error from
> above, whereas efficiency prevents substantial overestimation up to data
> oscillation.
>
> To distinguish the remaining error sources, let \(u_h^{\mathrm{alg}}\) be the
> output of the stopped algebraic solver in exact arithmetic and let
> \(\widetilde u_h\) be its finite-precision counterpart. Then
> \[
> u-\widetilde u_h
> =(u-u_h)+(u_h-u_h^{\mathrm{alg}})
> +(u_h^{\mathrm{alg}}-\widetilde u_h),
> \]
> whose three terms represent discretization, algebraic-solver, and rounding
> errors, respectively. They should not be merged without an explicit
> total-error estimate.
>
> Over the tested refinements, the measured log–log slope may be close to
> \(p\). This finite-run observation is consistent with \(O(h^p)\) behavior,
> but it does not prove the asymptotic rate or verify its hypotheses.

Self-audit:

> The passage separates conditioning from stability, qualifies the
> backward-to-forward error inference, distinguishes a priori and a posteriori
> statements, states reliability and efficiency independently, preserves three
> error components, and treats the fitted slope as empirical evidence rather
> than proof.

Initial distinction audit: **PASS, but incomplete**. The reviewer found no
defect in the mathematical distinctions. A later contract-preservation audit
superseded that result and marked the run **FAIL**: the prompt supplied only
category-level facts, while the output invented \(Ax=b\), a normwise
unit-roundoff perturbation model, \(\kappa(A)\), a shape-regular finite-element
setting, specific estimator inequalities, and \(\operatorname{osc}_h\).

The repair added a direct guard: category-level numerical facts must remain
symbolic or conditional, and a familiar linear system, finite-element model,
norm, perturbation formula, estimator bound, or decomposition must not be
instantiated unless supplied.

### Formula-supplied strictly blind rerun — PASS

Task contract:

```text
Perform a strictly blind forward evaluation of the current math-prose skill.
Read SKILL.md and routed mathematical guidance, but do not open
references/core-evaluation.md, any core-evaluation-round*.md, git history,
diffs, or prior outputs.

A nonzero nonsingular matrix A and nonzero b define Ax=b with exact solution x.
The problem is ill-conditioned. A supplied backward-stability result says the
computed xhat is the exact solution of (A+ΔA)xhat=b+Δb and

  max{||ΔA||/||A||, ||Δb||/||b||}≤ε.

No forward-error estimate is supplied. State that small backward error alone
does not establish small forward error without an applicable conditioning or
perturbation result.

Under H_ap, the supplied a priori theorem is

  ||u-u_h||_V≤C_ap h^p,

where C_ap is independent of h. Do not identify this upper bound with the
realized error. Under H_rel and H_eff, respectively, the supplied computable
estimator η_h obeys

  ||u-u_h||_V≤C_rel η_h

and

  η_h≤C_eff(||u-u_h||_V+osc_h).

Keep reliability and efficiency separate. Preserve the supplied exact
decomposition

  u-ũ_h=(u-u_h)+(u_h-u_h^alg)+(u_h^alg-ũ_h),

whose terms are stipulated to be discretization, algebraic-solver, and
rounding error. A finite refinement study observed a log-log slope close to p.
Report it as consistent with, but not proof of, an asymptotic O(h^p) rate.
Do not invent any other formula or setting.
```

Observed output:

> Let \(A\) be a nonzero nonsingular matrix and \(b\) be nonzero. The system
> \[
> Ax=b
> \]
> has exact solution \(x\) and is ill-conditioned. The supplied
> backward-stability result states that the computed solution \(\widehat{x}\)
> solves
> \[
> (A+\Delta A)\widehat{x}=b+\Delta b
> \]
> exactly, with
> \[
> \max\left\{\frac{\|\Delta A\|}{\|A\|},
> \frac{\|\Delta b\|}{\|b\|}\right\}\le\varepsilon.
> \]
> This result controls the backward error; no forward-error estimate has been
> supplied. In particular, small backward error alone does not establish small
> forward error without an applicable conditioning or perturbation result.
>
> Under \(H_{\mathrm{ap}}\), the supplied a priori theorem gives
> \[
> \|u-u_h\|_V\le C_{\mathrm{ap}}h^p,
> \]
> where \(C_{\mathrm{ap}}\) is independent of \(h\). This inequality is an
> upper bound and should not be identified with the realized error.
>
> Under \(H_{\mathrm{rel}}\), the computable estimator \(\eta_h\) satisfies the
> reliability bound
> \[
> \|u-u_h\|_V\le C_{\mathrm{rel}}\eta_h.
> \]
> Separately, under \(H_{\mathrm{eff}}\), its supplied efficiency bound is
> \[
> \eta_h\le C_{\mathrm{eff}}
> \bigl(\|u-u_h\|_V+\operatorname{osc}_h\bigr).
> \]
>
> The exact error decomposition is
> \[
> u-\widetilde u_h
> =(u-u_h)+(u_h-u_h^{\mathrm{alg}})
> +(u_h^{\mathrm{alg}}-\widetilde u_h).
> \]
> The three terms are stipulated to represent discretization, algebraic-solver,
> and rounding error, respectively.
>
> Finally, the finite refinement study observed a log–log slope close to
> \(p\). This observation is consistent with an asymptotic \(O(h^p)\) rate,
> but it does not prove that rate.

Self-audit:

> All supplied formulas, assumptions, constant dependence, and error
> categories were retained. Reliability and efficiency remain separate, and
> no forward-error estimate, realized-error equality, problem type,
> discretization type, norm, perturbation formula, regularity condition, or
> additional theorem hypothesis was introduced.

Independent audit: **PASS after corrected audit input**. The first re-audit
input accidentally omitted the supplied words `computable estimator` and the
required \(O(h^p)\) comparison, then misclassified both as inventions. Review
of the exact output against the exact contract found no defect.

## Round decision

The round passes after the CF6-2 and CF6-4 rule repairs and final strictly blind
reruns. The failures show that preserving a theorem name or high-level
distinction is insufficient: the prose must also preserve ambient spaces,
measurability, definedness, quantifier order, missing mathematical inputs, and
the difference between an unavailable justification and an impossibility
claim. This remains a targeted evaluation, not a completeness result for all
of statistical inference, measure theory, PDEs, or numerical analysis.
