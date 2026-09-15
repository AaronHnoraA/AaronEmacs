# Boundary-Composition Forward-Test Artifacts

Date: 2026-07-23

These artifacts preserve the task contracts, observed outputs, self-audits, and
independent review outcomes for the fifth forward-test round. Three fresh
writing runs received seven tasks without model answers. Three different
reviewers then audited the resulting text against the supplied mathematical
contracts.

All seven writing outputs passed independent review. Two initial review inputs
were themselves incomplete: the CF5-2 review input replaced the displayed
piecewise formula with a meta-description, and the CF5-5 review input replaced
the displayed theorem bound with the words “exact bound.” Those two review
results are retained below as invalid audit-input failures. Re-audits against
the exact observed outputs passed.

## CF5-1 — Quantifiers and probability modes — PASS

Task contract:

```text
Let r_n→0 and δ_n→0. Assume

  ∀α∈A ∃C(α)>0 ∀u∈U_adm ∀n,
  P(E_n(u)>C(α)r_n)≤δ_n.

Make explicit that C may depend on α but not on u or n. Treat E_n(u) as a
nonnegative error and derive convergence in probability for each fixed α and
u, but do not claim almost-sure convergence without a summability condition.
Separately state X_n⇒X as convergence in distribution. Also state that if Y_n
converges to Y in probability, every subsequence has a further subsequence
converging almost surely to Y. Do not conflate the convergence modes. Return
manuscript-ready English and a concise self-audit.
```

Observed output:

> Let \(r_n\to0\) and \(\delta_n\to0\). Assume that
> \[
> \forall \alpha\in A\;\exists C(\alpha)>0\;\forall u\in\mathcal U_{\mathrm{adm}}\;\forall n,
> \qquad
> \mathbb P\!\left(E_n(u)>C(\alpha)r_n\right)\le \delta_n .
> \]
> Thus, \(C(\alpha)\) may depend on \(\alpha\), but not on \(u\) or \(n\).
>
> Interpreting \(E_n(u)\) as a nonnegative error, fix \(\alpha\in A\), an
> admissible \(u\), and \(\varepsilon>0\). For all sufficiently large \(n\),
> \(C(\alpha)r_n<\varepsilon\), and hence
> \[
> \mathbb P\!\left(E_n(u)>\varepsilon\right)
> \le
> \mathbb P\!\left(E_n(u)>C(\alpha)r_n\right)
> \le\delta_n\to0.
> \]
> Therefore, \(E_n(u)\xrightarrow{\mathbb P}0\) for each fixed \(\alpha\) and
> \(u\). This estimate alone does not imply almost-sure convergence, because no
> summability condition such as \(\sum_n\delta_n<\infty\) is assumed.
>
> Separately,
> \[
> X_n\Rightarrow X
> \]
> denotes convergence in distribution.
>
> Finally, if \(Y_n\xrightarrow{\mathbb P}Y\), then every subsequence
> \((Y_{n_k})\) has a further subsequence \((Y_{n_{k_j}})\) such that
> \[
> Y_{n_{k_j}}\to Y\qquad\text{almost surely}.
> \]

Self-audit:

> The quantifier order makes \(C(\alpha)\) independent of \(u\) and \(n\). The
> convergence argument fixes both \(\alpha\) and \(u\). Nonnegativity of
> \(E_n(u)\) is necessary; otherwise the one-sided bound does not control
> \(\lvert E_n(u)\rvert\). No almost-sure conclusion is claimed without
> summability. Convergence in probability, distribution, and almost-sure
> subsequential convergence remain separate.

Independent audit: **PASS**. The reviewer found no defect in the quantifiers,
dependencies, hypotheses, convergence modes, or inference strength.

## CF5-2 — Piecewise definition and proof architectures — PASS

Task contract:

```text
Define g:R→R by g(x)=-x for x<0, g(x)=0 for x=0, and g(x)=x for x>0;
state that the cases are mutually exclusive and exhaustive. State an induction
proof of P(n) for n≥0 using base P(0), an arbitrary k≥0, hypothesis P(k), and
the supplied implication to P(k+1). State both directions of A iff B. State a
contradiction proof that assumes ¬C and uses the supplied argument to obtain D
and ¬D before concluding C. Do not invent internal calculations. Return
manuscript-ready English and a concise self-audit.
```

Observed output:

> Define \(g:\mathbb R\to\mathbb R\) by
> \[
> g(x)=
> \begin{cases}
> -x, & x<0,\\
> 0,  & x=0,\\
> x,  & x>0.
> \end{cases}
> \]
> These three cases are mutually exclusive and cover all \(x\in\mathbb R\).
>
> To prove \(P(n)\) for every \(n\in\mathbb Z_{\ge0}\), first establish the
> base case \(P(0)\). For the inductive step, fix an arbitrary
> \(k\in\mathbb Z_{\ge0}\) and assume \(P(k)\). The supplied implication
> \(P(k)\Rightarrow P(k+1)\) then yields \(P(k+1)\). Mathematical induction
> therefore establishes \(P(n)\) for all \(n\ge0\).
>
> To establish \(A\iff B\), use the supplied forward argument to prove
> \(A\Rightarrow B\), and then use the supplied reverse argument to prove
> \(B\Rightarrow A\).
>
> For the contradiction proof, suppose that \(\neg C\). The supplied argument
> yields both \(D\) and \(\neg D\), which is a contradiction. Hence \(C\).

Self-audit:

> The piecewise cases are exclusive and exhaustive, with \(x=0\) handled
> explicitly. Induction ranges over all nonnegative integers and uses an
> arbitrary \(k\ge0\). Both directions of the equivalence are stated
> separately. The contradiction architecture derives exactly \(D\) and
> \(\neg D\); no internal calculation was invented. The conclusion \(C\) uses
> the standard classical contradiction principle.

Independent audit: **PASS after corrected audit input**. The first audit input
omitted the displayed formula and therefore reported that omission. Because the
observed output above did contain the complete formula, that review result is
invalid and superseded. Re-audit of the exact output found no defect.

## CF5-3 — Optimization claim boundaries — PASS

Task contract:

```text
State that f(x)=1/x on (0,∞) has infimum 0 but does not attain it and therefore
has no minimizer. State that h(x)=x^3 satisfies h'(0)=0 although 0 is not a
local minimizer. State weak duality d*≤p*. For a supplied differentiable convex
problem satisfying Slater's condition, state only the supplied necessary-and-
sufficient KKT characterization. Explicitly avoid claiming that feasibility or
stationarity alone implies optimality, or that an argmin display establishes
existence or uniqueness. Return manuscript-ready English and a self-audit.
```

Observed output:

> For \(f(x)=1/x\) on \((0,\infty)\), \(\inf_{x>0}f(x)=0\), but no \(x>0\)
> attains this value; hence, \(f\) has no minimizer. For \(h(x)=x^3\), the
> condition \(h'(0)=0\) holds, although \(0\) is not a local minimizer. Weak
> duality gives \(d^\star\le p^\star\) for every primal–dual problem pair. For
> a differentiable convex problem satisfying Slater’s condition, the supplied
> KKT theorem provides the stated necessary-and-sufficient characterization of
> optimality. Feasibility or stationarity alone does not imply optimality, and
> a displayed \(\arg\min\) does not itself establish existence or uniqueness.

Self-audit:

> The text distinguishes infimum from attainment, stationarity from local
> minimality, and weak from strong duality. The KKT claim retains its stated
> hypotheses and does not add existence or uniqueness.

Independent audit: **PASS**. No substantive mathematical or prose defect was
found.

## CF5-4 — Set-valued maps and projections — PASS

Task contract:

```text
Let S:X⇉U be set-valued. Define
gph S={(x,u)∈X×U:u∈S(x)} and S^{-1}(u)={x∈X:u∈S(x)}, noting that an inverse
value may be empty or contain multiple points. Define the metric projection
P_C(x)=argmin_{c∈C}d(x,c); for arbitrary C it may be empty or multivalued.
Under the hypothesis that C is a nonempty closed convex subset of a Hilbert
space, state that the projection is unique. Reserve “orthogonal projection”
for a closed linear subspace L. Return manuscript-ready English and a
self-audit.
```

Observed output:

> Let \(S:X\rightrightarrows U\) be set-valued, and define
> \[
> \operatorname{gph}S:=\{(x,u)\in X\times U:u\in S(x)\},
> \qquad
> S^{-1}(u):=\{x\in X:u\in S(x)\}.
> \]
> Thus, \(S^{-1}(u)\) is a subset of \(X\) and may be empty or contain multiple
> points. Define the metric projection onto \(C\) by
> \[
> P_C(x):=\operatorname*{arg\,min}_{c\in C}d(x,c).
> \]
> For arbitrary \(C\), \(P_C(x)\) may likewise be empty or multivalued. If
> \(C\) is a nonempty closed convex subset of a Hilbert space, then \(P_C(x)\)
> contains a unique point for each \(x\). When \(C=L\) is a closed linear
> subspace, the resulting map is called the orthogonal projection onto \(L\).

Self-audit:

> Both inverse images and argmin values remain set-valued where appropriate.
> Uniqueness is restricted to the stated Hilbert-space assumptions, and
> “orthogonal projection” is reserved for the closed-linear-subspace case.

Independent audit: **PASS**. No substantive defect was found in the hypotheses,
set-valued distinctions, or terminology.

## CF5-5 — Formal, empirical, and superseded evidence — PASS

Task contract:

```text
Under assumptions H, a theorem gives
||e_k||≤Cρ^k||e_0|| with 0<ρ<1; limit the conclusion to H. In exactly four
benchmark runs on dataset D, method M has lower median runtime than B, but no
uncertainty interval or proof of superiority was supplied, so make no general
superiority claim. A publisher erratum says an old proof step is invalid
because q(a,b) is undefined when gcd(a,b)≠1; the corrected proof replaces that
construction. Mark the old step as superseded rather than current formal
evidence. Return manuscript-ready English and a self-audit.
```

Observed output:

> Under assumptions \(H\), the theorem establishes
> \[
> \lVert e_k\rVert \le C\rho^k\lVert e_0\rVert,\qquad 0<\rho<1.
> \]
> This formal conclusion is limited to \(H\). In four benchmark runs on
> dataset \(D\), method \(M\) had a lower median runtime than method \(B\).
> This empirical observation provides no general superiority claim because no
> uncertainty interval or proof of superiority was supplied. The publisher’s
> erratum marks the earlier proof step as invalid because \(q(a,b)\) was
> undefined when \(\gcd(a,b)\ne1\). The corrected proof replaces that
> construction, so the original step is superseded and should not be cited as
> current formal evidence.

Self-audit:

> Preserves the theorem’s assumptions and bound, confines the benchmark claim
> to the observed runs, and distinguishes the corrected proof from the invalid
> historical step.

Independent audit: **PASS after corrected audit input**. The first audit input
replaced the displayed inequality and \(0<\rho<1\) with the phrase “exact
bound,” then correctly rejected that abbreviated input. The exact observed
output above retained both. Re-audit of the exact output found no defect.

## CF5-6 — Ordered algorithm with conditional acceptance — PASS

Task contract:

```text
Initialize x^0 from the previous solve and set j=0. At iteration k, first
compute y^{k+1}=F(x^k), then form the candidate
x^{k+1}=G(y^{k+1}). State that the inner problem is solved inexactly to the
supplied tolerance. Accept the candidate if the residual test passes;
otherwise retain x^k. Do not invent an update for j, a convergence rate, or a
global-optimality guarantee. Return manuscript-ready prose and a self-audit.
```

Observed output:

> Initialize \(x^0\) with the value carried over from the previous solve and
> set \(j=0\). At iteration \(k\), first compute
> \[
> y^{k+1}=F(x^k),
> \]
> and then form the candidate
> \[
> x^{k+1}=G(y^{k+1}).
> \]
> The inner problem is solved inexactly to the supplied tolerance. If the
> residual test passes, accept \(x^{k+1}\). Otherwise, retain \(x^k\) as the
> current iterate. No convergence rate or global-optimality guarantee is
> asserted.

Self-audit:

> Retains the initialization source, counter value, sequential update order,
> acceptance rule, inexact tolerance, and stated limits. It does not invent an
> update for \(j\).

Independent audit: **PASS**. The reviewer confirmed the state, ordering,
acceptance branch, and absence of invented guarantees.

## CF5-7 — Quotient well-definedness, special cases, and comparison — PASS

Task contract:

```text
Let q:V→V/R be the quotient map. Before defining T~([v])=T(v), require
v-w∈R⇒T(v)=T(w), so the definition is independent of the representative. Say
that q is many-to-one and maps representatives to equivalence classes; do not
call q an equivalence of elements. Setting λ=0 in method B recovers A as a
special case, but does not make the methods equivalent for arbitrary λ. On one
test instance B uses fewer iterations than A; do not infer universal
superiority. Return manuscript-ready English and a self-audit.
```

Observed output:

> Let \(q:V\to V/R\) denote the quotient map. Before defining
> \[
> \widetilde T([v])=T(v),
> \]
> one must verify that
> \[
> v-w\in R\quad\Longrightarrow\quad T(v)=T(w).
> \]
> This condition ensures that \(\widetilde T\) is independent of the chosen
> representative. The quotient map \(q\) is many-to-one and maps
> representatives to equivalence classes. It is not an equivalence of
> elements.
>
> Setting \(\lambda=0\) in method \(B\) recovers method \(A\) as a special
> case, but this specialization does not make the methods equivalent for
> arbitrary \(\lambda\). On one test instance, \(B\) required fewer iterations
> than \(A\). This observation does not establish that \(B\) is universally
> superior.

Self-audit:

> Checks well-definedness before introducing the induced map, preserves the
> quotient-map distinction, and avoids strengthening either the special-case
> relation or the single-instance experiment.

Independent audit: **PASS**. The reviewer found no defect in representative
independence, quotient-map language, special-case scope, or empirical claim
strength.

## Round-five decision

The seven cases passed after exact-output review. The round specifically
exercises long compositions that cross behavior categories rather than isolated
word choices: nested quantification and probability, proof architecture,
optimization and attainment, set-valued constructions, evidence status,
ordered computation, and mapping well-definedness. The result supports these
targeted boundaries only; it is not evidence that all mathematical structures
or domain overlays are complete.
