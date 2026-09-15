# Math Prose

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

**Precise, natural mathematical writing for AI agents—without changing the
mathematics.**

Math Prose is an open Agent Skill for drafting, revising, and auditing English
mathematical prose. It helps an AI agent choose language from the mathematical
action being performed, instead of rotating through a list of synonyms.

Use it for research papers, theses, technical reports, theorem statements,
proofs, derivations, notation blocks, optimization problems, dynamical systems,
and prose around displayed equations.

Math Prose follows the open
[Agent Skills specification](https://agentskills.io/specification). Any
skills-compatible agent can load its `SKILL.md`; agents without native skill
support can use the same workflow by reading that file directly.

## Why Math Prose?

Many mathematical sentences contain *is*, but they do not all perform the same
action. A sentence may assign notation, introduce a definition, provide an
explicit formula, report a calculation, state an assumption, or draw a logical
consequence. Those distinctions determine the appropriate wording.

| Mathematical action | Typical construction |
|---|---|
| Assign a symbol | `Let x denote ...` |
| Introduce a definition | `Define r by r(x):=Ax-b.` |
| Supply an existing object's formula | `The coefficient is given by ...` |
| Rewrite an equivalent relation | `Equivalently, ...` or `can be rewritten as ...` |
| Report a calculation | `Substituting ... into ... gives ...` |
| Draw a logical consequence | `It follows from ... that ...` |
| State an assumption | `Given ...`, `Suppose ...`, or `Assume ...` |
| Interpret a quantity | `represents`, `measures`, or `encodes` |

The goal is not to eliminate *is*. Plain forms such as *is* and *are* remain
the clearest choice for identity, membership, status, and many mathematical
properties.

### It also avoids mechanical variation

A synonym-only edit can replace one repeated frame with another:

```text
The measured state collects these quantities in the ordered tuple X=(...).
The desired state collects these quantities in the ordered tuple X_d=(...).
```

When the surrounding mathematics supports distinct roles, Math Prose can vary
the information structure instead:

```text
The measured variables form the state tuple X=(...).
Define the desired reference by X_d=(...).
```

If the two objects are genuinely parallel, the skill retains the parallel
wording. It never trades mathematical precision for surface-level variety.

## Quick Start

### 1. Install the skill

For a project-level installation using the cross-client `.agents/skills`
convention:

```bash
mkdir -p .agents/skills
git clone https://github.com/TianhuaGao/math-prose.git \
  .agents/skills/math-prose
```

For a user-level installation:

```bash
mkdir -p ~/.agents/skills
git clone https://github.com/TianhuaGao/math-prose.git \
  ~/.agents/skills/math-prose
```

Skill discovery paths and invocation syntax vary by agent client. If your
client uses a different skill directory or provides its own installer, use the
client's documented installation method.

If your agent has no native skill support, clone the repository anywhere the
agent can read it, then ask the agent to open `math-prose/SKILL.md` and resolve
its linked resources relative to the repository root.

### 2. Ask the agent to use Math Prose

```text
Use the math-prose skill to revise this paragraph while preserving every
equation, symbol, assumption, number, and citation.
```

A skills-compatible agent may also select Math Prose automatically when the
request matches the skill description.

### 3. Provide the mathematical contract

For the most reliable result, include:

- the formulas and current prose;
- notation, numbers, and citations that must remain unchanged;
- known assumptions and the intended claim strength;
- the desired mode: draft, revise, or audit only;
- venue, terminology, or style constraints.

## What Can I Ask It to Do?

### Draft from mathematics

```text
Write the prose around these equations. Introduce the given data, define the
decision variables, state the optimization problem, and interpret the solution
without adding convexity, feasibility, or uniqueness claims.
```

### Revise existing prose

```text
Revise this notation block. Reduce repeated sentence frames, but preserve all
formulas and keep deliberate parallelism where the objects play parallel roles.
```

### Audit without rewriting

```text
Audit this theorem statement and proof for undefined notation, false
equivalence, unclear antecedents, and unsupported inference language. Report
the issues without rewriting the text.
```

### Check logical force

```text
Check whether “deduce,” “therefore,” “equivalently,” and “guarantees” are
justified by the visible derivation. Do not strengthen any claim.
```

## What It Protects

By default, Math Prose preserves:

- formulas, symbols, operators, indices, signs, and quantifiers;
- assumptions, domains, codomains, admissible sets, and edge conditions;
- approximation order, implication direction, and equivalence;
- numbers, units, citations, and stated claim strength;
- stable technical terminology required by the field.

It does not silently repair ambiguous mathematics, invent missing hypotheses,
fabricate a derivation or proof, or claim that numerical evidence establishes a
theorem. When the source relation is ambiguous, the skill keeps that ambiguity
visible and reports the possible interpretations.

## How It Works

For each equation-adjacent sentence, the skill:

1. records the mathematical content that must remain unchanged;
2. classifies the sentence's mathematical behavior;
3. identifies its position before, within, or after the displayed equation;
4. selects a construction licensed by that behavior;
5. checks the logical strength of inference and result language;
6. revises sentence structure before considering vocabulary changes;
7. compares nearby sentence frames and runs a preservation audit.

The behavior taxonomy contains 31 cases in six groups:

| Group | Coverage |
|---|---|
| A. Declaration and scope | variables, notation, definitions, types, assumptions, indexing |
| B. Representation and construction | mappings, formulas, decompositions, parameterizations, aggregation |
| C. Transformation and equivalence | substitution, rewriting, simplification, approximation, normalization |
| D. Inference and results | implication, derivation, bounds, existence, uniqueness, optimality, convergence |
| E. Dynamics and computation | differential equations, recurrences, algorithms, initialization, updates |
| F. Interpretation and comparison | meaning, measurement, encoding, contrast, limiting behavior |

See the full
[behavior taxonomy](references/behavior-taxonomy.md),
[equation-discourse guide](references/equation-discourse.md), and
[proof and claim-language guide](references/proof-and-claim-language.md).

## Evidence Base

The core corpus currently contains:

- **54 influential anchors**: 26 research articles, 22 classic or established
  textbooks, 4 research monographs, and 2 surveys or guides, plus 1 comparison
  source retained for a corrected-proof boundary;
- **267 localized, paraphrased prose observations**;
- coverage of **all 31 mathematical behavior codes**;
- **31 synthesized patterns**, each supported by at least three independent
  anchors and at least two core disciplines;
- **231 reusable construction frames**, **185 misuse boundaries**, and
  **44 evidence-linked boundary cases** across those patterns;
- 15 anchors in optimization and numerical analysis, and 13 each in pure
  mathematics, applied mathematics, and probability and statistics.

Research articles provide evidence for publication-state compression, while
textbooks and the research monographs provide fuller evidence for definitions,
constructions, derivations, and proofs. The survey-and-guide anchor contributes
reference-style definitions, validity conditions, and bounds. Sources act as
evidence anchors, not as authors to imitate.

The public corpus stores bibliographic metadata, precise locators, behavior
annotations, cue phrases, and paraphrased observations. It contains no paper or
textbook full text and currently stores no verbatim source quotations.
The concrete phrase inventory lives in each pattern record's `constructions`
array; the adjacent `boundaries` explain when a frame is too strong, ambiguous,
or mathematically inapplicable. High-risk boundaries additionally carry
counterexample, misuse, or near-synonym cases linked to observations, forward
evaluations, or both.

The latest expansion adds explicit evidence and misuse boundaries for
frequentist coverage, significance, and power; convergence and analytic
interchange conditions; weak, strong, and classical PDE solution claims; and
conditioning, stability, error estimators, and observed numerical rates. It
does not infer general estimator consistency or identifiability from the
model-selection-consistency evidence currently present.

Explore the
[core corpus](references/corpora/math-core.jsonl) and the
[corpus construction method](references/corpus-method.md).
The [`domain/control-systems`](https://github.com/TianhuaGao/math-prose/tree/domain/control-systems)
branch adds 3 reviewed control-systems anchors, 15 localized observations, and
2 validated domain patterns without mixing field-specific flow/jump,
barrier-function, and predictive-control semantics into the transferable core.

## Corpus Sources

The list below is the complete source registry for the current core corpus:
54 anchors and 1 comparison source.
Each entry links to the stable access or metadata page recorded in the corpus;
the JSONL registry remains authoritative for editions, access constraints,
influence evidence, and observation locators.

### Applied Mathematics

- Claude E. Shannon. [*A Mathematical Theory of Communication*](https://doi.org/10.1002/j.1538-7305.1948.tb01338.x). Bell System Technical Journal, 1948. — `research-article`
- A. M. Turing. [*The Chemical Basis of Morphogenesis*](https://royalsocietypublishing.org/doi/10.1098/rstb.1952.0012). Philosophical Transactions of the Royal Society B, 1952. — `research-article`
- Leonid I. Rudin, Stanley Osher, Emad Fatemi. [*Nonlinear Total Variation Based Noise Removal Algorithms*](https://doi.org/10.1016/0167-2789(92)90242-F). Physica D, 1992. — `research-article`
- Michael G. Crandall, Hitoshi Ishii, Pierre-Louis Lions. [*User's Guide to Viscosity Solutions of Second Order Partial Differential Equations*](https://arxiv.org/abs/math/9207212). Bulletin of the American Mathematical Society, 1992. — `research-article`
- Peter J. Olver. [*Applications of Lie Groups to Differential Equations*](https://link.springer.com/book/10.1007/978-1-4612-4350-2). Springer, 1993. — `textbook`
- Richard Jordan, David Kinderlehrer, Felix Otto. [*The Variational Formulation of the Fokker–Planck Equation*](https://doi.org/10.1137/S0036141096303359). SIAM Journal on Mathematical Analysis, 1998. — `research-article`
- David J. C. MacKay. [*Information Theory, Inference, and Learning Algorithms*](https://www.inference.org.uk/mackay/itila/book.html). Cambridge University Press, 2003. — `textbook`
- Emmanuel J. Candès, Justin Romberg, Terence Tao. [*Robust Uncertainty Principles: Exact Signal Reconstruction from Highly Incomplete Frequency Information*](https://arxiv.org/abs/math/0409186). IEEE Transactions on Information Theory, 2006. — `research-article`
- Terence Tao. [*Nonlinear Dispersive Equations: Local and Global Analysis*](https://bookstore.ams.org/cbms-106). American Mathematical Society, 2006. — `research-monograph`
- Lawrence C. Evans. [*Partial Differential Equations*](https://bookstore.ams.org/gsm-19-r). American Mathematical Society, 2010. — `textbook`
- Gerald Teschl. [*Ordinary Differential Equations and Dynamical Systems*](https://www.mat.univie.ac.at/~gerald/ftp/book-ode/index.html). American Mathematical Society, 2012. — `textbook`
- Karl Johan Åström, Richard M. Murray. [*Feedback Systems: An Introduction for Scientists and Engineers*](https://fbswiki.org/wiki/index.php/Main_Page). Princeton University Press, 2021. — `textbook`
- NIST Digital Library of Mathematical Functions Editorial Board. [*NIST Digital Library of Mathematical Functions*](https://dlmf.nist.gov/). National Institute of Standards and Technology, 2026. — `survey-or-guide`

### Optimization and Numerical Analysis

- Herbert Robbins, Sutton Monro. [*A Stochastic Approximation Method*](https://doi.org/10.1214/aoms/1177729586). Annals of Mathematical Statistics, 1951. — `research-article`
- Y. E. Nesterov. [*A Method for Solving the Convex Programming Problem with Convergence Rate O(1/k^2)*](https://www.mathnet.ru/eng/dan46009). Soviet Mathematics Doklady, 1983. — `research-article`
- Mark Ainsworth, J. Tinsley Oden. [*A Posteriori Error Estimation in Finite Element Analysis*](https://jtoden.oden.utexas.edu/wp-content/uploads/2013/06/1997-006.a_posteriori.pdf). Computer Methods in Applied Mechanics and Engineering, 1997. — `survey-or-guide`
- Lloyd N. Trefethen, David Bau III. [*Numerical Linear Algebra*](https://people.maths.ox.ac.uk/trefethen/text.html). Society for Industrial and Applied Mathematics, 1997. — `textbook`
- Douglas N. Arnold, Franco Brezzi, Bernardo Cockburn, L. Donatella Marini. [*Unified Analysis of Discontinuous Galerkin Methods for Elliptic Problems*](https://www-users.cse.umn.edu/~arnold/papers/dgerr.pdf). SIAM Journal on Numerical Analysis, 2002. — `research-article`
- Nicholas J. Higham. [*Accuracy and Stability of Numerical Algorithms*](https://nhigham.com/accuracy-and-stability-of-numerical-algorithms/). Society for Industrial and Applied Mathematics, 2002. — `research-monograph`
- Stephen Boyd, Lieven Vandenberghe. [*Convex Optimization*](https://web.stanford.edu/~boyd/cvxbook/). Cambridge University Press, 2004. — `textbook`
- Jorge Nocedal, Stephen J. Wright. [*Numerical Optimization*](https://link.springer.com/book/10.1007/978-0-387-40065-5). Springer, 2006. — `textbook`
- Susanne C. Brenner, L. Ridgway Scott. [*The Mathematical Theory of Finite Element Methods*](https://link.springer.com/book/10.1007/978-0-387-75934-0). Springer, 2008. — `textbook`
- Amir Beck, Marc Teboulle. [*A Fast Iterative Shrinkage-Thresholding Algorithm for Linear Inverse Problems*](https://doi.org/10.1137/080716542). SIAM Journal on Imaging Sciences, 2009. — `research-article`
- Antonin Chambolle, Thomas Pock. [*A First-Order Primal-Dual Algorithm for Convex Problems with Applications to Imaging*](https://optimization-online.org/2010/06/2646/). Journal of Mathematical Imaging and Vision, 2011. — `research-article`
- John Duchi, Elad Hazan, Yoram Singer. [*Adaptive Subgradient Methods for Online Learning and Stochastic Optimization*](https://www.jmlr.org/papers/v12/duchi11a.html). Journal of Machine Learning Research, 2011. — `research-article`
- Stephen Boyd, Neal Parikh, Eric Chu, Borja Peleato, Jonathan Eckstein. [*Distributed Optimization and Statistical Learning via the Alternating Direction Method of Multipliers*](https://stanford.edu/~boyd/papers/admm_distr_stats.html). Foundations and Trends in Machine Learning, 2011. — `research-article`
- Dimitri P. Bertsekas. [*Nonlinear Programming*](https://www.athenasc.com/nonlinbook.html). Athena Scientific, 2016. — `textbook`
- R. Tyrrell Rockafellar, Roger J-B Wets. [*Variational Analysis*](https://link.springer.com/book/10.1007/978-3-642-02431-3). Springer, 1998. — `research-monograph`

### Probability and Statistics

- Nicholas Metropolis, Arianna W. Rosenbluth, Marshall N. Rosenbluth, Augusta H. Teller, Edward Teller. [*Equation of State Calculations by Fast Computing Machines*](https://www.osti.gov/biblio/4390578). Journal of Chemical Physics, 1953. — `research-article`
- E. L. Kaplan, Paul Meier. [*Nonparametric Estimation from Incomplete Observations*](https://doi.org/10.1080/01621459.1958.10501452). Journal of the American Statistical Association, 1958. — `research-article`
- W. K. Hastings. [*Monte Carlo Sampling Methods Using Markov Chains and Their Applications*](https://academic.oup.com/biomet/article-abstract/57/1/97/284580). Biometrika, 1970. — `research-article`
- D. R. Cox. [*Regression Models and Life-Tables*](https://rss.onlinelibrary.wiley.com/doi/10.1111/j.2517-6161.1972.tb00899.x). Journal of the Royal Statistical Society: Series B, 1972. — `research-article`
- Bradley Efron. [*Bootstrap Methods: Another Look at the Jackknife*](https://projecteuclid.org/journals/annals-of-statistics/volume-7/issue-1/Bootstrap-Methods-Another-Look-at-the-Jackknife/10.1214/aos/1176344552.full). Annals of Statistics, 1979. — `research-article`
- David Williams. [*Probability with Martingales*](https://www.cambridge.org/highereducation/books/probability-with-martingales/B4CFCE0D08930FB46C6E93E775503926). Cambridge University Press, 1991. — `textbook`
- Charles M. Grinstead, J. Laurie Snell. [*Introduction to Probability*](https://math.dartmouth.edu/~prob/prob/prob.pdf). American Mathematical Society, 1997. — `textbook`
- Patrick Billingsley. [*Probability and Measure*](https://www.wiley-vch.de/en/areas-interest/mathematics-statistics/statistics-16st/probability-mathematical-statistics-16st1/probability-and-measure-978-1-118-12237-2). John Wiley & Sons, 2012. — `textbook`
- Matthew D. Hoffman, Andrew Gelman. [*The No-U-Turn Sampler: Adaptively Setting Path Lengths in Hamiltonian Monte Carlo*](https://jmlr.org/papers/v15/hoffman14a.html). Journal of Machine Learning Research, 2014. — `research-article`
- Bradley Efron, Trevor Hastie. [*Computer Age Statistical Inference: Algorithms, Evidence, and Data Science*](https://www.cambridge.org/core/books/computer-age-statistical-inference/E32C1911ED937D75CE159BBD21684D37). Cambridge University Press, 2016. — `research-monograph`
- David A. Levin, Yuval Peres, Elizabeth L. Wilmer. [*Markov Chains and Mixing Times*](https://pages.uoregon.edu/dlevin/MARKOV/). American Mathematical Society, 2017. — `textbook`
- Rick Durrett. [*Probability: Theory and Examples*](https://www.cambridge.org/core/books/probability/DD9A1907F810BB14CCFF022CDFC5677A). Cambridge University Press, 2019. — `textbook`
- Simo Särkkä, Arno Solin. [*Applied Stochastic Differential Equations*](https://www.cambridge.org/core/books/applied-stochastic-differential-equations/6BB1B8B0819F8C12616E4A0C78C29EAA). Cambridge University Press, 2019. — `textbook`

### Pure Mathematics

- N. Aronszajn. [*Theory of Reproducing Kernels*](https://www.ams.org/tran/1950-068-03/S0002-9947-1950-0051437-7/S0002-9947-1950-0051437-7.pdf). Transactions of the American Mathematical Society, 1950. — `research-article`
- M. F. Atiyah, I. M. Singer. [*The Index of Elliptic Operators: I*](https://annals.math.princeton.edu/1968/87-3/p05). Annals of Mathematics, 1968. — `research-article`
- Andrew Wiles. [*Modular Elliptic Curves and Fermat's Last Theorem*](https://annals.math.princeton.edu/1995/141-3/p01). Annals of Mathematics, 1995. — `research-article`
- W. T. Gowers. [*A New Proof of Szemerédi's Theorem*](https://www.dpmms.cam.ac.uk/~wtg10/papers.html). Geometric and Functional Analysis, 2001. — `research-article`
- Allen Hatcher. [*Algebraic Topology*](https://pi.math.cornell.edu/~hatcher/AT/ATpage.html). Cambridge University Press, 2002. — `textbook`
- Grisha Perelman. [*The Entropy Formula for the Ricci Flow and Its Geometric Applications*](https://arxiv.org/abs/math/0211159). arXiv, 2002. — `research-article`
- Manindra Agrawal, Neeraj Kayal, Nitin Saxena. [*PRIMES Is in P*](https://annals.math.princeton.edu/2004/160-2/p12). Annals of Mathematics, 2004. — `research-article`
- Terence Tao. [*Analysis I*](https://terrytao.wordpress.com/books/analysis-i/). Hindustan Book Agency, 2006. — `textbook`
- Ben Green, Terence Tao. [*The Primes Contain Arbitrarily Long Arithmetic Progressions*](https://annals.math.princeton.edu/2008/167-2/p03). Annals of Mathematics, 2008. — `research-article`
- Paolo Aluffi. [*Algebra: Chapter 0*](https://bookstore.ams.org/gsm-104/). American Mathematical Society, 2009. — `textbook`
- Haim Brezis. [*Functional Analysis, Sobolev Spaces and Partial Differential Equations*](https://link.springer.com/book/10.1007/978-0-387-70914-7). Springer, 2011. — `textbook`
- John M. Lee. [*Introduction to Smooth Manifolds*](https://sites.math.washington.edu/~lee/Books/ISM/). Springer, 2013. — `textbook`
- Sheldon Axler. [*Linear Algebra Done Right*](https://linear.axler.net/index.html). Springer, 2024. — `textbook`

### Comparison Evidence

- Manindra Agrawal, Neeraj Kayal, Nitin Saxena. [*Errata: PRIMES Is in P*](https://annals.math.princeton.edu/2019/189-1/p06). Annals of Mathematics, 2019. — `research-article`, `comparison`

## Repository Structure

```text
math-prose/
├── SKILL.md                         # Agent workflow and trigger description
├── agents/openai.yaml               # Optional client UI metadata
├── references/
│   ├── behavior-taxonomy.md         # The 31 mathematical behaviors
│   ├── equation-discourse.md        # Equation and paragraph structures
│   ├── proof-and-claim-language.md  # Logical-force safeguards
│   ├── corpus-method.md             # Evidence and annotation protocol
│   ├── core-evaluation.md           # Forward-test record
│   ├── core-evaluation-round5-artifacts.md
│   │                                # Boundary-composition test artifacts
│   ├── core-evaluation-round6-artifacts.md
│   │                                # Inference and analysis boundary tests
│   └── corpora/math-core.jsonl      # Core evidence registry
├── scripts/validate_corpus.py       # Corpus and readiness validator
└── tests/test_validate_corpus.py    # Validator tests
```

The runtime entry point stays concise. Detailed guidance and corpus data are
loaded only when a task needs them.

## Validation

The validator uses only the Python standard library. It reports construction,
boundary, and evidence-linked boundary-case totals; validates secondary
behaviors, semantic section roles, and appendix or supplement locations; and
rejects case- or whitespace-normalized duplicates.

Validate the corpus and its readiness gate:

```bash
python3 scripts/validate_corpus.py --require-core-ready \
  references/corpora/math-core.jsonl
```

Run the test suite:

```bash
python3 -m unittest discover -s tests -v
```

If the Agent Skills reference validator is installed, validate the skill
package with:

```bash
skills-ref validate .
```

The current forward tests cover definitions, mapping direction, proof-claim
strength, quotient construction, metric projection, asymptotic operations,
finite-dimensional and inexact surrogates, conditional uniqueness, semigroup
generation, missing-relation handling, terminal initialization, convergence
modes, invariant-set limits, dependency, comparison, bounded interpretation,
ordered updates, nested quantifiers, proof architectures, attainment,
set-valued maps, empirical-evidence scope, corrected-proof status, and local
repetition. See the
[core evaluation record](references/core-evaluation.md).

## Contributing

Issues and pull requests are welcome. Corpus contributions should keep three
evidence levels separate:

1. a source record with stable bibliographic, access, genre, and influence
   evidence;
2. a localized observation with an auditable locator and paraphrased summary;
3. a synthesized pattern with explicit boundaries and supporting source IDs;
4. for high-risk distinctions, an evidence-linked counterexample, misuse, or
   near-synonym boundary case.

Please do not add full papers, textbook chapters, or long quotations. A pattern
may be marked `validated` only when matching observations support it in at
least three independent anchor sources from the same corpus layer. For a
domain-general core pattern, evidence should span at least two disciplines.

Before submitting a pull request, run the corpus validator and unit tests.

## License

Math Prose is released under the [MIT License](LICENSE).
