# Typst Math Macros

这份表维护 note 里的数学宏和对应 snippet。宏源文件是
[lisp/note/typst/math.typ](../lisp/note/typst/math.typ)，同步后落到 note 根目录的
`/_typst/math.typ`。日常 note 仍然只需要：

```typst
#import "/_typst/note.typ": *
#show: note-entry
```

[lisp/note/typst/note.typ](../lisp/note/typst/note.typ) 会 import `/_typst/math.typ` 并 re-export。

## 宏和 snippet 的分工

- **宏**：放高频、带参数、需要统一排版、Typst 原生不方便输入或容易写错的数学记号。
- **snippet**：负责快速插入宏调用，例如 `ket` 展开成 `ket(psi)`，不再复制完整 bra-ket 写法。
- **不做宏**：Typst 已经很短的原生符号和一次性公式，例如普通 `sum`、`sqrt`、`frac`，保留 snippet 即可。

Typst 公式仍用 dollar signs。`$ket(psi)$` 是行内公式；`$ ket(psi) $` 因为公式首尾有空格，会排成 display equation。

## Common

| Macro | Snippet | Expands to / purpose |
| --- | --- | --- |
| `abs(x)` / `absval(x)` | `norm` still inserts raw absolute value | Absolute value with scalable bars |
| `norm(x)` | `Norm` | Norm with scalable double bars |
| `floorb(x)` | - | Floor with explicit delimiters |
| `ceilb(x)` | - | Ceiling with explicit delimiters |
| `inner(x, y)` | `inner` | Inner product `⟨x, y⟩` |
| `generated(x)` / `gen(x)` | `gen` | Generated subgroup/span notation `⟨x⟩` |
| `setof(x, cond: P)` | `setof` | Set comprehension |
| `tuple(a, b, c)` | - | Parenthesized tuple |
| `indicator(E)` | `indic` | Indicator `1{E}` |
| `choose(n, k)` | `choose` | Binomial coefficient |
| `falling(x, n)` / `rising(x, n)` | `falling` | Falling / rising factorial notation |
| `re`, `im`, `sgn`, `sinc`, `rectop`, `tr`, `res`, `rank`, `span`, `imof`, `kernel`, `argmin`, `argmax` | `Re`, `Im`, `Tr` where existing | Common named math operators |
| `Obj`, `Mor`, `Ext`, `Tor`, `Ann`, `ord`, `val` | - | Extra named operators for algebra / category / number theory notes |
| `cases2(c1, v1, c2, v2)` | - | Two-branch cases expression |
| `evaluated(expr, a, b)` | - | Evaluation bracket with lower/upper limits |

## Algebra

| Macro | Snippet | Purpose |
| --- | --- | --- |
| `comm(A, B)` | `comm` | Commutator `[A, B]` |
| `anticomm(A, B)` | `anticomm` | Anticommutator `{A, B}` |
| `tensor(V, W)` | `ox` for raw symbol | Tensor product |
| `directsum(V, W)` | - | Direct sum |
| `semidirect(G, H)` | `semidirect` | Semidirect product |
| `quotient(G, H)` | `quot` | Quotient object |
| `modclass(a, n)` / `congruent(a, b, n)` | `modclass`, `congmod` | Modular residue and congruence |
| `zmod(n)` | `zmod` | `Z / nZ` |
| `finiteField(q)` / `FF(q)` | `FF` | Finite field |
| `hom(V, W)` / `endom(V)` | `hom` | Hom / End |
| `aut(G)` / `gal(L, K)` | `aut`, `gal` | Automorphism and Galois groups |
| `GL(n, F)` / `SL(n, F)` | `GL` | Linear groups |
| `SO(n)` / `SU(n)` | - | Orthogonal / unitary groups |
| `det(A)`, `charpoly(A)`, `eigenspace(A, lambda)` | `det`, `charpoly`, `eigenspace` | Linear algebra shorthand |
| `identity(V)` / `id` | - | Identity operator/object |
| `compose(f, g)` | - | Function composition |
| `restrict(f, A)` | - | Restriction |
| `evalat(f(x), x = 0)` | - | Evaluation at a point |
| `maps(f, X, Y)`, `iso(G, H)`, `normalSubgroup(N, G)` | `maps`, `isom` | Maps, isomorphism, normal subgroup |
| `category(C)` / `opcat(C)` | `cat`, `opcat` | Category and opposite category |
| `objects(C)` / `morphisms(C)` / `homIn(C, A, B)` | `homin` | Category objects, morphisms, and Hom-sets |
| `functor(F, C, D)` / `nattrans(eta, F, G)` | `functor`, `nattrans` | Functor and natural transformation notation |
| `extGroup(n, A, B)` / `torGroup(n, A, B)` | `Ext`, `Tor` | Ext / Tor group notation |
| `ideal(a)`, `ann(M)`, `spectrum(R)`, `projectiveSpec(R)` | `ideal`, `Spec` | Commutative algebra / algebraic geometry shorthand |
| `localization(R, p)` / `completion(R, I)` | `loc` | Localized and completed rings/modules |
| `valuation(x, p)`, `legendre(a, p)`, `jacobi(a, n)` | `val`, `legendre` | Valuation and quadratic-residue symbols |
| `divides(a, b)` / `notdivides(a, b)` | - | Divisibility notation |

## Probability / Computing / TCS

| Macro | Snippet | Purpose |
| --- | --- | --- |
| `prob(E, sub: X)` | `prob` | Probability |
| `expect(f(x), sub: x in D)` | `expect` | Expectation |
| `variance(X)` / `cov(X, Y)` | `var` | Variance / covariance |
| `entropy(X)` / `condentropy(X, Y)` | `entropy`, `condH` | Entropy |
| `mutualinfo(X, Y)` / `kl(P, Q)` | `mi`, `kl` | Information measures |
| `classP`, `classNP`, `coNP`, `BPP`, `BQP`, `QMA`, `PSPACE`, `EXP`, `NEXP`, `PH`, `NC`, `AC0`, `TC0`, `logspace`, `NL`, `RP`, `ZPP`, `PP`, `IP`, `PCP`, `AM`, `MA` | existing class snippets | Complexity classes |
| `SAT`, `CNF`, `DNF`, `DFA`, `NFA`, `TM`, `PRG`, `PRF` | `SAT`, `DFA`, `NFA`, `TM`, `PRG`, `PRF` | Common problem/model/crypto abbreviations |
| `bigO(f(n))`, `bigOmega(f(n))`, `bigTheta(f(n))`, `softO(f(n))` | `bigO`, `bigOmega`, `bigTheta`, `tildeO` | Asymptotic notation |
| `poly(n)`, `polylog(n)`, `negl(lambda)` | `poly`, `polylog`, `negl` | Complexity shorthand |
| `bitstrings(n)` / `bits(n)` / `bool` | `bits` | Bitstring spaces |
| `strings(Sigma, n)` | - | Strings over an alphabet |
| `ceildiv(n, 2)` | `ceildiv` | Ceiling division |
| `reduce(A, B, kind: "m")`, `manyone(A, B)`, `turingred(A, B)` | `reduce`, `manyone`, `turingred` | Reduction notation |
| `runtime(T, n)` | - | Running-time notation |
| `advantage(game, A, lambda)` | `advantage` | Cryptographic/TCS advantage |
| `accept(M, x)`, `reject(M, x)` | `accept` | Machine acceptance/rejection predicates |
| `circuit(C, size: n)` | `circuit` | Circuit family notation |
| `xor(x, y)`, `concat(x, y)`, `sample(x, D)` | `mxor`, `mconcat`, `msample` | Bit operations and sampling assignment |
| `lang(L)` / `oracle(O)` | existing `lang`, `oracle` | Languages and oracles |
| `languageOf(M)`, `transition(delta, q, a)`, `config(M, x, t)` | `langof`, `trans` | Automata/Turing-machine notation |
| `oracleMachine(M, O)` | `omach` | Oracle machine |
| `negligible(fn: eps, sec: lambda)` / `indist(X, Y)` | `neglb` | Cryptographic negligible and indistinguishability shorthand |
| `securityParam`, `challengeBit`, `gamehop(i)`, `view(A)` | `secpar`, `chalbit`, `gamehop` | Security parameter, game hops, adversary view |

## Physics / Analysis

| Macro | Snippet | Purpose |
| --- | --- | --- |
| `deriv(f, x)` / `deriv(f, x, order: 2)` | `deriv` | Total derivative |
| `pderiv(f, x)` / `pderiv(f, x, order: 2)` | `pderiv` | Partial derivative |
| `grad(f)`, `divergence(F)`, `curl(F)`, `laplacian(f)`, `dalembert(f)` | `grad`, `curl`, `lap`, `dalembert` | Vector calculus / wave operator |
| `fourier(f)` / `invfourier(f)` | `fourier` | Fourier transform |
| `convolution(f, g)`, `dirac(x)` | `conv`, `dirac` | Convolution and Dirac delta |
| `poisson(f, g)` | - | Poisson bracket |
| `lagrangian`, `hamiltonian`, `action`, `hbar`, `kB` | `LL`, `HH`, `hbar` | Physics constants/objects |
| `pathint(O, fields: phi)` | - | Path integral shorthand |
| `schrodinger(state: ket(psi(t)))` | `sch` | Schrodinger equation |
| `heisenbergEq(A)` | `heisenberg` | Heisenberg equation |
| `partition(beta)` / `gibbs(beta)` / `boltzmann(E, T)` | `partition`, `gibbs`, `boltzmann` | Statistical mechanics |
| `maxwell1(rho)`, `maxwell2`, `maxwell3`, `maxwell4()` | `maxwell` | Maxwell equations |
| `lorentz(q)` | `lorentz` | Lorentz force |
| `metric(mu, nu)` / `minkowski(mu, nu)` | `metric` | Metric tensors |
| `christoffel(rho, mu, nu)` / `riemann(rho, sigma, mu, nu)` | `christoffel`, `riemann` | Christoffel symbol and Riemann tensor |
| `ricci(mu, nu)` / `einsteinTensor(mu, nu)` | - | Ricci and Einstein tensors |
| `covderiv(mu)` / `lieDeriv(X, T)` | - | Covariant and Lie derivatives |
| `gammaMat(mu)`, `diracAdjoint(psi)`, `diracEq()` | `gammamat`, `diraceq` | Dirac/QFT shorthand |
| `commRel(A, B)`, `anticommRel(A, B)`, `uncertainty(A, B)` | `commrel`, `uncertainty` | Canonical relations and uncertainty bound |
| `creation(a)` / `annihilation(a)` | `create`, `annihilate` | Creation / annihilation operator notation |

## Quantum Computing

| Macro | Snippet | Purpose |
| --- | --- | --- |
| `ket(psi)`, `bra(phi)` | `ket`, `bra` | Bra-ket notation |
| `braket(phi, psi)` | `bk` | Inner bra-ket |
| `ketbra(psi, phi)` / `dyad(psi, phi)` | `kb`, `dyad`, `outer` | Outer product |
| `projector(psi)` / `proj(psi)` | `proj` | Projector |
| `expval(A, state: psi)` | `expval` | Expectation value |
| `amp(phi, A, psi)` | `amp`, `me` | Matrix element / transition amplitude |
| `trace(A)` / `ptrace(B, rho)` | `trace`, `Tr`, `ptr` | Trace and partial trace |
| `dagger(U)` | `dag` remains raw suffix | Dagger |
| `density(psi)` | `density` | Pure-state density operator |
| `bloch(r)` | `bloch` | Bloch form |
| `kraus(state: rho, index: k, op: E)` | `kraus` | Kraus map summand |
| `gate("U")`, `CNOT`, `CZ`, `SWAP`, `Toffoli`, `Had` | `qgate`, `cnot`, `had` | Gates |
| `ket0`, `ket1`, `ketplus`, `ketminus`, `bell00` | `ket0`, `ket1`, `bell` | Common states |
| `pauliX`, `pauliY`, `pauliZ`, `hadamard` | `pauliX`, `pauliY`, `pauliZ`, `hadamard` | Common matrices |
| `qft(n, x)` | `qft` | Quantum Fourier transform |

## Examples

```typst
$ ket(psi) + bra(phi) + braket(phi, psi) + ketbra(psi, phi) $

$ expval(A, state: psi) = trace(A density(psi)) $

$ prob(E, sub: x in D) <= negl(lambda) $

$ GL(n, FF(q)) acts on FF(q)^n $

$ functor(F, category(C), category(D)) quad nattrans(eta, F, G) $

$ oracleMachine(M, oracle(O)) quad languageOf(M) $

$ schrodinger(state: ket(psi(t))) $

$ christoffel(rho, mu, nu) quad riemann(rho, sigma, mu, nu) $
```
