// math.typ --- Shared Typst math helpers for notes
//
// Keep semantic, reusable notation here.  Snippets should expand to these
// helpers when a notation is used often or is tedious to type correctly.

// --- Common operators -------------------------------------------------------

#let m-op(name) = math.op(name)

#let re = m-op("Re")
#let im = m-op("Im")
#let sgn = m-op("sgn")
#let sinc = m-op("sinc")
#let rectop = m-op("rect")
#let tr = m-op("Tr")
#let res = m-op("Res")
#let rank = m-op("rank")
#let nullity = m-op("nullity")
#let span = m-op("span")
#let imof = m-op("im")
#let kernel = m-op("ker")
#let cokernel = m-op("coker")
#let codim = m-op("codim")
#let diag = m-op("diag")
#let spec = m-op("Spec")
#let supp = m-op("supp")
#let lcm = m-op("lcm")
#let argmin = m-op("arg min")
#let argmax = m-op("arg max")
#let linsolve = m-op("solve")
#let Obj = m-op("Obj")
#let Mor = m-op("Mor")
#let Ext = m-op("Ext")
#let Tor = m-op("Tor")
#let Ann = m-op("Ann")
#let ord = m-op("ord")
#let val = m-op("val")

// --- Delimiters and elementary notation ------------------------------------

#let parens(x) = $ lr((#x)) $
#let brackets(x) = $ lr([#x]) $
#let braces(x) = $ lr({#x}) $
#let abs(x) = $ lr(|#x|) $
#let absval(x) = abs(x)
#let norm(x) = $ lr(||#x||) $
#let floorb(x) = $ floor.l #x floor.r $
#let ceilb(x) = $ ceil.l #x ceil.r $
#let inner(x, y) = $ chevron.l #x, #y chevron.r $
#let generated(x) = $ chevron.l #x chevron.r $
#let gen(x) = generated(x)
#let setof(x, cond: none) = {
  if cond == none {
    $ lr({#x}) $
  } else {
    $ lr({#x | #cond}) $
  }
}
#let tuple(..xs) = {
  let vals = xs.pos()
  $ lr((#vals.join[$, $])) $
}
#let seq(from, to, var: $ i $) = $ (#var)_#from^#to $
#let indicator(event) = $ bold(1) lr({#event}) $
#let cases2(cond1, val1, cond2, val2) = $ cases(#val1 & #cond1, #val2 & #cond2) $
#let evaluated(expr, lower, upper) = $ lr([#expr])_#lower^#upper $
#let choose(n, k) = $ binom(#n, #k) $
#let falling(x, n) = $ (#x)_#n $
#let rising(x, n) = $ #x^overline(#n) $

// --- Algebra ----------------------------------------------------------------

#let comm(a, b) = $ lr([#a, #b]) $
#let anticomm(a, b) = $ lr({#a, #b}) $
#let conj(g, h) = $ #g #h #g^(-1) $
#let tensor(a, b) = $ #a times.o #b $
#let directsum(a, b) = $ #a plus.o #b $
#let semidirect(a, b) = $ #a ⋉ #b $
#let quotient(a, b) = $ #a / #b $
#let modclass(a, n) = $ [#a]_(#n) $
#let congruent(a, b, n) = $ #a equiv #b mod #n $
#let zmod(n) = $ bb(Z) / #n bb(Z) $
#let finiteField(q) = $ bb(F)_#q $
#let FF(q) = finiteField(q)
#let hom(a, b) = $ "Hom" lr((#a, #b)) $
#let endom(a) = $ "End" lr((#a)) $
#let aut(a) = $ "Aut" lr((#a)) $
#let gal(ext, base) = $ "Gal" lr((#ext / #base)) $
#let GL(n, field) = $ "GL"_#n lr((#field)) $
#let SL(n, field) = $ "SL"_#n lr((#field)) $
#let SO(n) = $ "SO" lr((#n)) $
#let SU(n) = $ "SU" lr((#n)) $
#let matspace(rows, cols, field) = $ #field^(#rows times #cols) $
#let det(a) = $ "det" lr((#a)) $
#let charpoly(a, var: $ lambda $) = $ "det" lr((#var I - #a)) $
#let eigenspace(a, lambda) = $ "ker" lr((#a - #lambda I)) $
#let adjoint(a) = $ #a^* $
#let id = $ I $
#let identity(obj) = $ I_#obj $
#let idop(obj) = identity(obj)
#let compose(f, g) = $ #f compose #g $
#let restrict(f, domain) = $ #f | _#domain $
#let evalat(expr, point) = $ lr([#expr])_(#point) $
#let maps(f, from, to) = $ #f: #from -> #to $
#let iso(a, b) = $ #a tilde.equiv #b $
#let normalSubgroup(a, b) = $ #a triangleleft #b $
#let category(name) = $ cal(#name) $
#let opcat(cat) = $ #cat^("op") $
#let objects(cat) = $ "Obj" lr((#cat)) $
#let morphisms(cat) = $ "Mor" lr((#cat)) $
#let homIn(cat, a, b) = $ "Hom"_#cat lr((#a, #b)) $
#let functor(f, source, target) = $ #f: #source -> #target $
#let nattrans(eta, f, g) = $ #eta: #f arrow.double #g $
#let extGroup(n, a, b) = $ "Ext"^#n lr((#a, #b)) $
#let torGroup(n, a, b) = $ "Tor"_#n lr((#a, #b)) $
#let ideal(name) = $ frak(#name) $
#let ann(module) = $ "Ann" lr((#module)) $
#let spectrum(ring) = $ "Spec" lr((#ring)) $
#let projectiveSpec(ring) = $ "Proj" lr((#ring)) $
#let localization(ring, prime) = $ #ring _ #prime $
#let completion(ring, ideal) = $ hat(#ring) _ #ideal $
#let valuation(x, prime) = $ v_#prime lr((#x)) $
#let legendre(a, p) = $ lr((#a / #p)) $
#let jacobi(a, n) = $ lr((#a / #n)) $
#let divides(a, b) = $ #a | #b $
#let notdivides(a, b) = $ #a not | #b $

// --- Probability, computing, and TCS ---------------------------------------

#let prob(event, sub: none) = {
  if sub == none {
    $ bb(P) lr([#event]) $
  } else {
    $ bb(P)_#sub lr([#event]) $
  }
}
#let expect(expr, sub: none) = {
  if sub == none {
    $ bb(E) lr([#expr]) $
  } else {
    $ bb(E)_#sub lr([#expr]) $
  }
}
#let variance(expr, sub: none) = {
  if sub == none {
    $ "Var" lr([#expr]) $
  } else {
    $ "Var"_#sub lr([#expr]) $
  }
}
#let cov(a, b) = $ "Cov" lr((#a, #b)) $
#let entropy(x) = $ H lr((#x)) $
#let condentropy(x, y) = $ H lr((#x | #y)) $
#let mutualinfo(x, y) = $ I lr((#x; #y)) $
#let kl(p, q) = $ D_("KL") lr((#p || #q)) $

#let complexity(name) = $ sans(#name) $
#let classP = complexity("P")
#let classNP = complexity("NP")
#let coNP = complexity("coNP")
#let BPP = complexity("BPP")
#let BQP = complexity("BQP")
#let QMA = complexity("QMA")
#let PSPACE = complexity("PSPACE")
#let EXP = complexity("EXP")
#let NEXP = complexity("NEXP")
#let PH = complexity("PH")
#let NC = complexity("NC")
#let AC0 = $ sans("AC")^0 $
#let TC0 = $ sans("TC")^0 $
#let logspace = complexity("L")
#let NL = complexity("NL")
#let RP = complexity("RP")
#let ZPP = complexity("ZPP")
#let PP = complexity("PP")
#let IP = complexity("IP")
#let PCP = complexity("PCP")
#let AM = complexity("AM")
#let MA = complexity("MA")
#let SAT = complexity("SAT")
#let CNF = complexity("CNF")
#let DNF = complexity("DNF")
#let DFA = complexity("DFA")
#let NFA = complexity("NFA")
#let TM = complexity("TM")
#let PRG = complexity("PRG")
#let PRF = complexity("PRF")

#let bigO(x) = $ O lr((#x)) $
#let bigOmega(x) = $ Omega lr((#x)) $
#let bigTheta(x) = $ Theta lr((#x)) $
#let softO(x) = $ tilde(O) lr((#x)) $
#let poly(x) = $ "poly" lr((#x)) $
#let polylog(x) = $ "polylog" lr((#x)) $
#let negl(x) = $ "negl" lr((#x)) $
#let lang(name) = $ cal(#name) $
#let oracle(name) = $ cal(#name) $
#let bool = $ lr({0, 1}) $
#let bitstrings(n) = $ lr({0, 1})^#n $
#let bits(n) = bitstrings(n)
#let strings(alphabet, n) = $ #alphabet^#n $
#let ceildiv(a, b) = ceilb($ frac(#a, #b) $)
#let reduce(a, b, kind: "m") = $ #a <=_#kind #b $
#let manyone(a, b) = reduce(a, b, kind: "m")
#let turingred(a, b) = reduce(a, b, kind: "T")
#let runtime(t, n) = $ #t lr((#n)) $
#let advantage(game, adv, sec) = $ "Adv"_#adv^#game lr((#sec)) $
#let accept(machine, input) = $ #machine lr((#input)) = 1 $
#let reject(machine, input) = $ #machine lr((#input)) = 0 $
#let circuit(name, size: none) = {
  if size == none {
    $ cal(#name) $
  } else {
    $ cal(#name)_#size $
  }
}
#let xor(a, b) = $ #a plus.o #b $
#let concat(a, b) = $ #a ++ #b $
#let sample(x, dist) = $ #x arrow.l #dist $
#let uniform(space) = $ #space $
#let languageOf(machine) = $ L lr((#machine)) $
#let transition(delta, state, symbol) = $ #delta lr((#state, #symbol)) $
#let config(machine, input, time) = $ "conf"_#machine lr((#input, #time)) $
#let oracleMachine(machine, oracleName) = $ #machine^#oracleName $
#let negligible(fn: $ epsilon $, sec: $ lambda $) = $ #fn lr((#sec)) <= negl(#sec) $
#let indist(left, right, sec: $ lambda $) = $ #left approx_c #right $
#let securityParam = $ lambda $
#let challengeBit = $ b arrow.l lr({0, 1}) $
#let gamehop(i) = $ "Game"_#i $
#let view(adv) = $ "View"_#adv $

// --- Physics and analysis ---------------------------------------------------

#let deriv(f, x, order: none) = {
  if order == none {
    $ frac(dif #f, dif #x) $
  } else {
    $ frac(dif^#order #f, dif #x^#order) $
  }
}
#let pderiv(f, x, order: none) = {
  if order == none {
    $ frac(partial #f, partial #x) $
  } else {
    $ frac(partial^#order #f, partial #x^#order) $
  }
}
#let grad(f) = $ nabla #f $
#let divergence(v) = $ nabla dot #v $
#let curl(v) = $ nabla times #v $
#let laplacian(f) = $ nabla^2 #f $
#let dalembert(f) = $ square #f $
#let fourier(f) = $ cal(F) lr({#f}) $
#let invfourier(f) = $ cal(F)^(-1) lr({#f}) $
#let convolution(f, g) = $ #f ast #g $
#let dirac(x) = $ delta lr((#x)) $
#let poisson(a, b) = $ lr({#a, #b})_("PB") $
#let lagrangian = $ cal(L) $
#let hamiltonian = $ cal(H) $
#let action = $ cal(S) $
#let hbar = $ "ℏ" $
#let kB = $ k_B $
#let betaT(temp) = $ frac(1, kB #temp) $
#let pathint(expr, fields: $ phi $) = $ integral cal(D) #fields exp(i #action) #expr $
#let schrodinger(state: $ psi(t) $, ham: hamiltonian) = $ i hbar deriv(#state, t) = #ham #state $
#let heisenbergEq(op, ham: hamiltonian) = $ deriv(#op, t) = frac(i, hbar) comm(#ham, #op) $
#let partition(beta, ham: hamiltonian) = $ Z = trace(exp(-#beta #ham)) $
#let gibbs(beta, ham: hamiltonian) = $ rho = frac(exp(-#beta #ham), trace(exp(-#beta #ham))) $
#let boltzmann(energy, temp) = $ exp(-#energy / (kB #temp)) $
#let maxwell1(rho, eps: $ epsilon_0 $) = $ nabla dot bold(E) = frac(#rho, #eps) $
#let maxwell2 = $ nabla dot bold(B) = 0 $
#let maxwell3 = $ nabla times bold(E) = -pderiv(bold(B), t) $
#let maxwell4(current: $ bold(J) $, mu: $ mu_0 $, eps: $ epsilon_0 $) = $ nabla times bold(B) = #mu #current + #mu #eps pderiv(bold(E), t) $
#let lorentz(q, vel: $ bold(v) $) = $ bold(F) = #q lr((bold(E) + #vel times bold(B))) $
#let metric(mu, nu) = $ g_(#mu #nu) $
#let minkowski(mu, nu) = $ eta_(#mu #nu) $
#let christoffel(rho, mu, nu) = $ Gamma^(#rho)_(#mu #nu) $
#let riemann(rho, sigma, mu, nu) = $ R^(#rho)_(#sigma #mu #nu) $
#let ricci(mu, nu) = $ R_(#mu #nu) $
#let einsteinTensor(mu, nu) = $ G_(#mu #nu) $
#let covderiv(index) = $ nabla_#index $
#let lieDeriv(vector, tensor) = $ cal(L)_#vector #tensor $
#let gammaMat(mu) = $ gamma^#mu $
#let diracAdjoint(psi) = $ overline(#psi) $
#let diracEq(psi: $ psi $, mass: $ m $) = $ lr((i gamma^mu partial_mu - #mass)) #psi = 0 $
#let commRel(a, b, value: $ i hbar $) = $ comm(#a, #b) = #value $
#let anticommRel(a, b, value: $ 0 $) = $ anticomm(#a, #b) = #value $
#let uncertainty(a, b) = $ Delta #a Delta #b >= frac(1, 2) lr(|chevron.l comm(#a, #b) chevron.r|) $
#let creation(op) = $ #op^dagger $
#let annihilation(op) = $ #op $

// --- Quantum computing ------------------------------------------------------

#let ket(x) = $ |#x chevron.r $
#let bra(x) = $ chevron.l #x| $
#let braket(x, y) = $ chevron.l #x | #y chevron.r $
#let ketbra(x, y) = $ |#x chevron.r chevron.l #y| $
#let dyad(x, y) = ketbra(x, y)
#let projector(x) = ketbra(x, x)
#let proj(x) = projector(x)
#let expval(op, state: none) = {
  if state == none {
    $ chevron.l #op chevron.r $
  } else {
    $ chevron.l #state | #op | #state chevron.r $
  }
}
#let amp(phi, op, psi) = $ chevron.l #phi | #op | #psi chevron.r $
#let trace(x) = $ "Tr" lr((#x)) $
#let ptrace(sys, x) = $ "Tr"_#sys lr((#x)) $
#let dagger(x) = $ #x^dagger $
#let density(state) = ketbra(state, state)
#let bloch(r) = $ rho = frac(1, 2) lr((I + arrow(#r) dot arrow(sigma))) $
#let kraus(state: $ rho $, index: $ k $, op: $ E $) = $ sum_#index #op_#index #state #op_#index^dagger $

#let gate(name) = $ sans(#name) $
#let CNOT = gate("CNOT")
#let CZ = gate("CZ")
#let SWAP = gate("SWAP")
#let Toffoli = gate("Toffoli")
#let Had = gate("H")
#let ket0 = ket(0)
#let ket1 = ket(1)
#let ketplus = $ frac(ket(0) + ket(1), sqrt(2)) $
#let ketminus = $ frac(ket(0) - ket(1), sqrt(2)) $
#let bell00 = $ frac(ket(0 0) + ket(1 1), sqrt(2)) $
#let pauliX = $ mat(0, 1; 1, 0) $
#let pauliY = $ mat(0, -i; i, 0) $
#let pauliZ = $ mat(1, 0; 0, -1) $
#let hadamard = $ frac(1, sqrt(2)) mat(1, 1; 1, -1) $
#let qft(n, x, y: $ y $) = $ "QFT"_#n ket(#x) = frac(1, sqrt(2^#n)) sum_(#y=0)^(2^#n - 1) exp(2 pi i #x #y / 2^#n) ket(#y) $
