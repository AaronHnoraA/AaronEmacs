// math.typ --- Shared Typst math helpers for notes
//
// Keep global semantic notation here.  Formula skeletons and simple symbol
// expansions belong in snippets so the note source stays readable.

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
#let Rad = m-op("Rad")
#let ord = m-op("ord")
#let val = m-op("val")

// --- Complexity classes -----------------------------------------------------

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
