# Corpus Construction and Synthesis

## Contents

- [Purpose](#purpose)
- [Three record levels](#three-record-levels)
- [Annotation axes](#annotation-axes)
- [Influence gate](#influence-gate)
- [Source genres](#source-genres)
- [Core and domain layers](#core-and-domain-layers)
- [Sampling strategy](#sampling-strategy)
- [Extraction unit](#extraction-unit)
- [Pattern synthesis](#pattern-synthesis)
- [Copyright and provenance](#copyright-and-provenance)
- [Quality states](#quality-states)
- [JSONL fields](#jsonl-fields)

## Purpose

Build a corpus that teaches agents to choose language from mathematical meaning.
Do not optimize for rare vocabulary or imitate a single author. Influential
papers and established textbooks are useful anchors, but reputation does not
guarantee clear, modern, or transferable prose.

## Influence gate

Use influential, field-recognized works as the anchor corpus. An anchor source
must satisfy at least one stable criterion:

- it establishes a landmark theorem, model, or method that shaped later work;
- it received an official test-of-time or comparable scholarly award;
- it is a canonical survey, guide, or reference published by a leading venue;
- it is a classic or durably adopted textbook published by a recognized
  scholarly press and documented through an official publisher, author, or
  institutional source;
- its method became standard practice in an authoritative research software or
  technical community.

For every anchor, record a concise `influence_basis` and an authoritative
`influence_source`. Prefer official award pages, publisher pages, author or
institutional publication records, and authoritative technical documentation.
Do not use a volatile citation count as the sole admission criterion. Citation
counts may support an audit when dated and sourced, but they do not replace a
stable account of the paper's field-level role.

Influence is an admission gate, not a style endorsement. Annotate only local
equation-prose events that are mathematically sound, intelligible in context,
and transferable without copying an author's idiosyncrasies. Use
`source_role: comparison` for a non-anchor source retained only to analyze a
boundary, counterexample, or historical contrast.

## Source genres

Record `source_genre` for every source. Supported values are
`research-article`, `textbook`, `research-monograph`, and `survey-or-guide`.
Genre is an evidence axis, not a domain layer or a Git branch.

Research articles show publication-state compression: they are the primary
evidence for how mature mathematical arguments are presented under space and
venue constraints. Textbooks show fuller expository behavior: how definitions
are motivated, constructions are staged, derivations are narrated, and proof
dependencies are made visible. Use textbook evidence to enrich these behaviors,
then compress pedagogical scaffolding when producing research prose.

Do not count several observations from one book as independent support. Pattern
validation still depends on distinct anchor sources and author groups. When a
pattern is intended for research manuscripts, prefer support from both article
and book genres when the behavior appears naturally in both; a textbook-only
pattern should remain explicit about its expository scope.

Use an official publisher page, an author or institutional copy, or a clearly
licensed edition. Record the actual access route and reuse terms. A freely
downloadable file is not necessarily openly licensed, and an official sample
chapter supports observations only from that sample.

## Core and domain layers

Treat Git `main` as the transferable mathematical-prose base. It contains
`references/corpora/math-core.jsonl` and only sources whose prose teaches a
general mathematical behavior across fields. Use these core disciplines:

- `pure-mathematics`;
- `applied-mathematics`;
- `probability-statistics`;
- `optimization-numerical-analysis`.

Create a domain branch only from a reviewed core baseline. Name it
`domain/<field>`. A domain branch retains the
entire core corpus and adds `references/corpora/<field>.jsonl`. Domain records
use `corpus_layer: domain` and a stable `domain` label. Core records use
`corpus_layer: core` and do not set `domain`.

Keep core corrections on `main`, then merge or rebase them into active domain
branches. Do not merge domain-only sources or patterns back into `main` merely
because they are useful in one specialty. A domain pattern may cite both core
and domain sources, but its validation evidence must include at least three
independent anchors from that domain. A core pattern must not depend on a
domain source.

Validate a core corpus alone:

```bash
python3 scripts/validate_corpus.py references/corpora/math-core.jsonl
```

Validate a domain overlay together with its inherited base:

```bash
python3 scripts/validate_corpus.py \
  references/corpora/math-core.jsonl \
  references/corpora/example-field.jsonl
```

Do not create the first domain branch until the mathematical base has at least
24 influential anchors, 60 reviewed observations, 24 distinct behavior codes,
all four core disciplines, and successful blind tests spanning definitions,
derivations, proof claims, and optimization or computation.

Enforce the machine-verifiable portion of this gate with:

```bash
python3 scripts/validate_corpus.py --require-core-ready \
  references/corpora/math-core.jsonl
```

Record the separate forward-writing checks in
[core-evaluation.md](core-evaluation.md). A count-based pass does not replace
these tests.

## Three record levels

Keep three types of record in one JSONL file or in three validated JSONL files.

### Source

Record one paper or book with stable bibliographic metadata, source genre,
field, venue, access route, and reuse status. A source record answers *where the
evidence came from*.

### Observation

Record one local equation-prose event. Annotate the mathematical behavior,
formula form, discourse position, section role, cue phrase, conditions, and a
paraphrased summary. An observation answers *what the author did in this local
context*.

### Pattern

Synthesize a reusable recommendation across observations. Record recommended
constructions, boundaries, counterexamples, a synthetic example, contributing
sources, and validation state. A pattern answers *what another writer may safely
reuse*.

Never promote a memorable sentence from one paper directly into a universal
rule.

## Annotation axes

Annotate each observation on independent axes.

### Primary mathematical behavior

Use one code from `behavior-taxonomy.md`, such as `A1` for naming, `B2` for
construction, `C2` for equivalent rewriting, `D2` for logical inference, or
`E3` for an update law. Use optional `secondary_behaviors` only when the same
event genuinely performs additional mathematical actions. Secondary behaviors
may support pattern evidence, but they do not replace the primary
classification.

### Discourse position

Use one of:

- `before-display`
- `display-lead`
- `where-clause`
- `after-display`
- `derivation-step`
- `result-statement`
- `proof-step`
- `paragraph-transition`

### Formula form

Use one of:

- `symbol-or-notation`
- `definition`
- `equality-or-identity`
- `membership-or-signature`
- `mapping-or-transform`
- `differential-equation`
- `recurrence-or-update`
- `optimization`
- `inequality-or-bound`
- `limit-or-asymptotic`
- `integral-or-sum`
- `piecewise-or-cases`
- `probability-or-expectation`
- `theorem-or-proposition`
- `algorithmic-relation`

### Claim strength

Use one of:

- `descriptive`
- `definitional`
- `exact-algebraic`
- `approximate`
- `one-way-consequence`
- `equivalence`
- `proved-result`
- `empirical-observation`

### Section role

Use one of:

- `notation-or-preliminaries`
- `problem-formulation`
- `method-or-model`
- `derivation-or-analysis`
- `theorem-or-proof`
- `algorithm`
- `experiment-or-results`
- `discussion-or-limitations`

Record physical placement separately with optional `section_location`.
Supported values are `main-text`, `appendix`, and `supplement`. An appendix may
contain a proof, derivation, algorithm, or limitation; it is a location rather
than a rhetorical role.

### Conditions

Use optional `conditions` for hypotheses, regimes, quantifier dependencies,
nonzero requirements, existence assumptions, or sampling limits that control
the observation. Keep them as short semantic statements rather than copied
source prose.

### Discipline

For core records, use one of the four broad labels and add an optional
`subfield`:

- `pure-mathematics`
- `applied-mathematics`
- `probability-statistics`
- `optimization-numerical-analysis`

Domain overlays may use a stable domain-specific discipline label, but must
also carry `corpus_layer: domain` and a non-empty `domain` field.

## Sampling strategy

Build a stratified corpus of influential anchors rather than a popularity list.

1. Run a 6--8 source pilot across the four core disciplines to test the
   schema, annotation boundaries, and validator before scaling collection.
2. Expand initially to 24--36 core anchor sources across all four core
   disciplines only after the pilot records can be annotated consistently.
   Treat 24 as the readiness floor; growth beyond 36 should fill documented
   behavior, formula-form, genre, or disciplinary gaps rather than increase
   volume alone.
3. Include multiple author groups, venues, decades, and section roles.
4. Admit anchor papers and books through the influence gate, then sample local
   passages for expository quality and behavioral coverage.
5. Prefer author-owned manuscripts, open-access versions, preprints, and sources
   with clear reuse terms for any stored text.
6. Oversample underrepresented behaviors rather than collecting more instances
   of easy notation definitions.
7. Keep theorem/proof prose separate from applied equation exposition during
   analysis, then identify patterns that genuinely transfer.
8. Track source genre explicitly. Use articles for publication-state phrasing
   and textbooks for explanatory structure; do not let textbook volume dominate
   the corpus merely because books contain more examples.

For a stable public pattern, target at least three independent sources and two
author groups. For a domain-general recommendation, seek evidence from at least
two disciplines. Treat these as review thresholds, not statistical proof.

## Extraction unit

Use an equation-centered event rather than an isolated sentence:

- one relevant formula or theorem statement;
- up to two preceding sentences;
- the local `where` or definition sentence;
- up to two following interpretation or inference sentences;
- section role and exact locator;
- the annotator's paraphrase of the mathematical action.

Keep full extraction units private unless their license permits redistribution.
The public corpus should normally retain metadata, brief conventional cue
phrases, structural annotations, and synthesized examples.

## Pattern synthesis

Summarize observations into a behavior card with:

1. behavior code and mathematical intent;
2. canonical grammatical frames;
3. typical equation position;
4. required preconditions;
5. claim-strength ceiling;
6. common misuses and near-synonym distinctions;
7. domain-specific variants;
8. a synthetic example whose mathematics is self-contained;
9. contributing source identifiers;
10. status: `seed`, `provisional`, or `validated`.

For a high-risk boundary, add a `boundary_cases` entry that maps an exact
string from `boundaries` to evidence:

1. `case_type`: `mathematical-counterexample`, `misuse`, or `near-synonym`;
2. `boundary`: an exact parent-boundary string;
3. `challenge`: the tempting but unsafe strengthening or confusion;
4. `safe_response`: the behavior the writer should preserve;
5. one or both evidence channels: `observation_ids` and `evaluation_ids`.

Every linked observation must share the pattern behavior, either as its primary
behavior or in `secondary_behaviors`, and its source must occur in the pattern's
`source_ids`. Boundary evidence makes a risky distinction auditable; it does
not require every routine boundary to carry a test case.

Report coverage counts by behavior, discipline, and section role. Do not rank a
construction only by frequency. A frequent phrase may be vague, field-specific,
or tied to one journal's style.

## Copyright and provenance

- Store full papers and books outside the public Skill repository unless
  redistribution is explicitly permitted.
- Prefer paraphrased observations and synthetic examples.
- Keep any verbatim quotation short, attributed, and necessary. Record its word
  count and reuse basis.
- Record DOI, stable URL, version, section, page or equation locator, access
  route, and license when known.
- Do not infer an open license merely because a PDF is freely accessible.
- Separate source-attested wording from the Skill's recommended wording.

## Quality states

- `seed`: expert-proposed behavior or construction not yet corpus-supported.
- `provisional`: supported by one or two independent anchor sources or still
  narrow in discipline and context.
- `validated`: supported by at least three independent anchor sources,
  reviewed for mathematical meaning, and checked against counterexamples.
  Domain-general patterns should also span at least two disciplines.

Validation means the annotation and synthesis passed review. It does not mean
the phrase is mandatory or universally optimal.

## JSONL fields

Use `scripts/validate_corpus.py` to check the following records.

### Source record

Required fields:

```json
{"id":"src-example","record_type":"source","corpus_layer":"core","source_role":"anchor","source_genre":"textbook","title":"...","year":2026,"discipline":"applied-mathematics","citation":"...","access":"author-hosted open edition","influence_basis":"Canonical field reference for ...","influence_source":"https://..."}
```

`source_genre` is one of `research-article`, `textbook`,
`research-monograph`, or `survey-or-guide`. Recommended optional fields include
`authors`, `venue`, `doi`, `url`, `version`, `license`, and `notes`.

`source_role` is either `anchor` or `comparison`. Anchor records must include
non-empty `influence_basis` and `influence_source` fields. Comparison records
may include them, but they are not treated as evidence that a pattern has been
tested against influential literature.

`corpus_layer` is either `core` or `domain`. Domain sources must also include a
non-empty `domain` field.

### Observation record

Required fields:

```json
{"id":"obs-example","record_type":"observation","corpus_layer":"core","source_id":"src-example","behavior":"C1","discourse_position":"derivation-step","formula_form":"equality-or-identity","claim_strength":"exact-algebraic","section_role":"derivation-or-analysis","locator":"Sec. 3, Eq. (7)","cue":"substituting ... gives","summary":"A paraphrase of the mathematical action.","quote_words":0}
```

Use optional `quote` only when redistribution is justified. The validator limits
it to 25 words but cannot decide whether reuse is legally permitted. Optional
fields include `secondary_behaviors`, `conditions`, and `section_location`.

### Pattern record

Required fields:

```json
{"id":"pat-example","record_type":"pattern","corpus_layer":"core","behavior":"C1","intent":"Introduce the result of a valid substitution.","constructions":["substituting ... into ... gives"],"boundaries":["Name the substituted relation and preserve its conditions."],"boundary_cases":[{"case_type":"misuse","boundary":"Name the substituted relation and preserve its conditions.","challenge":"A rewrite drops a nonzero condition required by the substituted identity.","safe_response":"Retain the condition and restrict the resulting formula to its valid domain.","observation_ids":["obs-example"],"evaluation_ids":["CF5-example"]}],"synthetic_example":"Substituting the stated relation into the governing equation gives the reduced form.","source_ids":["src-example"],"status":"provisional"}
```
