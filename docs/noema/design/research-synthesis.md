# Research synthesis and Proposals

Phase F keeps model output outside authoritative research state until a human
reviews it. Magent and Pi adapters submit the same typed Proposal
envelope; neither adapter can create a Finding, Research IR version, Problem
Model version, Task, or notebook cell directly.

## Emacs workflow

The former JuText/Magent command and Graph Board presentation are retired.
Typed Proposal services, Attention review and immutable evidence remain.
Use the actual Research API to submit a typed proposal and inspect its result;
ordinary Markdown writing uses the separate versioned document review API.
`noema-research-propose-memory-from-run` still proposes memory from an existing
Run's Handoff source. A proposal alone is not an accepted Finding.

A cell acceptance uses three durable states:

1. `pending`: no notebook write has happened;
2. `accepting`: the reviewed payload and reviewer are reserved before the
   notebook compare-and-swap write;
3. `accepted`: the reindexed cell identity, content, lineage, dependencies and
   source hash match the frozen reviewed payload.

An interrupted `accepting` Proposal remains in Attention as “Resume
acceptance”. Replaying the same reservation and reconciling an exact existing
cell are idempotent. A concurrent rejection cannot cross the reservation.

## Pi supervisor hook

`noema:api:research:supervisor:propose` is a narrow JSON ingress. It forces
`proposedBy=agent:pi:supervisor` and `sourceAdapter=pi-supervisor-hook`, rejects
review/decision fields, and calls only Proposal creation. It is a transport
hook, not a claim that a real Pi supervisor RPC exists. The locally installed
`pi-acp` remains an ordinary ACP execution adapter without claiming a provider-specific supervisor transport.

## Workstream export

`noema:api:research:export:create` creates a bounded,
content-addressed `application/vnd.noema.workstream+json` artifact. The package
contains notebooks, runtime records, Proposals, Findings, immutable Research IR
and Problem Model versions, Tasks, Jobs, immutable Invocations/results, Worker
profiles, Delegations, redacted Worker leases, events, and the referenced CAS
closure. Lease tokens are never exported.

The default `includeLocalOnly=false` export removes `local_only` cells,
Findings, Proposals and Tasks, follows Task dependency/descendant closure, and
omits Jobs, Invocations and Delegations underneath excluded Tasks. It also
omits IR/Problem Model versions that could retain filtered Finding references
and records every omission in the disclosure manifest.
Including local-only data must be explicit. The returned artifact can be read
through the existing artifact endpoint or `artifact(read)` MCP tool.

Unit and integration tests establish the state-machine, hash, disclosure and
round-trip contracts. They do not replace the large provenance/scale/model
benchmarks or real Pi/manual UI validation listed in the Phase F review.
