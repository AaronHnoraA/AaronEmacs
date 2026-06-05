// Shared shapes for the Lean IPC boundary (`api.lean.*` responses) and the LSP
// payloads they carry. Declared once here so call sites in lean-placeholder.ts and
// lean-block.ts stop re-asserting the same `as { ... }` inline, and so a server-side
// shape change surfaces as a compile error instead of a silent runtime mismatch.

/** Common envelope for request-style responses. */
export type LeanOk = { ok?: boolean; message?: string };

/** Body span of a `@@lean4` region within its mirror file (source offsets). */
export type LeanRegionMeta = {
  bodyFrom: number;
  bodyTo: number;
};

/** `read-region` response. */
export type LeanRegionRead = LeanOk & {
  body?: string;
  text?: string;
  leanPath?: string;
  region?: LeanRegionMeta;
};

/** `open-region-file` response. */
export type LeanOpenRegionResponse = LeanOk & {
  lspVersion?: number;
  leanPath?: string;
  text?: string;
  body?: string;
  region?: LeanRegionMeta;
};

/** `update-region` response. */
export type LeanUpdateRegionResponse = LeanOk & {
  text?: string;
  region?: LeanRegionMeta;
  leanPath?: string;
  lspVersion?: number;
};

/** `getGoals` response (`$/lean/plainGoal`). */
export type LeanGoalsResponse = LeanOk & {
  result?: { rendered?: string; goals?: unknown[] } | null;
};

/** `getTermGoal` response (`$/lean/plainTermGoal`). */
export type LeanTermGoalResponse = LeanOk & {
  result?: { rendered?: string } | null;
};

/** `getHover` response (`textDocument/hover`). */
export type LeanHoverResponse = LeanOk & {
  result?: { contents?: string | { value?: string }; range?: unknown } | null;
};

/** `getCompletions` response (`textDocument/completion`); items stay opaque. */
export type LeanCompletionResponse = LeanOk & {
  result?: { items?: unknown[] } | unknown[] | null;
};

// --- LSP element shapes carried by the push notifications -------------------

export type LspPosition = { line?: number; character?: number };
export type LspRange = { start?: LspPosition; end?: LspPosition };

export type LspDiagnostic = {
  range?: LspRange;
  severity?: number;
  message?: string;
  leanTags?: unknown[];
};

export type LspFileProgressItem = {
  range?: LspRange;
  kind?: number;
};

// --- Push notification payloads (api.lean.on*) ------------------------------

export type LeanDiagnosticsPush = {
  uri?: string;
  version?: number;
  diagnostics?: LspDiagnostic[];
};

export type LeanProgressPush = {
  uri?: string;
  version?: number;
  processing?: LspFileProgressItem[];
};

export type LeanSemanticTokensPush = {
  uri?: string;
  legend?: unknown;
  data?: number[];
};
