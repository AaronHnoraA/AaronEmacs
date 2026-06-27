export type ExternalProseDiagnostic = {
  source: "vale" | "cspell";
  from: number;
  to: number;
  severity: "info" | "warning" | "error";
  message: string;
  rule?: string;
  word?: string;
  suggestions: string[];
};

export function parseValeDiagnostics(stdout: string, masked: string): ExternalProseDiagnostic[];
export function parseCspellDiagnostics(stdout: string, masked: string): ExternalProseDiagnostic[];
export function acceptProseWord(word: string): Promise<{ ok: boolean; word?: string; message?: string }>;
export function runExternalProseChecks(body?: {
  file?: string;
  content?: string;
  ranges?: Array<{ from: number; to: number }>;
  segments?: Array<{ from: number; text: string }>;
  totalChars?: number;
}): Promise<Record<string, unknown>>;
