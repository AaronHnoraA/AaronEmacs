export type ExternalProseDiagnostic = {
  source: "languagetool";
  from: number;
  to: number;
  severity: "info" | "warning" | "error";
  message: string;
  rule?: string;
  word?: string;
  suggestions: string[];
};

export function parseLanguageToolDiagnostics(stdout: string, masked: string): ExternalProseDiagnostic[];
export function acceptProseWord(word: string): Promise<{ ok: boolean; word?: string; message?: string }>;
export function runExternalProseChecks(body?: {
  file?: string;
  content?: string;
  ranges?: Array<{ from: number; to: number }>;
  segments?: Array<{ from: number; text: string }>;
  totalChars?: number;
  allowLocalFallback?: boolean;
}): Promise<Record<string, unknown>>;
