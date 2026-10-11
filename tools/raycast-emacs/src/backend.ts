// The channel to Noema's backend.  Searching and maintenance go straight to
// the running host over loopback HTTP; Emacs is not asked and shows nothing.
// The host forwards a search to the Go kernel, which is the only searcher.
import { getPreferenceValues } from "@raycast/api";
import { readFile } from "node:fs/promises";
import { homedir } from "node:os";
import { join } from "node:path";

type HostDescriptor = { url: string; pid: number; noteRoot?: string };

function stateRoot(): string {
  const { noemaState } = getPreferenceValues<{ noemaState?: string }>();
  return (noemaState || "~/.config/emacs/var/aaronnote").replace(/^~/, homedir());
}

function alive(pid: number): boolean {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
}

/** Where the running host listens, or null when Noema is not running. */
export async function noemaHost(): Promise<HostDescriptor | null> {
  try {
    const descriptor = JSON.parse(await readFile(join(stateRoot(), "host.json"), "utf8")) as HostDescriptor;
    return descriptor.url && alive(Number(descriptor.pid)) ? descriptor : null;
  } catch {
    return null;
  }
}

export class NoemaDown extends Error {
  constructor() {
    super("Noema is not running");
  }
}

/** Call one host channel with ARGS and return its JSON answer. */
export async function noema<T>(channel: string, args: unknown[] = [], timeoutMs = 8000): Promise<T> {
  const host = await noemaHost();
  if (!host) throw new NoemaDown();
  const response = await fetch(`${host.url}/api`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ channel: `aaronnote:api:${channel}`, args }),
    signal: AbortSignal.timeout(timeoutMs),
  });
  const payload = (await response.json()) as T & { ok?: boolean; error?: string; message?: string };
  if (!response.ok || payload?.ok === false) {
    throw new Error(String(payload?.error || payload?.message || `Noema answered ${response.status}`));
  }
  return payload;
}

export type Page = {
  id: string;
  title: string;
  namespace: string;
  qualifiedTitle?: string;
  file: string;
  repositoryId: string;
  repositoryPath: string;
  excerpt?: string;
  tags?: string[];
  mtimeMs?: number;
  matchKind?: string;
  via?: string;
  identityStatus?: string;
};

export type SearchAnswer = {
  items: Page[];
  total: number;
  explain?: string[];
  corrected?: { from: string; to: string };
  create?: { title: string; namespace: string };
  source?: string;
};

/** Search pages as typed; nothing typed lists the most recent ones. */
export function searchPages(query: string, limit = 30): Promise<SearchAnswer> {
  const text = query.trim();
  return noema<SearchAnswer>("knowledge:search", [
    text ? { query, mode: "suggest", limit } : { query: "", sort: "recent", limit },
  ]);
}

/** The Wiki link that refers to PAGE, as the kernel writes it. */
export function pageLink(page: Page): string {
  return page.identityStatus === "provisional" || !page.id
    ? `[[${page.title}]]`
    : `[[roam://${page.id}|${page.title}]]`;
}
