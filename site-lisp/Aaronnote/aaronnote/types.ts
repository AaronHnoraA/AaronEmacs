export type NoteSummary = {
  key?: string;
  id?: string;
  title?: string;
  file?: string;
  link?: string;
  path?: string;
  ext?: string;
  kind?: string;
  date?: string;
  groupKey?: string;
  groupLabel?: string;
  section?: string;
  source?: string;
  aliases?: string[];
  summary?: string;
  tags?: string[];
  refs?: string[];
  backlinks?: string[];
};

export type SnippetSummary = {
  key?: string;
  name?: string;
  mode?: string;
  group?: string;
  body?: string;
  source?: string;
};

export type Inbound =
  | { type: "open"; file?: string; title?: string; content?: string; mode?: "markdown" | "source"; notes?: NoteSummary[]; snippets?: SnippetSummary[] }
  | { type: "saved"; ok?: boolean; message?: string; file?: string; notes?: NoteSummary[] }
  | { type: "notes"; notes?: NoteSummary[] }
  | { type: "snippets"; snippets?: SnippetSummary[] };
