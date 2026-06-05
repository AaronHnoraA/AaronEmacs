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
  inlineTags?: string[];
  refs?: string[];
  backlinks?: string[];
  roam?: boolean;
  bookRole?: "" | "cover" | "included";
  bookParentRef?: string;
  bookCoverId?: string;
  bookCoverPath?: string;
  bookParentPath?: string;
  bookIncludeRefs?: string[];
  bookIncludedPaths?: string[];
  bookToc?: Array<{ level?: number; text?: string; slug?: string; path?: string; id?: string }>;
  bookDomTargets?: Array<{ label?: string; slug?: string; path?: string; level?: number }>;
  domTargets?: Array<{ label?: string; slug?: string; path?: string[]; labelPath?: string[]; level?: number; notePath?: string }>;
  bookDiagnostics?: Array<{ level?: string; message?: string; path?: string }>;
  leanBlocks?: Array<{ tag?: string; selector?: string; targetKind?: string; leanPath?: string }>;
  standalone?: boolean;
  mtimeMs?: number;
  size?: number;
};

export type DirectorySummary = {
  path: string;
  label?: string;
  parent?: string;
  noteCount?: number;
  fileCount?: number;
  generated?: boolean;
};

export type FileSummary = {
  file: string;
  path: string;
  name?: string;
  ext?: string;
  type?: string;
  size?: number;
  mtimeMs?: number;
  groupKey?: string;
  groupLabel?: string;
  generated?: boolean;
};

export type SnippetSummary = {
  key?: string;
  name?: string;
  mode?: string;
  group?: string;
  kind?: string;
  body?: string;
  source?: string;
};

export type TemplateSummary = {
  key?: string;
  name?: string;
  mode?: string;
  kind?: string;
  group?: string;
  body?: string;
  source?: string;
};

export type UnusedAsset = {
  file: string;
  path: string;
  name: string;
  type: string;
  size: number;
  mtimeMs: number;
  isImage: boolean;
};

export type CursorPosition = {
  file: string;
  mode: "markdown" | "source";
  from: number;
  to: number;
  scrollY: number;
  updatedAt: number;
};

export type RecentNote = {
  file: string;
  openedAt: number;
};

export type UploadedAsset = {
  ok?: boolean;
  file?: string;
  name?: string;
  type?: string;
  isImage?: boolean;
  markdownPath?: string;
  message?: string;
};

export type GitChange = {
  path: string;
  file: string;
  gitPath?: string;
  oldPath?: string;
  status: string;
  staged?: boolean;
  unstaged?: boolean;
  tracked?: boolean;
  kind?: string;
  summary?: string;
  isMarkdown?: boolean;
};

export type GitCommitEntry = {
  sha: string;
  date: string;
  subject: string;
};

export type GitRepoStatus = {
  branch?: string;
  ahead?: number;
  behind?: number;
  uncommitted?: boolean;
  hasRemote?: boolean;
  remoteUrl?: string;
  message?: string;
};

export type Inbound =
  | { type: "open"; file?: string; title?: string; content?: string; kind?: string; mode?: "markdown" | "source"; standalone?: boolean; mtimeMs?: number; size?: number; notes?: NoteSummary[]; directories?: DirectorySummary[]; files?: FileSummary[]; snippets?: SnippetSummary[]; templates?: TemplateSummary[]; selection?: { from?: number; to?: number } }
  | { type: "saved"; ok?: boolean; message?: string; file?: string; kind?: string; standalone?: boolean; stale?: boolean; conflict?: boolean; mtimeMs?: number; size?: number; note?: NoteSummary; notes?: NoteSummary[]; directories?: DirectorySummary[]; files?: FileSummary[]; notesRefresh?: "full" | "deferred" }
  | { type: "notes"; notes?: NoteSummary[]; directories?: DirectorySummary[]; files?: FileSummary[] }
  | { type: "snippets"; snippets?: SnippetSummary[] }
  | { type: "templates"; templates?: TemplateSummary[] };
