import type { DirectorySummary, FileSummary, NoteSummary } from "./types.ts";
import { formatShortDateTime } from "./ui-format.ts";

declare global {
  interface Window {
    AaronnoteResolveAssetUrl?: (src: string) => string;
  }
}

type RecentNote = { file: string; openedAt: number };
type OpenNoteOptions = { newWindow?: boolean; equationTag?: string };
type RecentEntryItem = { entry: RecentNote; note: NoteSummary; file: string };
type RangerEntry =
  | { type: "dir"; path: string; label: string; count: number }
  | { type: "file"; note: NoteSummary; label: string; meta: string }
  | { type: "asset"; file: FileSummary; label: string; meta: string };
type SearchQuery = {
  terms: string[];
  notTerms: string[];
  tags: string[];
  notTags: string[];
  aliases: string[];
  notAliases: string[];
  paths: string[];
  notPaths: string[];
  titles: string[];
  notTitles: string[];
  groups: string[];
  notGroups: string[];
  sections: string[];
  notSections: string[];
  contents: string[];
  notContents: string[];
  refs: string[];
  notRefs: string[];
  backlinks: string[];
  notBacklinks: string[];
  kinds: string[];
  notKinds: string[];
  ids: string[];
  notIds: string[];
};
type SearchSuggestion = {
  field: string;
  label: string;
  detail: string;
  replacement: string;
};
type SearchSuggestionSources = Record<"tag" | "alias" | "path" | "title" | "group" | "section" | "ref" | "backlink" | "kind" | "id", string[]>;

export type FilesystemBrowser = {
  render: () => void;
  renderRecent: () => void;
  scheduleRender: () => void;
  collapseAll: () => void;
  expandAll: () => void;
  focus: () => boolean;
  focusRecent: () => boolean;
};

function fileNameFromPath(path: string): string {
  return path.split(/[\\/]/).filter(Boolean).at(-1) || "attachment";
}

function fileExtension(path: string): string {
  const name = fileNameFromPath(path).toLowerCase();
  const index = name.lastIndexOf(".");
  return index >= 0 ? name.slice(index + 1) : "";
}

function fileMediaUrl(file: FileSummary): string {
  const source = file.file || file.path || "";
  const url = new URL("aaronnote-asset://media/");
  url.searchParams.set("file", source);
  return window.AaronnoteResolveAssetUrl?.(url.toString()) ?? url.toString();
}

function filePreviewKind(file: FileSummary): "image" | "pdf" | "audio" | "video" | "text" | "html" | "" {
  const type = String(file.type || "").toLowerCase();
  const ext = String(file.ext || fileExtension(file.path || file.file || "")).toLowerCase();
  if (type.startsWith("image/") || ["avif", "bmp", "gif", "jpeg", "jpg", "png", "svg", "webp"].includes(ext)) return "image";
  if (type === "application/pdf" || ext === "pdf") return "pdf";
  if (type.startsWith("audio/") || ["mp3", "m4a", "ogg", "wav", "flac"].includes(ext)) return "audio";
  if (type.startsWith("video/") || ["mp4", "mov", "webm", "m4v"].includes(ext)) return "video";
  if (type === "text/html" || type.startsWith("text/html;") || ["html", "htm"].includes(ext)) return "html";
  if (type.startsWith("text/") || ["css", "csv", "json", "log", "md", "mjs", "js", "tex", "txt", "xml", "yaml", "yml"].includes(ext)) return "text";
  return "";
}

function renderFilePreviewMedia(file: FileSummary): HTMLElement | null {
  const kind = filePreviewKind(file);
  if (!kind) return null;
  const box = document.createElement("div");
  box.className = `aaronnote-ranger-media-preview aaronnote-ranger-media-preview-${kind}`;
  const src = fileMediaUrl(file);
  const label = file.name || fileNameFromPath(file.path || file.file || "");
  if (kind === "image") {
    const img = document.createElement("img");
    img.src = src;
    img.alt = label;
    img.loading = "lazy";
    img.decoding = "async";
    box.appendChild(img);
    return box;
  }
  if (kind === "audio") {
    const audio = document.createElement("audio");
    audio.src = src;
    audio.controls = true;
    audio.preload = "metadata";
    box.appendChild(audio);
    return box;
  }
  if (kind === "video") {
    const video = document.createElement("video");
    video.src = src;
    video.controls = true;
    video.preload = "metadata";
    box.appendChild(video);
    return box;
  }
  const frame = document.createElement("iframe");
  frame.src = src;
  frame.title = label;
  frame.loading = "lazy";
  if (kind === "html") frame.setAttribute("sandbox", "allow-scripts allow-forms allow-popups allow-downloads");
  box.appendChild(frame);
  return box;
}

function pathParts(path: string): string[] {
  return path.replace(/^\.\/?/, "").split(/[\\/]/).filter(Boolean);
}

function filesystemGroupFor(note: NoteSummary): string {
  return note.groupKey || (note.path || "").split(/[\\/]/).slice(0, -1).join("/") || "Root";
}

function groupLabel(group: string): string {
  if (!group || group === "Root") return "Root";
  return pathParts(group).at(-1) || group;
}

function groupParent(group: string): string {
  const parts = pathParts(group);
  if (parts.length <= 1) return "Root";
  return parts.slice(0, -1).join("/");
}

function normalizeDirectoryPath(dir: string): string {
  const parts = pathParts(dir);
  return parts.length ? parts.join("/") : "Root";
}

function isDirectChild(parent: string, child: string): boolean {
  if (child === "Root") return false;
  const parentParts = parent === "Root" ? [] : pathParts(parent);
  const childParts = pathParts(child);
  return childParts.length === parentParts.length + 1
    && parentParts.every((part, index) => childParts[index] === part);
}

function noteTitle(note: NoteSummary): string {
  return note.title || note.id || fileNameFromPath(note.file || note.path || "") || "Untitled";
}

function noteFileMeta(note: NoteSummary): string {
  if (note.tags?.length) return note.tags.slice(0, 4).map((tag) => `#${tag.replace(/^#/, "")}`).join(" ");
  return note.date || note.kind || note.section || note.source || (note.roam ? "roam" : "");
}

function normalizeSearchText(value: unknown): string {
  return String(value || "").toLowerCase();
}

function normalizeTag(value: unknown): string {
  return String(value || "").trim().replace(/^#/, "").toLowerCase();
}

function unquoteQueryValue(value: string): string {
  const text = String(value || "");
  if (text.length >= 2 && text.startsWith("\"") && text.endsWith("\"")) {
    return text.slice(1, -1).replace(/\\"/g, "\"").replace(/\\\\/g, "\\");
  }
  return text;
}

function tokenizeQuery(text: string): string[] {
  const tokens: string[] = [];
  const pattern = /(#"(?:\\.|[^"\\])*")|([a-zA-Z]+:"(?:\\.|[^"\\])*")|("(?:\\.|[^"\\])*")|(\S+)/g;
  let match: RegExpExecArray | null;
  while ((match = pattern.exec(String(text || "")))) {
    if (match[1] !== undefined) tokens.push(match[1]);
    else if (match[2] !== undefined) tokens.push(match[2]);
    else if (match[3] !== undefined) tokens.push(unquoteQueryValue(match[3]));
    else if (match[4] !== undefined) tokens.push(match[4]);
  }
  return tokens.map((token) => token.trim()).filter(Boolean);
}

function uniqueSearchValues(values: string[]): string[] {
  return [...new Set(values.filter(Boolean))];
}

function uniqueSorted(values: string[]): string[] {
  return [...new Set(values.map((value) => String(value || "").trim()).filter(Boolean))]
    .sort((a, b) => a.localeCompare(b));
}

function quoteSearchValue(value: string): string {
  const text = String(value || "");
  if (/[\s"]/u.test(text)) {
    return `"${text.replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"`;
  }
  return text;
}

function parseSearchQuery(text: string): SearchQuery {
  const parsed: SearchQuery = {
    terms: [],
    notTerms: [],
    tags: [],
    notTags: [],
    aliases: [],
    notAliases: [],
    paths: [],
    notPaths: [],
    titles: [],
    notTitles: [],
    groups: [],
    notGroups: [],
    sections: [],
    notSections: [],
    contents: [],
    notContents: [],
    refs: [],
    notRefs: [],
    backlinks: [],
    notBacklinks: [],
    kinds: [],
    notKinds: [],
    ids: [],
    notIds: [],
  };
  for (const token of tokenizeQuery(text)) {
    const negated = token.startsWith("-") && token.length > 1;
    const rawToken = negated ? token.slice(1) : token;
    if (rawToken.startsWith("#") && rawToken.length > 1) {
      const tag = normalizeTag(unquoteQueryValue(rawToken.slice(1)));
      if (tag) (negated ? parsed.notTags : parsed.tags).push(tag);
      continue;
    }

    const fieldMatch = rawToken.match(/^([a-zA-Z]+):(.*)$/);
    if (!fieldMatch) {
      (negated ? parsed.notTerms : parsed.terms).push(normalizeSearchText(rawToken));
      continue;
    }

    const field = fieldMatch[1]!.toLowerCase();
    const value = normalizeSearchText(unquoteQueryValue(fieldMatch[2] || ""));
    if (!value) continue;

    if (field === "tag" || field === "tags") (negated ? parsed.notTags : parsed.tags).push(normalizeTag(value));
    else if (field === "alias" || field === "aliases" || field === "aka") (negated ? parsed.notAliases : parsed.aliases).push(value);
    else if (field === "path" || field === "file") (negated ? parsed.notPaths : parsed.paths).push(value);
    else if (field === "title") (negated ? parsed.notTitles : parsed.titles).push(value);
    else if (field === "group" || field === "folder") (negated ? parsed.notGroups : parsed.groups).push(value);
    else if (field === "section") (negated ? parsed.notSections : parsed.sections).push(value);
    else if (field === "content" || field === "body" || field === "text") (negated ? parsed.notContents : parsed.contents).push(value);
    else if (field === "ref" || field === "refs" || field === "out") (negated ? parsed.notRefs : parsed.refs).push(value);
    else if (field === "backlink" || field === "backlinks" || field === "in") (negated ? parsed.notBacklinks : parsed.backlinks).push(value);
    else if (field === "kind" || field === "type") (negated ? parsed.notKinds : parsed.kinds).push(value);
    else if (field === "id" || field === "key") (negated ? parsed.notIds : parsed.ids).push(value);
    else (negated ? parsed.notTerms : parsed.terms).push(normalizeSearchText(rawToken));
  }

  for (const key of Object.keys(parsed) as Array<keyof SearchQuery>) {
    parsed[key] = uniqueSearchValues(parsed[key]);
  }
  return parsed;
}

function includesAll(haystack: string, needles: readonly string[]): boolean {
  return needles.length === 0 || needles.every((needle) => haystack.includes(needle));
}

function listMatchesAll(values: readonly string[] | undefined, needles: readonly string[]): boolean {
  const normalized = (values ?? []).map(normalizeSearchText);
  return needles.length === 0 || needles.every((needle) =>
    normalized.some((value) => value === needle || value.includes(needle)));
}

function listMatchesAny(values: readonly string[] | undefined, needles: readonly string[]): boolean {
  const normalized = (values ?? []).map(normalizeSearchText);
  return needles.length > 0 && needles.some((needle) =>
    normalized.some((value) => value === needle || value.includes(needle)));
}

function noteSearchText(note: NoteSummary, cache: WeakMap<NoteSummary, string>): string {
  const cached = cache.get(note);
  if (cached !== undefined) return cached;
  const extra = note as NoteSummary & { searchText?: string; content?: string };
  const text = [
    note.title,
    note.id,
    note.key,
    note.file,
    note.path,
    note.summary,
    extra.searchText,
    extra.content,
    note.ext,
    note.kind,
    note.date,
    note.groupKey,
    note.groupLabel,
    note.section,
    note.source,
    ...(note.aliases ?? []),
    ...(note.tags ?? []),
  ].filter((item): item is string => Boolean(item)).join(" ").toLowerCase();
  cache.set(note, text);
  return text;
}

function noteMatchesSearch(note: NoteSummary, query: SearchQuery, cache: WeakMap<NoteSummary, string>): boolean {
  const haystack = noteSearchText(note, cache);
  const matchesText = includesAll(haystack, query.terms) && !query.notTerms.some((term) => haystack.includes(term));
  const matchesTags = query.tags.length === 0
    || query.tags.every((tag) => (note.tags ?? []).map(normalizeTag).includes(tag));
  const excludesTags = query.notTags.some((tag) => (note.tags ?? []).map(normalizeTag).includes(tag));
  const matchesAliases = listMatchesAll(note.aliases, query.aliases);
  const excludesAliases = listMatchesAny(note.aliases, query.notAliases);
  const matchesPath = includesAll(normalizeSearchText(`${note.path || ""} ${note.file || ""}`), query.paths);
  const excludesPath = query.notPaths.some((path) => normalizeSearchText(`${note.path || ""} ${note.file || ""}`).includes(path));
  const matchesTitle = includesAll(normalizeSearchText(noteTitle(note)), query.titles);
  const excludesTitle = query.notTitles.some((title) => normalizeSearchText(noteTitle(note)).includes(title));
  const matchesGroup = includesAll(normalizeSearchText(`${note.groupKey || ""} ${note.groupLabel || ""} ${filesystemGroupFor(note)}`), query.groups);
  const excludesGroup = query.notGroups.some((group) => normalizeSearchText(`${note.groupKey || ""} ${note.groupLabel || ""} ${filesystemGroupFor(note)}`).includes(group));
  const matchesSection = includesAll(normalizeSearchText(note.section), query.sections);
  const excludesSection = query.notSections.some((section) => normalizeSearchText(note.section).includes(section));
  const contentText = normalizeSearchText(`${(note as NoteSummary & { searchText?: string }).searchText || ""} ${note.summary || ""}`);
  const matchesContent = includesAll(contentText, query.contents);
  const excludesContent = query.notContents.some((content) => contentText.includes(content));
  const refText = normalizeSearchText((note.refs ?? []).join(" "));
  const backlinkText = normalizeSearchText((note.backlinks ?? []).join(" "));
  const matchesRefs = includesAll(refText, query.refs);
  const excludesRefs = query.notRefs.some((ref) => refText.includes(ref));
  const matchesBacklinks = includesAll(backlinkText, query.backlinks);
  const excludesBacklinks = query.notBacklinks.some((backlink) => backlinkText.includes(backlink));
  const kindText = normalizeSearchText(`${note.kind || ""} ${note.ext || ""} ${note.roam ? "roam" : ""}`);
  const matchesKind = includesAll(kindText, query.kinds);
  const excludesKind = query.notKinds.some((kind) => kindText.includes(kind));
  const idText = normalizeSearchText(`${note.id || ""} ${note.key || ""}`);
  const matchesId = includesAll(idText, query.ids);
  const excludesId = query.notIds.some((id) => idText.includes(id));
  return matchesText
    && matchesTags
    && !excludesTags
    && matchesAliases
    && !excludesAliases
    && matchesPath
    && !excludesPath
    && matchesTitle
    && !excludesTitle
    && matchesGroup
    && !excludesGroup
    && matchesSection
    && !excludesSection
    && matchesContent
    && !excludesContent
    && matchesRefs
    && !excludesRefs
    && matchesBacklinks
    && !excludesBacklinks
    && matchesKind
    && !excludesKind
    && matchesId
    && !excludesId;
}

function sortedNotes(items: NoteSummary[]): NoteSummary[] {
  return [...items].sort((a, b) =>
    noteTitle(a).localeCompare(noteTitle(b)) || String(a.file || "").localeCompare(String(b.file || "")));
}

function sortedFiles(items: FileSummary[]): FileSummary[] {
  return [...items].sort((a, b) =>
    String(a.name || fileNameFromPath(a.path || a.file || "")).localeCompare(String(b.name || fileNameFromPath(b.path || b.file || "")))
    || String(a.path || a.file || "").localeCompare(String(b.path || b.file || "")));
}

function groupsFromNotes(notes: NoteSummary[]): Map<string, NoteSummary[]> {
  const groups = new Map<string, NoteSummary[]>();
  for (const note of notes) {
    const group = filesystemGroupFor(note);
    if (!groups.has(group)) groups.set(group, []);
    groups.get(group)!.push(note);
  }
  return groups;
}

function filesFromSummaries(files: FileSummary[], showAll: boolean): Map<string, FileSummary[]> {
  const groups = new Map<string, FileSummary[]>();
  if (!showAll) return groups;
  for (const file of files) {
    const group = file.groupKey || (file.path || "").split(/[\\/]/).slice(0, -1).join("/") || "Root";
    if (!groups.has(group)) groups.set(group, []);
    groups.get(group)!.push(file);
  }
  return groups;
}

function visibleDirectory(dir: DirectorySummary, showAll: boolean): boolean {
  if (showAll) return true;
  return dir.path === "Root" || !dir.generated || Number(dir.noteCount || 0) > 0;
}

function allDirectoryGroups(groups: Map<string, NoteSummary[]>, extraDirs: Iterable<string> = []): string[] {
  const dirs = new Set<string>(["Root"]);
  for (const group of groups.keys()) {
    const parts = pathParts(group);
    for (let i = 1; i <= parts.length; i++) dirs.add(parts.slice(0, i).join("/"));
  }
  for (const dir of extraDirs) {
    const parts = pathParts(dir);
    for (let i = 1; i <= parts.length; i++) dirs.add(parts.slice(0, i).join("/"));
  }
  return [...dirs].sort((a, b) => {
    if (a === "Root") return -1;
    if (b === "Root") return 1;
    return a.localeCompare(b);
  });
}

function directoryMapFromSummaries(
  directories: DirectorySummary[],
  groups: Map<string, NoteSummary[]>,
  showAll: boolean,
  extraDirs: Iterable<string> = [],
): Map<string, DirectorySummary> {
  const map = new Map<string, DirectorySummary>();
  for (const dir of directories) {
    const path = normalizeDirectoryPath(dir.path || "Root");
    const summary = { ...dir, path, label: dir.label || groupLabel(path) };
    if (visibleDirectory(summary, showAll)) map.set(path, summary);
  }
  for (const group of allDirectoryGroups(groups, extraDirs)) {
    if (!map.has(group)) map.set(group, { path: group, label: groupLabel(group), noteCount: directoryCount(group, groups) });
  }
  if (!map.has("Root")) map.set("Root", { path: "Root", label: "Root", noteCount: directoryCount("Root", groups) });
  return map;
}

function directoryCount(group: string, groups: Map<string, NoteSummary[]>): number {
  if (group === "Root") return [...groups.values()].reduce((total, items) => total + items.length, 0);
  const prefix = `${group}/`;
  return [...groups.entries()]
    .filter(([dir]) => dir === group || dir.startsWith(prefix))
    .reduce((total, [, items]) => total + items.length, 0);
}

export function createFilesystemBrowser(options: {
  noteList: HTMLElement;
  recentList: HTMLElement;
  noteFilter: HTMLInputElement;
  noteCount: HTMLElement;
  managementCount: HTMLElement;
  getNotes: () => NoteSummary[];
  getDirectories?: () => DirectorySummary[];
  getFiles?: () => FileSummary[];
  getRecentNotes: () => RecentNote[];
  getCurrentFile: () => string;
  getShowAllFiles?: () => boolean;
  toggleShowAllFiles?: () => void;
  openNote: (note: NoteSummary, options?: OpenNoteOptions) => void;
  deleteNote?: (note: NoteSummary) => void;
  deleteFile?: (file: FileSummary) => Promise<void>;
  createNode?: (dir?: string, options?: { stayInFilesystem?: boolean }) => void;
  createFolder?: (dir: string) => Promise<string | null | undefined>;
  renameNote?: (note: NoteSummary) => Promise<void>;
  renameFile?: (file: FileSummary) => Promise<void>;
  renameDirectory?: (dir: string) => Promise<void>;
  moveNote?: (note: NoteSummary) => Promise<void>;
  moveFile?: (file: FileSummary) => Promise<void>;
  moveDirectory?: (dir: string) => Promise<void>;
  duplicateNote?: (note: NoteSummary) => Promise<void>;
  duplicateFile?: (file: FileSummary) => Promise<void>;
  trashDirectory?: (dir: string) => Promise<void>;
  revealPath?: (path: string) => Promise<void>;
  openDirectory?: (path: string) => Promise<void>;
  openDirectoryInKitty?: (path: string) => Promise<void>;
}): FilesystemBrowser {
  const searchRenderLimit = 240;
  const browseRenderLimit = 900;
  const searchCache = new WeakMap<NoteSummary, string>();
  let renderFrame = 0;
  let currentDir = "";
  let selectedFile = "";
  let selectedEntryId = "";
  let parentEntryId = "";
  let activePane: "parent" | "current" = "current";
  let lastFileClick = { id: "", at: 0 };
  let lastRecentRenderKey = "";
  let lastRenderKey = "";
  let selectedRecentFile = "";
  let focusRecentAfterRender = false;
  let lastNotesSignatureItems: NoteSummary[] | null = null;
  let lastNotesSignature = "";
  let lastDirectoriesSignatureItems: DirectorySummary[] | null = null;
  let lastDirectoriesSignature = "";
  let lastFilesSignatureItems: FileSummary[] | null = null;
  let lastFilesSignature = "";
  let helpVisible = false;
  let focusAfterRender = false;
  let searchSuggestionBox: HTMLElement | null = null;
  let searchSuggestions: SearchSuggestion[] = [];
  let searchActiveSuggestionIndex = -1;
  let searchSourceNotes: NoteSummary[] | null = null;
  let searchSourceCache: SearchSuggestionSources | null = null;
  const extraDirs = new Set<string>();

  function searchSuggestionSources(notes: NoteSummary[]): SearchSuggestionSources {
    if (notes === searchSourceNotes && searchSourceCache) return searchSourceCache;
    searchSourceNotes = notes;
    searchSourceCache = {
      tag: uniqueSorted(notes.flatMap((note) => note.tags ?? [])),
      alias: uniqueSorted(notes.flatMap((note) => note.aliases ?? [])),
      path: uniqueSorted(notes.flatMap((note) => [note.path, note.file].filter(Boolean) as string[])),
      title: uniqueSorted(notes.map((note) => noteTitle(note))),
      group: uniqueSorted(notes.flatMap((note) => [note.groupKey, note.groupLabel, filesystemGroupFor(note)].filter(Boolean) as string[])),
      section: uniqueSorted(notes.map((note) => note.section || "").filter(Boolean)),
      ref: uniqueSorted(notes.flatMap((note) => note.refs ?? [])),
      backlink: uniqueSorted(notes.flatMap((note) => note.backlinks ?? [])),
      kind: uniqueSorted(notes.flatMap((note) => [note.kind, note.ext, note.roam ? "roam" : ""].filter(Boolean) as string[])),
      id: uniqueSorted(notes.flatMap((note) => [note.id, note.key].filter(Boolean) as string[])),
    };
    return searchSourceCache;
  }

  function searchSuggestionField(token: string): { field: keyof SearchSuggestionSources; query: string; sigil: string; negated: boolean } | null {
    const raw = String(token || "");
    const negated = raw.startsWith("-") && raw.length > 1;
    const body = negated ? raw.slice(1) : raw;
    const lower = body.toLowerCase();
    if (lower.startsWith("#")) {
      return { field: "tag", query: lower.slice(1), sigil: "#", negated };
    }
    const match = lower.match(/^([a-z]+):(.*)$/);
    if (!match) return null;
    const aliases: Record<string, keyof SearchSuggestionSources> = {
      tags: "tag",
      aka: "alias",
      aliases: "alias",
      file: "path",
      folder: "group",
      out: "ref",
      refs: "ref",
      in: "backlink",
      backlinks: "backlink",
      type: "kind",
      key: "id",
    };
    const field = aliases[match[1]!] || match[1]!;
    if (!["tag", "alias", "path", "title", "group", "section", "ref", "backlink", "kind", "id"].includes(field)) return null;
    return { field: field as keyof SearchSuggestionSources, query: match[2] || "", sigil: "", negated };
  }

  function currentSearchToken(): { value: string; start: number; end: number; token: string } {
    const value = options.noteFilter.value;
    const cursor = options.noteFilter.selectionStart ?? value.length;
    let start = cursor;
    let end = cursor;
    while (start > 0 && !/\s/u.test(value[start - 1]!)) start -= 1;
    while (end < value.length && !/\s/u.test(value[end]!)) end += 1;
    return { value, start, end, token: value.slice(start, end) };
  }

  function replacementForSuggestion(field: string, value: string, sigil = "", negated = false): string {
    const prefix = negated ? "-" : "";
    if (sigil) return `${prefix}${sigil}${quoteSearchValue(value)}`;
    return `${prefix}${field}:${quoteSearchValue(value)}`;
  }

  function buildSearchSuggestions(): SearchSuggestion[] {
    const tokenInfo = currentSearchToken();
    const token = tokenInfo.token;
    const negated = token.startsWith("-") && token.length > 1;
    const visibleToken = negated ? token.slice(1) : token;
    const lowerToken = visibleToken.toLowerCase();
    const sources = searchSuggestionSources(options.getNotes());
    const field = searchSuggestionField(token);
    const fieldLabels: Array<{ field: keyof SearchSuggestionSources; label: string; detail: string }> = [
      { field: "tag", label: "tag:", detail: "filter by tag" },
      { field: "alias", label: "alias:", detail: "filter by alias" },
      { field: "path", label: "path:", detail: "filter by path" },
      { field: "title", label: "title:", detail: "filter by title" },
      { field: "group", label: "group:", detail: "filter by folder/group" },
      { field: "section", label: "section:", detail: "filter by section" },
      { field: "ref", label: "ref:", detail: "outgoing refs" },
      { field: "backlink", label: "backlink:", detail: "incoming refs" },
      { field: "kind", label: "kind:", detail: "note kind/type" },
      { field: "id", label: "id:", detail: "roam id/key" },
    ];

    if (field) {
      return (sources[field.field] || [])
        .filter((value) => value.toLowerCase().includes(field.query))
        .slice(0, 12)
        .map((value) => ({
          field: field.field,
          label: field.sigil ? `${field.negated ? "-" : ""}${field.sigil}${value}` : `${field.negated ? "-" : ""}${field.field}:${value}`,
          detail: field.negated ? `exclude ${field.field}` : field.field,
          replacement: replacementForSuggestion(field.field, value, field.sigil, field.negated),
        }));
    }

    const fieldSuggestions = fieldLabels
      .filter((item) => item.label.startsWith(lowerToken))
      .map((item) => ({
        field: item.field,
        label: `${negated ? "-" : ""}${item.label}`,
        detail: negated ? `exclude ${item.detail.replace(/^filter by /, "")}` : item.detail,
        replacement: `${negated ? "-" : ""}${item.label}`,
      }));

    if (!lowerToken) return fieldSuggestions.slice(0, 12);

    const valueSuggestions: SearchSuggestion[] = [
      ...sources.tag.filter((value) => value.toLowerCase().includes(lowerToken)).slice(0, 4).map((value) => ({
        field: "tag",
        label: `${negated ? "-" : ""}tag:${value}`,
        detail: negated ? "exclude tag" : "tag",
        replacement: replacementForSuggestion("tag", value, "", negated),
      })),
      ...sources.alias.filter((value) => value.toLowerCase().includes(lowerToken)).slice(0, 3).map((value) => ({
        field: "alias",
        label: `${negated ? "-" : ""}alias:${value}`,
        detail: negated ? "exclude alias" : "alias",
        replacement: replacementForSuggestion("alias", value, "", negated),
      })),
      ...sources.path.filter((value) => value.toLowerCase().includes(lowerToken)).slice(0, 3).map((value) => ({
        field: "path",
        label: `${negated ? "-" : ""}path:${value}`,
        detail: negated ? "exclude path" : "path",
        replacement: replacementForSuggestion("path", value, "", negated),
      })),
      ...sources.title.filter((value) => value.toLowerCase().includes(lowerToken)).slice(0, 3).map((value) => ({
        field: "title",
        label: `${negated ? "-" : ""}title:${value}`,
        detail: negated ? "exclude title" : "title",
        replacement: replacementForSuggestion("title", value, "", negated),
      })),
    ];
    return [...fieldSuggestions, ...valueSuggestions].slice(0, 12);
  }

  function ensureSearchSuggestionBox(): HTMLElement {
    if (searchSuggestionBox) return searchSuggestionBox;
    searchSuggestionBox = document.createElement("div");
    searchSuggestionBox.className = "aaronnote-ranger-search-suggestions";
    searchSuggestionBox.hidden = true;
    searchSuggestionBox.setAttribute("role", "listbox");
    document.body.appendChild(searchSuggestionBox);
    return searchSuggestionBox;
  }

  function closeSearchSuggestions(): void {
    searchSuggestions = [];
    searchActiveSuggestionIndex = -1;
    if (searchSuggestionBox) searchSuggestionBox.replaceChildren();
    if (searchSuggestionBox) searchSuggestionBox.hidden = true;
    options.noteFilter.setAttribute("aria-expanded", "false");
  }

  function placeSearchSuggestions(): void {
    const box = ensureSearchSuggestionBox();
    const rect = options.noteFilter.getBoundingClientRect();
    const margin = 8;
    const width = Math.min(Math.max(320, rect.width), window.innerWidth - margin * 2);
    box.style.left = `${Math.min(Math.max(margin, rect.left), Math.max(margin, window.innerWidth - width - margin))}px`;
    box.style.top = `${Math.min(rect.bottom + 4, window.innerHeight - margin)}px`;
    box.style.width = `${width}px`;
  }

  function setActiveSearchSuggestion(index: number): void {
    const box = ensureSearchSuggestionBox();
    if (searchSuggestions.length === 0) {
      searchActiveSuggestionIndex = -1;
      return;
    }
    searchActiveSuggestionIndex = (index + searchSuggestions.length) % searchSuggestions.length;
    box.querySelectorAll(".aaronnote-ranger-search-suggestion").forEach((button, buttonIndex) => {
      const active = buttonIndex === searchActiveSuggestionIndex;
      button.classList.toggle("is-active", active);
      button.setAttribute("aria-selected", active ? "true" : "false");
    });
  }

  function applySearchSuggestion(suggestion: SearchSuggestion | undefined): void {
    if (!suggestion) return;
    const tokenInfo = currentSearchToken();
    const before = tokenInfo.value.slice(0, tokenInfo.start);
    const after = tokenInfo.value.slice(tokenInfo.end).replace(/^\s+/u, "");
    const trailing = suggestion.replacement.endsWith(":") ? "" : " ";
    const nextValue = `${before}${suggestion.replacement}${trailing}${after}`;
    const nextCursor = before.length + suggestion.replacement.length + trailing.length;
    options.noteFilter.value = nextValue;
    options.noteFilter.focus();
    options.noteFilter.setSelectionRange(nextCursor, nextCursor);
    closeSearchSuggestions();
    scheduleRender();
  }

  function showSearchSuggestions(): void {
    if (document.activeElement !== options.noteFilter) return;
    const box = ensureSearchSuggestionBox();
    searchSuggestions = buildSearchSuggestions();
    searchActiveSuggestionIndex = -1;
    if (searchSuggestions.length === 0) {
      closeSearchSuggestions();
      return;
    }
    const frag = document.createDocumentFragment();
    searchSuggestions.forEach((suggestion, index) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "aaronnote-ranger-search-suggestion";
      button.dataset.index = String(index);
      button.setAttribute("role", "option");
      button.setAttribute("aria-selected", "false");
      const label = document.createElement("span");
      label.textContent = suggestion.label;
      const detail = document.createElement("small");
      detail.textContent = suggestion.detail;
      button.append(label, detail);
      button.addEventListener("mousedown", (event) => event.preventDefault());
      button.addEventListener("click", () => applySearchSuggestion(searchSuggestions[index]));
      button.addEventListener("mouseenter", () => setActiveSearchSuggestion(index));
      frag.appendChild(button);
    });
    box.replaceChildren(frag);
    placeSearchSuggestions();
    box.hidden = false;
    options.noteFilter.setAttribute("aria-expanded", "true");
  }

  function notesSignature(items: NoteSummary[]): string {
    if (items === lastNotesSignatureItems) return lastNotesSignature;
    lastNotesSignatureItems = items;
    lastNotesSignature = items
      .map((note) => `${note.file}:${note.title}:${note.mtimeMs ?? ""}:${note.size ?? ""}`)
      .join("\u0001");
    return lastNotesSignature;
  }

  function directoriesSignature(items: DirectorySummary[]): string {
    if (items === lastDirectoriesSignatureItems) return lastDirectoriesSignature;
    lastDirectoriesSignatureItems = items;
    lastDirectoriesSignature = items
      .map((dir) => `${dir.path}:${dir.noteCount ?? ""}:${dir.fileCount ?? ""}:${dir.generated ? 1 : 0}`)
      .join("\u0001");
    return lastDirectoriesSignature;
  }

  function filesSignature(items: FileSummary[]): string {
    if (items === lastFilesSignatureItems) return lastFilesSignature;
    lastFilesSignatureItems = items;
    lastFilesSignature = items
      .map((file) => `${file.path}:${file.mtimeMs ?? ""}:${file.size ?? ""}:${file.generated ? 1 : 0}`)
      .join("\u0001");
    return lastFilesSignature;
  }

  function requestRangerFocus(): void {
    focusAfterRender = true;
  }

  function forgetExtraDirectory(dir: string): void {
    const target = normalizeDirectoryPath(dir);
    const prefix = `${target}/`;
    for (const item of [...extraDirs]) {
      const normalized = normalizeDirectoryPath(item);
      if (normalized === target || normalized.startsWith(prefix)) extraDirs.delete(item);
    }
  }

  function currentFilesystemGroup(notes: NoteSummary[]): string {
    const note = notes.find((item) => item.file === options.getCurrentFile());
    return note ? filesystemGroupFor(note) : "";
  }

  function renderNoteButton(note: NoteSummary, detail: string, extra?: string): HTMLButtonElement {
    const button = document.createElement("button");
    button.type = "button";
    button.className = "aaronnote-note";
    if (note.file && note.file === options.getCurrentFile()) button.classList.add("is-active");
    const title = document.createElement("strong");
    title.textContent = noteTitle(note);
    const detailEl = document.createElement("span");
    detailEl.textContent = detail;
    button.append(title, detailEl);
    if (extra) {
      const extraEl = document.createElement("span");
      extraEl.className = "aaronnote-note-extra";
      extraEl.textContent = extra;
      button.appendChild(extraEl);
    }
    button.title = note.file || "";
    button.addEventListener("click", (event) => options.openNote(note, { newWindow: event.altKey || event.metaKey }));
    button.addEventListener("auxclick", (event) => {
      if (event.button !== 1) return;
      event.preventDefault();
      options.openNote(note, { newWindow: true });
    });
    return button;
  }

  function shouldOpenFileClick(entry: RangerEntry, event: MouseEvent): boolean {
    if (entry.type !== "file") return false;
    if (event.detail >= 2 || event.altKey || event.metaKey) return true;
    const id = entryId(entry);
    const now = Date.now();
    const repeated = lastFileClick.id === id && now - lastFileClick.at < 700;
    lastFileClick = { id, at: now };
    return repeated;
  }

  function directoryEntries(
    dir: string,
    groups: Map<string, NoteSummary[]>,
    directories: Map<string, DirectorySummary>,
    files: Map<string, FileSummary[]>,
  ): RangerEntry[] {
    const dirs = [...directories.keys()]
      .filter((group) => isDirectChild(dir, group))
      .map((group) => {
        const summary = directories.get(group);
        return {
          type: "dir" as const,
          path: group,
          label: summary?.label || groupLabel(group),
          count: summary?.noteCount || summary?.fileCount || directoryCount(group, groups),
        };
      });
    const noteEntries = sortedNotes(groups.get(dir) ?? []).map((note) => ({
      type: "file" as const,
      note,
      label: noteTitle(note),
      meta: noteFileMeta(note) || fileNameFromPath(note.file || note.path || ""),
    }));
    const assetEntries = sortedFiles(files.get(dir) ?? []).map((file) => ({
      type: "asset" as const,
      file,
      label: file.name || fileNameFromPath(file.path || file.file || ""),
      meta: file.ext ? `.${file.ext}` : file.type || "file",
    }));
    return [...dirs, ...noteEntries, ...assetEntries];
  }

  function setCurrentSelection(entry: RangerEntry | null | undefined): void {
    selectedEntryId = entry ? entryId(entry) : "";
    selectedFile = entry?.type === "file" ? entry.note.file || "" : "";
  }

  function selectDirectory(
    dir: string,
    groups: Map<string, NoteSummary[]>,
    directories: Map<string, DirectorySummary>,
    files: Map<string, FileSummary[]>,
    preferredEntryId = "",
  ): void {
    currentDir = dir || "Root";
    const entries = directoryEntries(currentDir, groups, directories, files);
    const next = (preferredEntryId ? entries.find((entry) => entryId(entry) === preferredEntryId) : null)
      ?? entries.find((entry) => entry.type === "file" && entry.note.file === selectedFile)
      ?? entries[0];
    setCurrentSelection(next);
  }

  function ensureSelection(
    groups: Map<string, NoteSummary[]>,
    directories: Map<string, DirectorySummary>,
    files: Map<string, FileSummary[]>,
    shown: NoteSummary[],
    query: string,
  ): void {
    const current = shown.find((note) => note.file === options.getCurrentFile());
    if (!currentDir) currentDir = current ? filesystemGroupFor(current) : "Root";
    if (query) {
      if (!selectedFile || !shown.some((note) => note.file === selectedFile)) {
        selectedFile = current?.file || shown[0]?.file || "";
      }
      return;
    }
    if (!directories.has(currentDir)) {
      const parent = groupParent(currentDir);
      if (directories.has(parent)) currentDir = parent;
      else if (!shown.some((note) => filesystemGroupFor(note) === currentDir)) currentDir = "Root";
    }
    const currentEntries = directoryEntries(currentDir, groups, directories, files);
    if (!selectedEntryId || !currentEntries.some((entry) => entryId(entry) === selectedEntryId)) {
      const first = currentEntries.find((entry) => entry.type === "file" && entry.note.file === selectedFile)
        ?? currentEntries[0];
      setCurrentSelection(first);
    }
  }

  function selectedNoteFrom(entries: readonly RangerEntry[], shown: NoteSummary[]): NoteSummary | null {
    return entries.find((entry) => entry.type === "file" && entryId(entry) === selectedEntryId)?.note
      ?? shown.find((note) => note.file === selectedFile)
      ?? shown.find((note) => note.file === options.getCurrentFile())
      ?? shown[0]
      ?? null;
  }

  function entryId(entry: RangerEntry): string {
    if (entry.type === "dir") return `dir:${entry.path}`;
    if (entry.type === "asset") return `asset:${entry.file.file || entry.file.path || ""}`;
    return `file:${entry.note.file || entry.note.path || entry.note.id || ""}`;
  }

  function renderRangerRow(optionsArg: {
    label: string;
    meta?: string;
    icon: string;
    active?: boolean;
    title?: string;
    onClick: (event: MouseEvent) => void;
    onDoubleClick?: (event: MouseEvent) => void;
    onAuxClick?: (event: MouseEvent) => void;
  }): HTMLButtonElement {
    const button = document.createElement("button");
    button.type = "button";
    button.className = "aaronnote-ranger-row";
    if (optionsArg.active) button.classList.add("is-active");
    button.title = optionsArg.title || optionsArg.label;
    const icon = document.createElement("span");
    icon.className = "aaronnote-ranger-icon";
    icon.textContent = optionsArg.icon;
    const label = document.createElement("strong");
    label.textContent = optionsArg.label;
    const meta = document.createElement("span");
    meta.className = "aaronnote-ranger-meta";
    meta.textContent = optionsArg.meta || "";
    button.append(icon, label, meta);
    button.addEventListener("click", optionsArg.onClick);
    if (optionsArg.onDoubleClick) button.addEventListener("dblclick", optionsArg.onDoubleClick);
    if (optionsArg.onAuxClick) button.addEventListener("auxclick", optionsArg.onAuxClick);
    return button;
  }

  function renderPreview(note: NoteSummary | null, query: string): HTMLElement {
    const preview = document.createElement("section");
    preview.className = "aaronnote-ranger-preview";
    if (!note) {
      const empty = document.createElement("div");
      empty.className = "aaronnote-empty";
      empty.textContent = "No preview";
      preview.appendChild(empty);
      return preview;
    }
    const badge = document.createElement("span");
    badge.className = "aaronnote-ranger-kind";
    badge.textContent = (note.ext || note.kind || (note.roam ? "roam" : "note")).toUpperCase().slice(0, 12);
    const title = document.createElement("h2");
    title.textContent = noteTitle(note);
    const path = document.createElement("p");
    path.className = "aaronnote-ranger-path";
    path.textContent = note.path || note.file || note.id || "";
    preview.append(badge, title, path);

    const stats = document.createElement("dl");
    const fields: Array<[string, string]> = [
      ["Folder", filesystemGroupFor(note)],
      ["Date", note.date || ""],
      ["Section", note.section || ""],
      ["Source", note.source || ""],
      ["Refs", note.refs?.length ? String(note.refs.length) : ""],
      ["Backlinks", note.backlinks?.length ? String(note.backlinks.length) : ""],
    ].filter(([, value]) => Boolean(value));
    for (const [key, value] of fields) {
      const dt = document.createElement("dt");
      dt.textContent = key;
      const dd = document.createElement("dd");
      dd.textContent = value;
      stats.append(dt, dd);
    }
    preview.appendChild(stats);

    if (note.summary) {
      const summary = document.createElement("p");
      summary.className = "aaronnote-ranger-summary";
      summary.textContent = note.summary;
      preview.appendChild(summary);
    }
    if (note.tags?.length) {
      const tags = document.createElement("div");
      tags.className = "aaronnote-ranger-tags";
      for (const tag of note.tags.slice(0, 12)) {
        const pill = document.createElement("span");
        pill.textContent = `#${tag.replace(/^#/, "")}`;
        tags.appendChild(pill);
      }
      preview.appendChild(tags);
    }
    const open = document.createElement("button");
    open.type = "button";
    open.className = "aaronnote-ranger-open";
    open.textContent = query ? "Open match" : "Open file";
    open.addEventListener("click", (event) => options.openNote(note, { newWindow: event.altKey || event.metaKey }));
    preview.appendChild(open);
    return preview;
  }

  function appendPreviewActions(preview: HTMLElement, actions: Array<[string, (() => void) | undefined]>): void {
    const enabled = actions.filter(([, run]) => Boolean(run)) as Array<[string, () => void]>;
    if (enabled.length === 0) return;
    const row = document.createElement("div");
    row.className = "aaronnote-ranger-actions";
    for (const [label, run] of enabled) {
      const button = document.createElement("button");
      button.type = "button";
      button.textContent = label;
      button.addEventListener("click", () => run());
      row.appendChild(button);
    }
    preview.appendChild(row);
  }

  function renderDirectoryPreview(dir: string, directories: Map<string, DirectorySummary>): HTMLElement {
    const preview = document.createElement("section");
    preview.className = "aaronnote-ranger-preview";
    const summary = directories.get(dir);
    const badge = document.createElement("span");
    badge.className = "aaronnote-ranger-kind";
    badge.textContent = summary?.generated ? "FOLDER*" : "FOLDER";
    const title = document.createElement("h2");
    title.textContent = summary?.label || groupLabel(dir);
    const path = document.createElement("p");
    path.className = "aaronnote-ranger-path";
    path.textContent = dir;
    preview.append(badge, title, path);
    const stats = document.createElement("dl");
    const fields: Array<[string, string]> = [
      ["Notes", String(summary?.noteCount ?? 0)],
      ["Files", String(summary?.fileCount ?? 0)],
      ["Parent", groupParent(dir)],
    ];
    for (const [key, value] of fields) {
      const dt = document.createElement("dt");
      dt.textContent = key;
      const dd = document.createElement("dd");
      dd.textContent = value;
      stats.append(dt, dd);
    }
    preview.appendChild(stats);
    appendPreviewActions(preview, [
      ["New note", options.createNode ? () => options.createNode?.(dir === "Root" ? "" : dir) : undefined],
      ["New folder", options.createFolder ? () => void options.createFolder?.(dir === "Root" ? "" : dir).then((created) => {
        requestRangerFocus();
        if (created) extraDirs.add(normalizeDirectoryPath(created));
        requestRangerFocus();
        render();
      }) : undefined],
      ["Rename", dir !== "Root" && options.renameDirectory ? () => void options.renameDirectory?.(dir) : undefined],
      ["Move", dir !== "Root" && options.moveDirectory ? () => void options.moveDirectory?.(dir) : undefined],
      ["Reveal", options.revealPath ? () => void options.revealPath?.(dir) : undefined],
      ["Trash", dir !== "Root" && options.trashDirectory ? () => {
        requestRangerFocus();
        void options.trashDirectory?.(dir).then(() => {
          forgetExtraDirectory(dir);
          if (currentDir === dir || currentDir.startsWith(`${dir}/`)) currentDir = groupParent(dir);
          selectedEntryId = `dir:${currentDir}`;
          activePane = "current";
          requestRangerFocus();
          scheduleRender();
        });
      } : undefined],
    ]);
    return preview;
  }

  function renderAssetPreview(file: FileSummary): HTMLElement {
    const preview = document.createElement("section");
    preview.className = "aaronnote-ranger-preview";
    const badge = document.createElement("span");
    badge.className = "aaronnote-ranger-kind";
    badge.textContent = (file.ext || "file").toUpperCase().slice(0, 12);
    const title = document.createElement("h2");
    title.textContent = file.name || fileNameFromPath(file.path || file.file || "");
    const path = document.createElement("p");
    path.className = "aaronnote-ranger-path";
    path.textContent = file.path || file.file || "";
    preview.append(badge, title, path);
    const media = renderFilePreviewMedia(file);
    if (media) preview.appendChild(media);
    const stats = document.createElement("dl");
    const fields: Array<[string, string]> = [
      ["Folder", file.groupKey || "Root"],
      ["Type", file.type || ""],
      ["Size", typeof file.size === "number" ? `${file.size} bytes` : ""],
      ["Generated", file.generated ? "Yes" : ""],
    ].filter(([, value]) => Boolean(value));
    for (const [key, value] of fields) {
      const dt = document.createElement("dt");
      dt.textContent = key;
      const dd = document.createElement("dd");
      dd.textContent = value;
      stats.append(dt, dd);
    }
    preview.appendChild(stats);
    appendPreviewActions(preview, [
      ["Rename", options.renameFile ? () => void options.renameFile?.(file) : undefined],
      ["Move", options.moveFile ? () => void options.moveFile?.(file) : undefined],
      ["Duplicate", options.duplicateFile ? () => void options.duplicateFile?.(file) : undefined],
      ["Reveal", options.revealPath ? () => void options.revealPath?.(file.path || file.file || "") : undefined],
      ["Trash", options.deleteFile ? () => {
        requestRangerFocus();
        void options.deleteFile?.(file).then(() => {
          requestRangerFocus();
          scheduleRender();
        });
      } : undefined],
    ]);
    return preview;
  }

  function renderHelpOverlay(): HTMLElement {
    const overlay = document.createElement("div");
    overlay.className = "aaronnote-ranger-help";
    const panel = document.createElement("section");
    panel.setAttribute("role", "dialog");
    panel.setAttribute("aria-label", "Ranger shortcuts");
    const title = document.createElement("h2");
    title.textContent = "Ranger keys";
    const grid = document.createElement("dl");
    const rows: Array<[string, string]> = [
      ["j / k", "Move down / up"],
      ["h / l", "Parent pane / enter"],
      ["Enter", "Open note or enter folder"],
      ["n", "New note in current folder"],
      ["a", "New folder"],
      ["r", "Rename selected item"],
      ["m", "Move selected item"],
      ["D", "Duplicate selected file"],
      ["d", "Move selected item to Trash"],
      ["S", "Open selected folder in system"],
      ["K", "Open selected folder in Kitty"],
      ["Tab", "Switch Recent / Filesystem"],
      [".", "Show or hide all files"],
      ["?", "Show or hide this help"],
      ["Esc", "Close help"],
    ];
    for (const [key, desc] of rows) {
      const dt = document.createElement("dt");
      dt.textContent = key;
      const dd = document.createElement("dd");
      dd.textContent = desc;
      grid.append(dt, dd);
    }
    const close = document.createElement("button");
    close.type = "button";
    close.textContent = "Close";
    close.addEventListener("click", () => {
      helpVisible = false;
      render();
    });
    panel.append(title, grid, close);
    overlay.appendChild(panel);
    overlay.addEventListener("mousedown", (event) => {
      if (event.target === overlay) {
        helpVisible = false;
        render();
      }
    });
    return overlay;
  }

  function recentEntries(): RecentEntryItem[] {
    const notes = options.getNotes();
    const byFile = new Map(notes.map((note) => [note.file, note]));
    return options.getRecentNotes()
      .map((entry) => ({
        entry,
        note: byFile.get(entry.file) || { file: entry.file, path: entry.file, title: fileNameFromPath(entry.file), standalone: true },
      }))
      .filter((item): item is { entry: RecentNote; note: NoteSummary } => Boolean(item.note?.file))
      .map((item) => ({ ...item, file: item.note.file || item.entry.file }));
  }

  function selectedRecentIndex(entries: readonly RecentEntryItem[]): number {
    return entries.findIndex((item) => item.file === selectedRecentFile);
  }

  function recentColumnStep(): number {
    const style = getComputedStyle(options.recentList);
    const columns = style.gridTemplateColumns
      .split(/\s+/u)
      .filter((part) => part && part !== "none").length;
    return Math.max(1, columns);
  }

  function setRecentSelection(index: number): void {
    const entries = recentEntries();
    if (entries.length === 0) {
      selectedRecentFile = "";
      renderRecent();
      return;
    }
    const nextIndex = Math.max(0, Math.min(entries.length - 1, index));
    selectedRecentFile = entries[nextIndex]?.file || "";
    focusRecentAfterRender = true;
    renderRecent();
  }

  function moveRecentSelection(delta: number): void {
    const entries = recentEntries();
    if (entries.length === 0) return;
    const index = selectedRecentIndex(entries);
    const baseIndex = index >= 0 ? index : delta > 0 ? -1 : entries.length;
    setRecentSelection(baseIndex + delta);
  }

  function openSelectedRecent(event: KeyboardEvent): void {
    const entries = recentEntries();
    const item = entries[selectedRecentIndex(entries)] ?? entries[0];
    if (!item) return;
    options.openNote(item.note, { newWindow: event.altKey || event.metaKey });
  }

  function scrollRecentSelectionIntoView(): void {
    const selected = Array.from(options.recentList.querySelectorAll<HTMLElement>(".aaronnote-note"))
      .find((button) => button.dataset.recentFile === selectedRecentFile)
      ?? options.recentList.querySelector<HTMLElement>(".aaronnote-note");
    selected?.scrollIntoView({ block: "nearest", inline: "nearest" });
  }

  function restoreRecentFocusIfRequested(): void {
    if (!focusRecentAfterRender) return;
    focusRecentAfterRender = false;
    options.recentList.focus({ preventScroll: true });
    scrollRecentSelectionIntoView();
  }

  function renderRecent(): void {
    const entries = recentEntries();
    const currentFile = options.getCurrentFile();
    if (entries.length === 0) selectedRecentFile = "";
    else if (!selectedRecentFile || !entries.some((item) => item.file === selectedRecentFile)) {
      selectedRecentFile = entries.find((item) => item.file === currentFile)?.file || entries[0]?.file || "";
    }
    const renderKey = [
      currentFile,
      selectedRecentFile,
      ...entries.map(({ entry, note }) => `${entry.file}\u0000${entry.openedAt}\u0000${note.title || ""}\u0000${note.path || ""}`),
    ].join("\u0001");
    if (renderKey === lastRecentRenderKey && options.recentList.childNodes.length > 0) {
      restoreRecentFocusIfRequested();
      return;
    }
    lastRecentRenderKey = renderKey;

    const frag = document.createDocumentFragment();
    if (entries.length === 0) {
      const empty = document.createElement("div");
      empty.className = "aaronnote-empty";
      empty.textContent = "No recent notes";
      frag.appendChild(empty);
      options.recentList.replaceChildren(frag);
      restoreRecentFocusIfRequested();
      return;
    }
    for (const { entry, note, file } of entries) {
      const button = renderNoteButton(note, note.standalone ? "Standalone Markdown" : note.path || note.id || "", formatShortDateTime(entry.openedAt));
      button.dataset.recentFile = file;
      button.tabIndex = -1;
      button.classList.toggle("is-active", file === selectedRecentFile);
      button.setAttribute("role", "option");
      button.setAttribute("aria-selected", file === selectedRecentFile ? "true" : "false");
      if (file === currentFile) button.setAttribute("aria-current", "page");
      frag.appendChild(button);
    }
    options.recentList.replaceChildren(frag);
    restoreRecentFocusIfRequested();
  }

  function renderLimitMessage(parent: DocumentFragment, shownCount: number, totalCount: number): void {
    if (shownCount >= totalCount) return;
    const message = document.createElement("div");
    message.className = "aaronnote-files-limit";
    message.textContent = `Showing ${shownCount} of ${totalCount}. Refine the filter to narrow the list.`;
    parent.appendChild(message);
  }

  function render(): void {
    const activeElement = document.activeElement;
    const restoreRangerFocus = focusAfterRender || activeElement instanceof HTMLElement
      && options.noteList.contains(activeElement)
      && Boolean(activeElement.closest(".aaronnote-ranger"));
    focusAfterRender = false;
    const notes = options.getNotes();
    const directoriesList = options.getDirectories?.() ?? [];
    const fileList = options.getFiles?.() ?? [];
    const showAll = options.getShowAllFiles?.() === true;
    options.managementCount.textContent = `${notes.filter((note) => note.roam).length} / ${notes.length}`;
    renderRecent();
    const query = options.noteFilter.value.trim();
    const renderKey = [
      options.getCurrentFile(),
      query,
      currentDir,
      selectedFile,
      selectedEntryId,
      parentEntryId,
      activePane,
      notes.length,
      notesSignature(notes),
      directoriesSignature(directoriesList),
      filesSignature(fileList),
      showAll ? "all" : "notes",
      helpVisible ? "help" : "",
    ].join("\u0001");
    if (renderKey === lastRenderKey && options.noteList.childNodes.length > 0) {
      if (restoreRangerFocus) focus();
      return;
    }
    lastRenderKey = renderKey;
    const parsedQuery = parseSearchQuery(query);
    const filtered = notes.filter((note) => !query || noteMatchesSearch(note, parsedQuery, searchCache));
    const renderLimit = query ? searchRenderLimit : browseRenderLimit;
    const shown = filtered.slice(0, renderLimit);
    options.noteCount.textContent = query ? `${shown.length} / ${filtered.length} matches` : `${notes.length} notes`;

    const groups = groupsFromNotes(shown);
    const visibleFiles = filesFromSummaries(fileList, showAll && !query);
    const directoryMap = query
      ? directoryMapFromSummaries([], groups, true)
      : directoryMapFromSummaries(directoriesList, groups, showAll, extraDirs);
    const rootHasEntries = directoryEntries("Root", groups, directoryMap, visibleFiles).length > 0;

    const frag = document.createDocumentFragment();
    if (shown.length === 0 && (query || !rootHasEntries)) {
      const empty = document.createElement("div");
      empty.className = "aaronnote-empty";
      empty.textContent = query ? "No notes" : "No files";
      frag.appendChild(empty);
      options.noteList.replaceChildren(frag);
      return;
    }

    ensureSelection(groups, directoryMap, visibleFiles, shown, query);
    if (query) activePane = "current";
    const shell = document.createElement("div");
    shell.className = "aaronnote-ranger";
    shell.tabIndex = 0;
    const dirs = document.createElement("section");
    dirs.className = "aaronnote-ranger-pane aaronnote-ranger-dirs";
    const files = document.createElement("section");
    files.className = "aaronnote-ranger-pane aaronnote-ranger-files";

    const parentDir = groupParent(currentDir);
    const parentEntries = directoryEntries(parentDir, groups, directoryMap, visibleFiles);
    const currentEntries: RangerEntry[] = query
      ? sortedNotes(shown).map((note) => ({ type: "file" as const, note, label: noteTitle(note), meta: filesystemGroupFor(note) }))
      : directoryEntries(currentDir, groups, directoryMap, visibleFiles);
    if (!currentEntries.some((entry) => entryId(entry) === selectedEntryId)) {
      const first = currentEntries.find((entry) => entry.type === "file" && entry.note.file === selectedFile)
        ?? currentEntries[0];
      setCurrentSelection(first);
    }
    if (!parentEntries.some((entry) => entryId(entry) === parentEntryId)) {
      const currentDirEntry = parentEntries.find((entry) => entry.type === "dir" && entry.path === currentDir);
      parentEntryId = currentDirEntry ? entryId(currentDirEntry) : "";
    }

    for (const entry of parentEntries) {
      const active = activePane === "parent" && entryId(entry) === parentEntryId;
      dirs.appendChild(renderRangerRow({
        label: entry.label,
        meta: entry.type === "dir" ? String(entry.count) : entry.meta,
        icon: entry.type === "dir" ? ">" : entry.type === "asset" ? "-" : entry.note.file === options.getCurrentFile() ? "*" : ".",
        active,
        title: entry.type === "dir" ? entry.path : entry.type === "asset" ? entry.file.path || entry.file.file || "" : entry.note.file || "",
        onClick: (event) => {
          activePane = "parent";
          activateParentEntry(entry);
          if (entry.type === "file" && shouldOpenFileClick(entry, event)) {
            options.openNote(entry.note, { newWindow: event.altKey || event.metaKey });
            return;
          }
          render();
        },
      }));
    }

    for (const entry of currentEntries) {
      const active = activePane === "current" && entryId(entry) === selectedEntryId;
      files.appendChild(renderRangerRow({
        label: entry.label,
        meta: entry.type === "dir" ? String(entry.count) : entry.meta,
        icon: entry.type === "dir" ? ">" : entry.type === "asset" ? "-" : entry.note.file === options.getCurrentFile() ? "*" : ".",
        active,
        title: entry.type === "dir" ? entry.path : entry.type === "asset" ? entry.file.path || entry.file.file || "" : entry.note.file || entry.note.path || entry.note.id || "",
        onClick: (event) => {
          activePane = "current";
          if (entry.type === "dir") {
            selectDirectory(entry.path, groups, directoryMap, visibleFiles);
            render();
          } else if (entry.type === "file") {
            setCurrentSelection(entry);
            if (shouldOpenFileClick(entry, event)) {
              options.openNote(entry.note, { newWindow: event.altKey || event.metaKey });
            }
            else render();
          } else {
            setCurrentSelection(entry);
            render();
          }
        },
        onAuxClick: (event) => {
          if (event.button !== 1 || entry.type !== "file") return;
          event.preventDefault();
          setCurrentSelection(entry);
          options.openNote(entry.note, { newWindow: true });
        },
      }));
    }

    function nextEntry(entries: readonly RangerEntry[], currentId: string, delta: number): RangerEntry | null {
      if (entries.length === 0) return null;
      const index = entries.findIndex((entry) => entryId(entry) === currentId);
      const baseIndex = index >= 0 ? index : delta > 0 ? -1 : entries.length;
      const nextIndex = Math.max(0, Math.min(entries.length - 1, baseIndex + delta));
      return entries[nextIndex] ?? null;
    }

    function selectedEntry(): RangerEntry | null {
      return currentEntries.find((entry) => entryId(entry) === selectedEntryId) ?? currentEntries[0] ?? null;
    }

    function selectedParentEntry(): RangerEntry | null {
      return parentEntries.find((entry) => entryId(entry) === parentEntryId) ?? null;
    }

    function activateParentEntry(entry: RangerEntry): void {
      parentEntryId = entryId(entry);
      if (entry.type === "dir") {
        selectDirectory(entry.path, groups, directoryMap, visibleFiles);
      } else {
        setCurrentSelection(entry);
      }
    }

    function activateParentPane(): void {
      if (!parentEntryId) return;
      activePane = "parent";
      render();
    }

    function activateCurrentPane(): void {
      const entry = selectedParentEntry();
      if (entry?.type === "file") {
        selectDirectory(filesystemGroupFor(entry.note), groups, directoryMap, visibleFiles, entryId(entry));
      }
      activePane = "current";
      render();
    }

    function moveUpDirectory(): void {
      const nextDir = groupParent(currentDir);
      if (nextDir === currentDir) return;
      const previousDir = currentDir;
      selectDirectory(nextDir, groups, directoryMap, visibleFiles, `dir:${previousDir}`);
      const nextParentEntries = directoryEntries(groupParent(nextDir), groups, directoryMap, visibleFiles);
      const nextParentId = `dir:${nextDir}`;
      parentEntryId = nextParentEntries.some((entry) => entryId(entry) === nextParentId) ? nextParentId : "";
      activePane = parentEntryId ? "parent" : "current";
      render();
    }

    function openSelected(event: KeyboardEvent): void {
      const entry = selectedEntry();
      if (!entry) return;
      if (entry.type === "dir") {
        selectDirectory(entry.path, groups, directoryMap, visibleFiles);
        activePane = "current";
        render();
      } else if (entry.type === "file") {
        options.openNote(entry.note, { newWindow: event.altKey || event.metaKey });
      }
    }

    function moveCurrent(delta: number): void {
      const next = nextEntry(currentEntries, selectedEntryId, delta);
      if (!next) return;
      activePane = "current";
      setCurrentSelection(next);
      render();
    }

    function moveParent(delta: number): void {
      const next = nextEntry(parentEntries, parentEntryId, delta);
      if (!next) return;
      activePane = "parent";
      activateParentEntry(next);
      render();
    }

    async function trashSelectedEntry(entry: RangerEntry | null): Promise<void> {
      if (!entry) return;
      requestRangerFocus();
      if (entry.type === "file" && options.deleteNote) {
        options.deleteNote(entry.note);
      } else if (entry.type === "dir" && entry.path !== "Root" && options.trashDirectory) {
        const trashed = entry.path;
        await options.trashDirectory(trashed);
        forgetExtraDirectory(trashed);
        if (currentDir === trashed || currentDir.startsWith(`${trashed}/`)) currentDir = groupParent(trashed);
        selectedEntryId = `dir:${currentDir}`;
        activePane = "current";
      } else if (entry.type === "asset" && options.deleteFile) {
        await options.deleteFile(entry.file);
      }
      requestRangerFocus();
      scheduleRender();
    }

    function deleteSelectedFile(): void {
      const entry = activePane === "parent" ? selectedParentEntry() : selectedEntry();
      void trashSelectedEntry(entry);
    }

    function createNodeFromRanger(): void {
      requestRangerFocus();
      options.createNode?.(currentDir === "Root" ? "" : currentDir, { stayInFilesystem: true });
    }

    async function createFolderFromRanger(): Promise<void> {
      if (!options.createFolder) return;
      requestRangerFocus();
      const created = await options.createFolder(currentDir === "Root" ? "" : currentDir);
      if (!created) return;
      const dir = normalizeDirectoryPath(created);
      extraDirs.add(dir);
      selectDirectory(dir, groups, directoryMap, visibleFiles);
      activePane = "current";
      requestRangerFocus();
      render();
    }

    function renameSelectedEntry(): void {
      const entry = activePane === "parent" ? selectedParentEntry() : selectedEntry();
      if (entry?.type === "file" && options.renameNote) void options.renameNote(entry.note);
      else if (entry?.type === "asset" && options.renameFile) void options.renameFile(entry.file);
      else if (entry?.type === "dir" && entry.path !== "Root" && options.renameDirectory) void options.renameDirectory(entry.path);
    }

    function moveSelectedEntry(): void {
      const entry = activePane === "parent" ? selectedParentEntry() : selectedEntry();
      if (entry?.type === "file" && options.moveNote) void options.moveNote(entry.note);
      else if (entry?.type === "asset" && options.moveFile) void options.moveFile(entry.file);
      else if (entry?.type === "dir" && entry.path !== "Root" && options.moveDirectory) void options.moveDirectory(entry.path);
    }

    function duplicateSelectedEntry(): void {
      const entry = activePane === "parent" ? selectedParentEntry() : selectedEntry();
      if (entry?.type === "file" && options.duplicateNote) void options.duplicateNote(entry.note);
      else if (entry?.type === "asset" && options.duplicateFile) void options.duplicateFile(entry.file);
    }

    function entryTargetPath(entry: RangerEntry): string {
      if (entry.type === "dir") return entry.path;
      if (entry.type === "file") return entry.note.path || entry.note.file || "";
      return entry.file.path || entry.file.file || "";
    }

    function openSelectedDirectory(): void {
      const entry = activePane === "parent" ? selectedParentEntry() : selectedEntry();
      if (!entry || !options.openDirectory) return;
      void options.openDirectory(entryTargetPath(entry));
    }

    function openSelectedDirectoryInKitty(): void {
      const entry = activePane === "parent" ? selectedParentEntry() : selectedEntry();
      if (!entry || !options.openDirectoryInKitty) return;
      void options.openDirectoryInKitty(entryTargetPath(entry));
    }

    shell.addEventListener("keydown", (event) => {
      const key = event.key.length === 1 ? event.key.toLowerCase() : event.key;
      if ((event.ctrlKey || event.metaKey) && key === "Enter") return;
      if ((event.ctrlKey || event.metaKey || event.altKey) && key !== "Enter") return;
      if (event.key === "Escape" && helpVisible) {
        event.preventDefault();
        helpVisible = false;
        render();
      } else if (event.key === "?") {
        event.preventDefault();
        helpVisible = !helpVisible;
        render();
      } else if (event.key === "S") {
        event.preventDefault();
        openSelectedDirectory();
      } else if (event.key === "K") {
        event.preventDefault();
        openSelectedDirectoryInKitty();
      } else if (event.key === ".") {
        event.preventDefault();
        requestRangerFocus();
        options.toggleShowAllFiles?.();
      } else if (key === "ArrowLeft" || key === "h") {
        event.preventDefault();
        if (activePane === "current") activateParentPane();
        else moveUpDirectory();
      } else if (key === "ArrowRight" || key === "l" || key === "Enter") {
        event.preventDefault();
        if (activePane === "parent") activateCurrentPane();
        else openSelected(event);
      } else if (key === "ArrowUp" || key === "k") {
        event.preventDefault();
        if (activePane === "parent" || event.shiftKey) moveParent(-1);
        else moveCurrent(-1);
      } else if (key === "ArrowDown" || key === "j") {
        event.preventDefault();
        if (activePane === "parent" || event.shiftKey) moveParent(1);
        else moveCurrent(1);
      } else if (key === "PageUp" || key === "PageDown") {
        event.preventDefault();
        if (activePane === "parent" || event.shiftKey) moveParent(key === "PageDown" ? 8 : -8);
        else moveCurrent(key === "PageDown" ? 8 : -8);
      } else if (event.key === "D") {
        event.preventDefault();
        duplicateSelectedEntry();
      } else if (key === "d") {
        event.preventDefault();
        deleteSelectedFile();
      } else if (key === "r") {
        event.preventDefault();
        renameSelectedEntry();
      } else if (key === "m") {
        event.preventDefault();
        moveSelectedEntry();
      } else if (key === "n") {
        event.preventDefault();
        createNodeFromRanger();
      } else if (key === "a") {
        event.preventDefault();
        void createFolderFromRanger();
      }
    });

    const previewEntry = activePane === "parent" ? selectedParentEntry() : selectedEntry();
    const preview = previewEntry?.type === "dir"
      ? renderDirectoryPreview(previewEntry.path, directoryMap)
      : previewEntry?.type === "asset"
        ? renderAssetPreview(previewEntry.file)
        : renderPreview(previewEntry?.type === "file" ? previewEntry.note : selectedNoteFrom(currentEntries, shown), query);
    if (previewEntry?.type === "file") {
      appendPreviewActions(preview, [
        ["Rename", options.renameNote ? () => void options.renameNote?.(previewEntry.note) : undefined],
        ["Move", options.moveNote ? () => void options.moveNote?.(previewEntry.note) : undefined],
        ["Duplicate", options.duplicateNote ? () => void options.duplicateNote?.(previewEntry.note) : undefined],
        ["Reveal", options.revealPath ? () => void options.revealPath?.(previewEntry.note.path || previewEntry.note.file || "") : undefined],
        ["Trash", options.deleteNote ? () => void trashSelectedEntry(previewEntry) : undefined],
      ]);
    }
    shell.append(dirs, files, preview);
    if (helpVisible) shell.appendChild(renderHelpOverlay());
    frag.appendChild(shell);
    renderLimitMessage(frag, shown.length, filtered.length);
    options.noteList.replaceChildren(frag);
    if (restoreRangerFocus) {
      shell.focus({ preventScroll: true });
      shell.querySelector<HTMLElement>(".aaronnote-ranger-row.is-active")?.scrollIntoView({ block: "nearest" });
    }
  }

  function focus(): boolean {
    const target = options.noteList.querySelector<HTMLElement>(".aaronnote-ranger");
    if (!target) return false;
    target.focus();
    return document.activeElement === target;
  }

  function focusRangerAfterFilterRender(): void {
    requestRangerFocus();
    render();
  }

  function openSelectedFilterResult(event: KeyboardEvent): boolean {
    render();
    const note = options.getNotes().find((item) => item.file === selectedFile);
    if (!note) return focus();
    options.openNote(note, { newWindow: event.altKey || event.metaKey });
    return true;
  }

  function scheduleRender(): void {
    window.cancelAnimationFrame(renderFrame);
    renderFrame = window.requestAnimationFrame(render);
  }

  function collapseAll(): void {
    currentDir = groupParent(currentDir || currentFilesystemGroup(options.getNotes()));
    selectedFile = "";
    render();
  }

  function expandAll(): void {
    const notes = options.getNotes();
    const current = notes.find((note) => note.file === options.getCurrentFile());
    if (current) {
      currentDir = filesystemGroupFor(current);
      selectedFile = current.file || "";
    }
    render();
  }

  function focusRecent(): boolean {
    renderRecent();
    focusRecentAfterRender = true;
    restoreRecentFocusIfRequested();
    return document.activeElement === options.recentList;
  }

  options.recentList.tabIndex = 0;
  options.recentList.setAttribute("role", "listbox");
  options.recentList.setAttribute("aria-label", "Recent notes");
  options.recentList.addEventListener("keydown", (event) => {
    if (event.ctrlKey) return;
    if ((event.metaKey || event.altKey) && event.key !== "Enter") return;
    const key = event.key.length === 1 ? event.key.toLowerCase() : event.key;
    if (key === "ArrowLeft" || key === "h") {
      event.preventDefault();
      moveRecentSelection(-1);
    } else if (key === "ArrowRight" || key === "l") {
      event.preventDefault();
      moveRecentSelection(1);
    } else if (key === "ArrowUp" || key === "k") {
      event.preventDefault();
      moveRecentSelection(-recentColumnStep());
    } else if (key === "ArrowDown" || key === "j") {
      event.preventDefault();
      moveRecentSelection(recentColumnStep());
    } else if (key === "Home") {
      event.preventDefault();
      setRecentSelection(0);
    } else if (key === "End") {
      event.preventDefault();
      setRecentSelection(recentEntries().length - 1);
    } else if (key === "Enter") {
      event.preventDefault();
      openSelectedRecent(event);
    }
  });

  options.noteFilter.setAttribute("aria-autocomplete", "list");
  options.noteFilter.setAttribute("aria-expanded", "false");
  options.noteFilter.addEventListener("input", showSearchSuggestions);
  options.noteFilter.addEventListener("focus", showSearchSuggestions);
  options.noteFilter.addEventListener("keydown", (event) => {
    if (event.key === "ArrowDown" && searchSuggestions.length > 0) {
      event.preventDefault();
      setActiveSearchSuggestion(searchActiveSuggestionIndex + 1);
      return;
    }
    if (event.key === "ArrowUp" && searchSuggestions.length > 0) {
      event.preventDefault();
      setActiveSearchSuggestion(searchActiveSuggestionIndex - 1);
      return;
    }
    if ((event.key === "Home" || event.key === "End") && searchSuggestions.length > 0) {
      event.preventDefault();
      setActiveSearchSuggestion(event.key === "Home" ? 0 : searchSuggestions.length - 1);
      return;
    }
    if (event.key === "Enter" && searchActiveSuggestionIndex >= 0) {
      event.preventDefault();
      applySearchSuggestion(searchSuggestions[searchActiveSuggestionIndex]);
      return;
    }
    if (event.key === "Enter") {
      event.preventDefault();
      closeSearchSuggestions();
      if (options.noteFilter.value.trim()) openSelectedFilterResult(event);
      else focusRangerAfterFilterRender();
      return;
    }
    if (event.key === "Escape" && searchSuggestionBox && !searchSuggestionBox.hidden) {
      event.preventDefault();
      closeSearchSuggestions();
      return;
    }
    if (event.key === "Escape" && options.noteFilter.value.trim()) {
      event.preventDefault();
      options.noteFilter.value = "";
      closeSearchSuggestions();
      focusRangerAfterFilterRender();
      return;
    }
    if (event.key === "Escape") {
      event.preventDefault();
      focusRangerAfterFilterRender();
    }
  });
  document.addEventListener("mousedown", (event) => {
    const target = event.target as Node | null;
    if (!target || !searchSuggestionBox || searchSuggestionBox.hidden) return;
    if (target === options.noteFilter || options.noteFilter.contains(target) || searchSuggestionBox.contains(target)) return;
    closeSearchSuggestions();
  });
  window.addEventListener("resize", closeSearchSuggestions);

  return { render, renderRecent, scheduleRender, collapseAll, expandAll, focus, focusRecent };
}
