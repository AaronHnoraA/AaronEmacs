import { api } from "./api-client.ts";
import type { GitChange, GitCommitEntry, GitRepoStatus } from "./types.ts";
import { Epoch } from "../src/async-epoch.ts";

export type GitPanel = {
  refresh: () => void;
  deactivate: () => void;
};

type GitPanelOptions = {
  root: HTMLElement;
  getCurrentFile: () => string;
  openNote: (file: string) => void | Promise<void>;
  setStatus: (message: string) => void;
  syncRoamDb: () => Promise<void>;
  beforeRefresh?: () => Promise<void>;
};

type DiffTarget =
  | { type: "change"; change: GitChange }
  | { type: "commit"; commit: GitCommitEntry };

function requireElement<T extends HTMLElement>(root: HTMLElement, selector: string): T {
  const element = root.querySelector<T>(selector);
  if (!element) throw new Error("Missing Git panel element: " + selector);
  return element;
}

function shortSha(sha: string | undefined): string {
  return String(sha || "").slice(0, 8);
}

function formatCommitDate(value: string | undefined): string {
  const time = Date.parse(String(value || ""));
  if (!Number.isFinite(time)) return String(value || "").slice(0, 16).replace("T", " ");
  return new Intl.DateTimeFormat(undefined, {
    month: "short",
    day: "numeric",
    hour: "2-digit",
    minute: "2-digit",
  }).format(new Date(time));
}

function statusLabel(change: GitChange): string {
  const kind = change.kind || "changed";
  if (kind === "untracked") return "UNTRACKED";
  if (kind === "modified") return "MODIFIED";
  if (kind === "added") return "ADDED";
  if (kind === "deleted") return "DELETED";
  if (kind === "renamed") return "RENAMED";
  if (kind === "conflict") return "CONFLICT";
  return String(change.status || "??").trim() || "CHANGE";
}

function summarizeStatus(status: GitRepoStatus, changes: GitChange[]): string {
  const parts: string[] = [];
  if (status.branch) parts.push(status.branch);
  if ((status.ahead || 0) > 0) parts.push(String(status.ahead) + " ahead");
  if ((status.behind || 0) > 0) parts.push(String(status.behind) + " behind");
  if (changes.length) parts.push(String(changes.length) + " changed");
  if (!status.hasRemote) parts.push("no remote");
  return parts.join(" / ") || "Clean";
}

function changeSortKey(change: GitChange): string {
  return [change.kind === "conflict" ? "0" : "1", change.path || change.file || ""].join("|");
}

function renderEmpty(text: string): HTMLElement {
  const empty = document.createElement("div");
  empty.className = "aaronnote-empty";
  empty.textContent = text;
  return empty;
}

function renderDiffLines(diffEl: HTMLElement, diff: string): void {
  const text = String(diff || "").trimEnd();
  if (!text) {
    diffEl.replaceChildren(renderEmpty("No diff to show"));
    return;
  }
  const fragment = document.createDocumentFragment();
  for (const line of text.split("\n")) {
    const row = document.createElement("div");
    row.className = "aaronnote-git-diff-line";
    if (line.startsWith("+") && !line.startsWith("+++")) row.classList.add("is-add");
    else if (line.startsWith("-") && !line.startsWith("---")) row.classList.add("is-del");
    else if (line.startsWith("@@")) row.classList.add("is-hunk");
    else if (line.startsWith("diff --git") || line.startsWith("# ")) row.classList.add("is-head");
    row.textContent = line || " ";
    fragment.appendChild(row);
  }
  diffEl.replaceChildren(fragment);
}

export function createGitPanel(options: GitPanelOptions): GitPanel {
  const root = options.root;
  const branchEl = requireElement<HTMLElement>(root, "[data-git-branch]");
  const summaryEl = requireElement<HTMLElement>(root, "[data-git-summary]");
  const remoteEl = requireElement<HTMLElement>(root, "[data-git-remote]");
  const countsEl = requireElement<HTMLElement>(root, "[data-git-counts]");
  const changesEl = requireElement<HTMLElement>(root, "[data-git-changes]");
  const historyEl = requireElement<HTMLElement>(root, "[data-git-history]");
  const diffTitleEl = requireElement<HTMLElement>(root, "[data-git-diff-title]");
  const diffMetaEl = requireElement<HTMLElement>(root, "[data-git-diff-meta]");
  const diffEl = requireElement<HTMLElement>(root, "[data-git-diff]");
  const messageInput = requireElement<HTMLInputElement>(root, "[data-git-message]");
  const refreshButton = requireElement<HTMLButtonElement>(root, "[data-action='git-refresh']");
  const commitButton = requireElement<HTMLButtonElement>(root, "[data-action='git-commit']");
  const pullButton = requireElement<HTMLButtonElement>(root, "[data-action='git-pull']");
  const pushButton = requireElement<HTMLButtonElement>(root, "[data-action='git-push']");
  const syncButton = requireElement<HTMLButtonElement>(root, "[data-action='git-sync']");
  const openButton = requireElement<HTMLButtonElement>(root, "[data-action='git-open-file']");
  const restoreButton = requireElement<HTMLButtonElement>(root, "[data-action='git-restore-file']");

  let status: GitRepoStatus = {};
  let changes: GitChange[] = [];
  let history: GitCommitEntry[] = [];
  let selected: DiffTarget | null = null;
  let active = false;
  const refreshEpoch = new Epoch();
  const diffEpoch = new Epoch();

  function selectedChange(): GitChange | null {
    return selected?.type === "change" ? selected.change : null;
  }

  function renderStatus(): void {
    branchEl.textContent = status.branch || "No branch";
    summaryEl.textContent = summarizeStatus(status, changes);
    remoteEl.textContent = status.hasRemote ? (status.remoteUrl || "origin") : "No remote configured";
    countsEl.textContent = [
      String(changes.length) + " files",
      String(changes.filter((item) => item.staged).length) + " staged",
      String(changes.filter((item) => item.unstaged).length) + " unstaged",
    ].join(" / ");
    commitButton.disabled = changes.length === 0;
    pushButton.disabled = !status.hasRemote;
    pullButton.disabled = !status.hasRemote;
  }

  function renderChanges(): void {
    if (changes.length === 0) {
      changesEl.replaceChildren(renderEmpty("Working tree is clean"));
      openButton.disabled = true;
      restoreButton.disabled = true;
      return;
    }
    const fragment = document.createDocumentFragment();
    for (const change of changes) {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "aaronnote-git-change";
      button.dataset.gitChange = change.file || change.path;
      button.classList.toggle("is-active", selected?.type === "change" && selected.change.file === change.file);

      const badge = document.createElement("span");
      badge.className = "aaronnote-git-badge";
      badge.textContent = statusLabel(change);

      const body = document.createElement("span");
      body.className = "aaronnote-git-change-body";
      const title = document.createElement("strong");
      title.textContent = change.path || change.file || "Untitled";
      const meta = document.createElement("span");
      meta.textContent = [change.summary, change.oldPath ? "from " + change.oldPath : ""].filter(Boolean).join(" / ");
      body.append(title, meta);
      button.append(badge, body);
      fragment.appendChild(button);
    }
    changesEl.replaceChildren(fragment);
    openButton.disabled = !selectedChange()?.file;
    restoreButton.disabled = !selectedChange()?.file;
  }

  function renderHistory(): void {
    if (history.length === 0) {
      historyEl.replaceChildren(renderEmpty("No commits yet"));
      return;
    }
    const fragment = document.createDocumentFragment();
    for (const commit of history) {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "aaronnote-git-commit";
      button.dataset.gitCommit = commit.sha;
      button.classList.toggle("is-active", selected?.type === "commit" && selected.commit.sha === commit.sha);
      const title = document.createElement("strong");
      title.textContent = commit.subject || "Untitled commit";
      const meta = document.createElement("span");
      meta.textContent = formatCommitDate(commit.date) + " / " + shortSha(commit.sha);
      button.append(title, meta);
      fragment.appendChild(button);
    }
    historyEl.replaceChildren(fragment);
  }

  async function loadChangeDiff(change: GitChange): Promise<void> {
    if (!active) return;
    selected = { type: "change", change };
    renderChanges();
    renderHistory();
    const run = diffEpoch.begin();
    diffTitleEl.textContent = change.path || change.file || "Change";
    diffMetaEl.textContent = change.summary || statusLabel(change);
    diffEl.replaceChildren(renderEmpty("Loading diff..."));
    try {
      const msg = await api.roamTools.diff({ file: change.file || change.path });
      if (!run.current || !active) return;
      renderDiffLines(diffEl, msg.diff || "");
    } catch (err) {
      if (!run.current || !active) return;
      diffEl.replaceChildren(renderEmpty(err instanceof Error ? err.message : "Diff failed"));
    }
  }

  async function loadCommitDiff(commit: GitCommitEntry): Promise<void> {
    if (!active) return;
    selected = { type: "commit", commit };
    renderChanges();
    renderHistory();
    const run = diffEpoch.begin();
    diffTitleEl.textContent = commit.subject || "Commit";
    diffMetaEl.textContent = formatCommitDate(commit.date) + " / " + shortSha(commit.sha);
    diffEl.replaceChildren(renderEmpty("Loading commit..."));
    try {
      const msg = await api.roamTools.commitDiff(commit.sha);
      if (!run.current || !active) return;
      renderDiffLines(diffEl, msg.diff || "");
    } catch (err) {
      if (!run.current || !active) return;
      diffEl.replaceChildren(renderEmpty(err instanceof Error ? err.message : "Commit diff failed"));
    }
  }

  function chooseInitialSelection(): void {
    const currentFile = options.getCurrentFile();
    const existingChange = changes.find((item) => item.file === currentFile)
      || changes.find((item) => selected?.type === "change" && item.file === selected.change.file)
      || changes[0];
    if (existingChange) {
      void loadChangeDiff(existingChange);
      return;
    }
    selected = null;
    diffTitleEl.textContent = "Diff";
    diffMetaEl.textContent = "No target selected";
    diffEl.replaceChildren(renderEmpty("Select a file or commit"));
  }

  async function refresh(refreshOptions: { beforeRefresh?: boolean } = {}): Promise<void> {
    active = true;
    const run = refreshEpoch.begin();
    options.setStatus("Loading git state");
    try {
      if (refreshOptions.beforeRefresh !== false) await options.beforeRefresh?.();
      if (!run.current || !active) return;
      const [statusMsg, changesMsg, historyMsg] = await Promise.all([
        api.roamTools.repoStatus(),
        api.roamTools.changes(),
        api.roamTools.repoHistory(40),
      ]);
      if (!run.current || !active) return;
      status = statusMsg;
      changes = (changesMsg.changes || [])
        .filter((change) => change.isMarkdown)
        .slice()
        .sort((a, b) => changeSortKey(a).localeCompare(changeSortKey(b)));
      history = historyMsg.entries || [];
      renderStatus();
      renderChanges();
      renderHistory();
      chooseInitialSelection();
      options.setStatus(summarizeStatus(status, changes));
    } catch (err) {
      if (!run.current || !active) return;
      const message = err instanceof Error ? err.message : "Git state failed";
      summaryEl.textContent = message;
      changesEl.replaceChildren(renderEmpty(message));
      historyEl.replaceChildren(renderEmpty("History unavailable"));
      diffEl.replaceChildren(renderEmpty(message));
      options.setStatus(message);
    }
  }

  async function commit(): Promise<void> {
    const message = messageInput.value.trim();
    if (!message) {
      messageInput.focus();
      options.setStatus("Commit message required");
      return;
    }
    commitButton.disabled = true;
    options.setStatus("Committing roam changes");
    try {
      await api.roamTools.commit(message);
      messageInput.value = "";
      await refresh();
      options.setStatus("Committed");
    } catch (err) {
      options.setStatus(err instanceof Error ? err.message : "Commit failed");
    } finally {
      commitButton.disabled = changes.length === 0;
    }
  }

  async function pull(): Promise<void> {
    pullButton.disabled = true;
    options.setStatus("Pulling roam repo");
    try {
      await api.roamTools.pull();
      await refresh();
      options.setStatus("Pulled");
    } catch (err) {
      options.setStatus(err instanceof Error ? err.message : "Pull failed");
    } finally {
      pullButton.disabled = !status.hasRemote;
    }
  }

  async function push(): Promise<void> {
    pushButton.disabled = true;
    options.setStatus("Pushing roam repo");
    try {
      await api.roamTools.push();
      await refresh();
      options.setStatus("Pushed");
    } catch (err) {
      options.setStatus(err instanceof Error ? err.message : "Push failed");
    } finally {
      pushButton.disabled = !status.hasRemote;
    }
  }

  async function restoreSelectedFile(): Promise<void> {
    const change = selectedChange();
    if (!change?.file) return;
    const isCurrentFile = change.file === options.getCurrentFile();
    options.setStatus("Restoring file");
    try {
      await api.roamTools.discardFileChanges(change.file);
      // Do not let the post-restore git refresh save stale editor content back
      // over a file that was just restored on disk.
      await refresh({ beforeRefresh: !isCurrentFile });
      if (isCurrentFile) await options.openNote(change.file);
      options.setStatus("Restored file");
    } catch (err) {
      options.setStatus(err instanceof Error ? err.message : "Restore failed");
    }
  }

  function deactivate(): void {
    active = false;
    refreshEpoch.cancel();
    diffEpoch.cancel();
    status = {};
    changes = [];
    history = [];
    selected = null;
    messageInput.value = "";
    branchEl.textContent = "No branch";
    summaryEl.textContent = "Not loaded";
    remoteEl.textContent = "No remote";
    countsEl.textContent = "0 files";
    commitButton.disabled = true;
    pullButton.disabled = true;
    pushButton.disabled = true;
    openButton.disabled = true;
    restoreButton.disabled = true;
    changesEl.replaceChildren();
    historyEl.replaceChildren();
    diffTitleEl.textContent = "Diff";
    diffMetaEl.textContent = "No target selected";
    diffEl.replaceChildren();
  }

  root.addEventListener("click", (event) => {
    const target = event.target as Element | null;
    const changeButton = target?.closest<HTMLButtonElement>("[data-git-change]");
    if (changeButton && root.contains(changeButton)) {
      const change = changes.find((item) => (item.file || item.path) === changeButton.dataset.gitChange);
      if (change) void loadChangeDiff(change);
      return;
    }
    const commitButtonEl = target?.closest<HTMLButtonElement>("[data-git-commit]");
    if (commitButtonEl && root.contains(commitButtonEl)) {
      const commitEntry = history.find((item) => item.sha === commitButtonEl.dataset.gitCommit);
      if (commitEntry) void loadCommitDiff(commitEntry);
    }
  });

  refreshButton.addEventListener("click", () => void refresh());
  commitButton.addEventListener("click", () => void commit());
  pullButton.addEventListener("click", () => void pull());
  pushButton.addEventListener("click", () => void push());
  syncButton.addEventListener("click", () => void options.syncRoamDb());
  openButton.addEventListener("click", () => {
    const change = selectedChange();
    if (change?.file) void options.openNote(change.file);
  });
  restoreButton.addEventListener("click", () => void restoreSelectedFile());
  messageInput.addEventListener("keydown", (event) => {
    if (event.key === "Enter" && (event.metaKey || event.ctrlKey)) {
      event.preventDefault();
      void commit();
    }
  });

  return { refresh: () => void refresh(), deactivate };
}
