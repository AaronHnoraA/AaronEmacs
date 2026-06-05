const { contextBridge, ipcRenderer, webFrame } = require("electron");

contextBridge.exposeInMainWorld("AaronnoteDesktop", {
  chooseNotePath(options = {}) {
    return ipcRenderer.invoke("aaronnote:choose-note-path", {
      suggestedPath: String(options.suggestedPath || ""),
      title: String(options.title || ""),
      mode: options.mode === "directory" ? "directory" : options.mode === "openFile" ? "openFile" : "file",
    });
  },
  trashNote(file = "") {
    return ipcRenderer.invoke("aaronnote:trash-note", String(file || ""));
  },
  exportPdf(options = {}) {
    return ipcRenderer.invoke("aaronnote:export-pdf", {
      file: String(options.file || ""),
      name: String(options.name || ""),
    });
  },
  ready() {
    ipcRenderer.send("aaronnote:renderer-ready");
  },
  onOpenFile(handler) {
    if (typeof handler !== "function") return () => {};
    const listener = (_event, file = "") => handler(String(file || ""));
    ipcRenderer.on("aaronnote:open-file", listener);
    return () => ipcRenderer.removeListener("aaronnote:open-file", listener);
  },
});

const invoke = (channel, ...args) => ipcRenderer.invoke(channel, ...args);

contextBridge.exposeInMainWorld("aaronnoteApi", {
  notes: {
    bootstrap: (file = "") => invoke("aaronnote:api:notes:bootstrap", String(file || "")),
    open: (file = "") => invoke("aaronnote:api:notes:open", String(file || "")),
    list: (force = false) => invoke("aaronnote:api:notes:list", force === true),
    save: (body = {}) => invoke("aaronnote:api:notes:save", body),
    createNode: (draft = {}) => invoke("aaronnote:api:notes:create-node", draft),
    deleteNote: (file = "") => invoke("aaronnote:api:notes:delete", String(file || "")),
    createFolder: (path = "") => invoke("aaronnote:api:notes:create-folder", String(path || "")),
    pathSuggestions: (file = "") => invoke("aaronnote:api:notes:path-suggestions", String(file || "")),
    roamSync: (reload = false) => invoke("aaronnote:api:notes:roam-sync", reload === true),
    roamSyncFull: () => invoke("aaronnote:api:notes:roam-sync-full"),
    templates: (force = false) => invoke("aaronnote:api:notes:templates", force === true),
    snippets: () => invoke("aaronnote:api:notes:snippets"),
    todos: (file = "") => invoke("aaronnote:api:notes:todos", String(file || "")),
    metaAdd: (body = {}) => invoke("aaronnote:api:notes:meta-add", body),
  },
  roamTools: {
    renameTag: (body = {}) => invoke("aaronnote:api:roam-tools:rename-tag", body),
    deleteTag: (body = {}) => invoke("aaronnote:api:roam-tools:delete-tag", body),
    tagOverlap: () => invoke("aaronnote:api:roam-tools:tag-overlap"),
    rewritePathRefs: (body = {}) => invoke("aaronnote:api:roam-tools:rewrite-path-refs", body),
    fileHistory: (file = "") => invoke("aaronnote:api:roam-tools:file-history", String(file || "")),
    restoreFileVersion: (body = {}) => invoke("aaronnote:api:roam-tools:restore-file-version", body),
    discardFileChanges: (file = "") => invoke("aaronnote:api:roam-tools:discard-file-changes", String(file || "")),
    repoStatus: () => invoke("aaronnote:api:roam-tools:repo-status"),
    repoHistory: (limit = 30) => invoke("aaronnote:api:roam-tools:repo-history", Number(limit) || 30),
    changes: () => invoke("aaronnote:api:roam-tools:changes"),
    diff: (body = {}) => invoke("aaronnote:api:roam-tools:diff", body),
    commitDiff: (sha = "") => invoke("aaronnote:api:roam-tools:commit-diff", String(sha || "")),
    pull: () => invoke("aaronnote:api:roam-tools:pull"),
    push: () => invoke("aaronnote:api:roam-tools:push"),
    commit: (message = "") => invoke("aaronnote:api:roam-tools:commit", String(message || "")),
  },
  assets: {
    upload: (body = {}) => invoke("aaronnote:api:assets:upload", body),
    storeFromPath: (body = {}) => invoke("aaronnote:api:assets:store-from-path", body),
    renderTikz: (body = {}) => invoke("aaronnote:api:assets:render-tikz", body),
    scanOrphans: () => invoke("aaronnote:api:assets:scan-orphans"),
    trashOrphans: (files = []) => invoke("aaronnote:api:assets:trash-orphans", files),
  },
  session: {
    getRecent: () => invoke("aaronnote:api:session:recent"),
    touchRecent: (file = "", openedAt = Date.now()) => invoke("aaronnote:api:session:touch-recent", String(file || ""), Number(openedAt) || Date.now()),
    getPositions: () => invoke("aaronnote:api:session:positions"),
    savePosition: (position = {}) => invoke("aaronnote:api:session:save-position", position),
  },
  plugins: {
    list: () => invoke("aaronnote:api:plugins:list"),
    getOverrides: () => invoke("aaronnote:api:plugins:overrides"),
    saveOverrides: (overrides = {}) => invoke("aaronnote:api:plugins:save-overrides", overrides),
  },
  fs: {
    rename: (body = {}) => invoke("aaronnote:api:fs:rename", body),
    move: (body = {}) => invoke("aaronnote:api:fs:move", body),
    duplicate: (body = {}) => invoke("aaronnote:api:fs:duplicate", body),
    trash: (body = {}) => invoke("aaronnote:api:fs:trash", body),
  },
  meta: {
    add: (body = {}) => invoke("aaronnote:api:meta:add", body),
    remove: (body = {}) => invoke("aaronnote:api:meta:remove", body),
    tag: (body = {}) => invoke("aaronnote:api:meta:tag", body),
    hideRoam: (body = {}) => invoke("aaronnote:api:meta:hide-roam", body),
    activateRoam: (body = {}) => invoke("aaronnote:api:meta:activate-roam", body),
  },
  shell: {
    showInFolder: (file = "") => invoke("aaronnote:api:shell:show-in-folder", String(file || "")),
    openPath: (file = "") => invoke("aaronnote:api:shell:open-path", String(file || "")),
    openDirectory: (path = "", base = "") => invoke("aaronnote:api:shell:open-directory", {
      path: String(path || ""),
      base: String(base || ""),
    }),
    openDirectoryInKitty: (path = "", base = "") => invoke("aaronnote:api:shell:open-directory-in-kitty", {
      path: String(path || ""),
      base: String(base || ""),
    }),
    showAttachmentMenu: (file = "", base = "", options = {}) => invoke(
      "aaronnote:api:shell:show-attachment-menu",
      String(file || ""),
      String(base || ""),
      options || {},
    ),
    showEditorContextMenu: (options = {}) => invoke("aaronnote:api:shell:show-editor-context-menu", options),
    showLeanEditorMenu: (options = {}) => invoke("aaronnote:api:shell:show-lean-editor-menu", options),
    openLeanLocation: (target = {}) => invoke("aaronnote:api:shell:open-lean-location", target),
  },
  externalEditor: {
    open: (target = {}) => invoke("aaronnote:api:external-editor:open", target),
  },
  jupyter: {
    request: (action = "", body = {}) => invoke("aaronnote:api:jupyter:request", String(action || ""), body || {}),
    scroll: (body = {}) => invoke("aaronnote:api:jupyter:scroll", body || {}),
    kernelStatus: (body = {}) => invoke("aaronnote:api:jupyter:kernel-status", body || {}),
    onStatus: (handler) => {
      if (typeof handler !== "function") return () => {};
      const listener = (_event, data) => handler(data);
      ipcRenderer.on("aaronnote:jupyter:status", listener);
      return () => ipcRenderer.removeListener("aaronnote:jupyter:status", listener);
    },
  },
  proseCheck: {
    run: (body = {}) => invoke("aaronnote:api:prose-check:run", body),
    browserSpellcheck(words = []) {
      const list = Array.isArray(words) ? words.map((word) => String(word || "")).filter(Boolean).slice(0, 2500) : [];
      return list.map((word) => {
        let misspelled = false;
        let suggestions = [];
        try {
          misspelled = webFrame.isWordMisspelled(word);
          suggestions = misspelled ? webFrame.getWordSuggestions(word).slice(0, 8) : [];
        } catch {}
        return { word, misspelled, suggestions };
      });
    },
  },
  copilot: {
    request: (action = "", body = {}) => invoke("aaronnote:api:copilot:request", String(action || ""), body),
    status: () => invoke("aaronnote:api:copilot:request", "status", {}),
    inline: (body = {}) => invoke("aaronnote:api:copilot:request", "inline", body),
    shown: (body = {}) => invoke("aaronnote:api:copilot:request", "shown", body),
    accept: (body = {}) => invoke("aaronnote:api:copilot:request", "accept", body),
    signIn: (body = {}) => invoke("aaronnote:api:copilot:request", "sign-in", body),
    signOut: (body = {}) => invoke("aaronnote:api:copilot:request", "sign-out", body),
    quota: (body = {}) => invoke("aaronnote:api:copilot:request", "quota", body),
    log: (body = {}) => invoke("aaronnote:api:copilot:request", "log", body),
  },
  lean: {
    request: (action = "", body = {}) => invoke("aaronnote:api:lean:request", String(action || ""), body),
    status: () => invoke("aaronnote:api:lean:request", "status", {}),
    openNote: (body = {}) => invoke("aaronnote:api:lean:request", "open-note", body),
    changeNote: (body = {}) => invoke("aaronnote:api:lean:request", "change-note", body),
    closeNote: (body = {}) => invoke("aaronnote:api:lean:request", "close-note", body),
    saveNote: (body = {}) => invoke("aaronnote:api:lean:request", "save-note", body),
    deleteNote: (body = {}) => invoke("aaronnote:api:lean:request", "delete-note", body),
    renameNote: (body = {}) => invoke("aaronnote:api:lean:request", "rename-note", body),
    getGoals: (body = {}) => invoke("aaronnote:api:lean:request", "get-goals", body),
    getTermGoal: (body = {}) => invoke("aaronnote:api:lean:request", "get-term-goal", body),
    getHover: (body = {}) => invoke("aaronnote:api:lean:request", "get-hover", body),
    getCompletions: (body = {}) => invoke("aaronnote:api:lean:request", "get-completions", body),
    rpcCall: (body = {}) => invoke("aaronnote:api:lean:request", "rpc-call", body),
    getDefinition: (body = {}) => invoke("aaronnote:api:lean:request", "get-definition", body),
    getDiagnostics: (body = {}) => invoke("aaronnote:api:lean:request", "get-diagnostics", body),
    lspRequest: (body = {}) => invoke("aaronnote:api:lean:request", "lsp-request", body),
    lspNotify: (body = {}) => invoke("aaronnote:api:lean:request", "lsp-notify", body),
    createRpcSession: (body = {}) => invoke("aaronnote:api:lean:request", "create-rpc-session", body),
    closeRpcSession: (body = {}) => invoke("aaronnote:api:lean:request", "close-rpc-session", body),
    rpcRelease: (body = {}) => invoke("aaronnote:api:lean:request", "rpc-release", body),
    cacheStatus: () => invoke("aaronnote:api:lean:request", "cache-status", {}),
    cacheGet: (body = {}) => invoke("aaronnote:api:lean:request", "cache-get", body),
    onDiagnostics: (handler) => {
      if (typeof handler !== "function") return () => {};
      const listener = (_event, data) => handler(data);
      ipcRenderer.on("aaronnote:lean:diagnostics", listener);
      return () => ipcRenderer.removeListener("aaronnote:lean:diagnostics", listener);
    },
    onProgress: (handler) => {
      if (typeof handler !== "function") return () => {};
      const listener = (_event, data) => handler(data);
      ipcRenderer.on("aaronnote:lean:progress", listener);
      return () => ipcRenderer.removeListener("aaronnote:lean:progress", listener);
    },
    onSemanticTokens: (handler) => {
      if (typeof handler !== "function") return () => {};
      const listener = (_event, data) => handler(data);
      ipcRenderer.on("aaronnote:lean:semantic-tokens", listener);
      return () => ipcRenderer.removeListener("aaronnote:lean:semantic-tokens", listener);
    },
    onStatus: (handler) => {
      if (typeof handler !== "function") return () => {};
      const listener = (_event, data) => handler(data);
      ipcRenderer.on("aaronnote:lean:status", listener);
      return () => ipcRenderer.removeListener("aaronnote:lean:status", listener);
    },
    onNotification: (handler) => {
      if (typeof handler !== "function") return () => {};
      const listener = (_event, data) => handler(data);
      ipcRenderer.on("aaronnote:lean:notification", listener);
      return () => ipcRenderer.removeListener("aaronnote:lean:notification", listener);
    },
    onClientNotification: (handler) => {
      if (typeof handler !== "function") return () => {};
      const listener = (_event, data) => handler(data);
      ipcRenderer.on("aaronnote:lean:client-notification", listener);
      return () => ipcRenderer.removeListener("aaronnote:lean:client-notification", listener);
    },
  },
  roamlookup: {
    request: (action = "", body = {}) => invoke("aaronnote:api:roamlookup:request", String(action || ""), body),
    status: () => invoke("aaronnote:api:roamlookup:request", "status", {}),
    start: (body = {}) => invoke("aaronnote:api:roamlookup:request", "start", body),
    query: (body = {}) => invoke("aaronnote:api:roamlookup:request", "query", body),
    close: (body = {}) => invoke("aaronnote:api:roamlookup:request", "close", body),
  },
});
