// lean-infoview-host.ts — official @leanprover/infoview host backed by
// lean-proxy.mjs HTTP/SSE.  Adapted from Aaronnote aaronnote/lean-infoview-host.ts.
import { loadRenderInfoview } from "@leanprover/infoview/loader";
import {
  defaultInfoviewConfig,
  type EditorApi,
  type InfoviewActionKind,
  type InfoviewApi,
} from "@leanprover/infoview-api";
import infoviewModuleUrl from "../node_modules/@leanprover/infoview/dist/index.production.min.js?url";
import infoviewReactUrl from "../node_modules/@leanprover/infoview/dist/react.production.min.js?url";
import infoviewReactDomUrl from "../node_modules/@leanprover/infoview/dist/react-dom.production.min.js?url";
import infoviewReactJsxRuntimeUrl from "../node_modules/@leanprover/infoview/dist/react-jsx-runtime.production.min.js?url";
import { apiLean } from "./api-lean";

export type LeanInfoviewLocation = {
  uri: string;
  line: number;
  character: number;
};

export type LeanOfficialInfoviewHost = {
  setLocation: (location: LeanInfoviewLocation | null) => void;
  refresh: () => void;
  requestAction: (kind: InfoviewActionKind) => void;
  hasContent: () => boolean;
  isReady: () => boolean;
  textContent: () => string;
  destroy: () => void;
};

export type LeanOfficialInfoviewHostOptions = {
  showDocument?: (show: Parameters<EditorApi["showDocument"]>[0]) => Promise<void> | void;
  restartFile?: (uri: string) => Promise<void> | void;
  insertText?: (
    text: string,
    kind: Parameters<EditorApi["insertText"]>[1],
    pos?: Parameters<EditorApi["insertText"]>[2],
  ) => Promise<void> | void;
  applyEdit?: (edit: Parameters<EditorApi["applyEdit"]>[0]) => Promise<void> | void;
  onReady?: () => void;
  onContentChange?: () => void;
};

type LeanNotification = {
  method?: string;
  params?: unknown;
};

function sameLocation(
  a: LeanInfoviewLocation | null,
  b: LeanInfoviewLocation | null,
): boolean {
  return a?.uri === b?.uri && a?.line === b?.line && a?.character === b?.character;
}

function asResult(raw: unknown): unknown {
  const value = raw as { ok?: boolean; result?: unknown; message?: string } | null;
  if (value?.ok === false) throw new Error(value.message || "Lean request failed");
  return value && "result" in value ? value.result : raw;
}

function normalizeInitializeResult(raw: unknown): {
  capabilities: Record<string, unknown>;
  serverInfo: { name: string; version: string };
} {
  const value =
    raw && typeof raw === "object" ? (raw as Record<string, unknown>) : {};
  const serverInfoValue =
    value.serverInfo && typeof value.serverInfo === "object"
      ? (value.serverInfo as Record<string, unknown>)
      : {};
  const version = String(serverInfoValue.version ?? "").trim();
  return {
    ...value,
    capabilities:
      value.capabilities && typeof value.capabilities === "object"
        ? (value.capabilities as Record<string, unknown>)
        : {},
    serverInfo: {
      name: String(serverInfoValue.name ?? "Lean"),
      version: /^\d+\.\d+\.\d+/.test(version) ? version : "4.0.0",
    },
  };
}

function infoviewImports(): Record<string, string> {
  return {
    "@leanprover/infoview": infoviewModuleUrl,
    react: infoviewReactUrl,
    "react-dom": infoviewReactDomUrl,
    "react/jsx-runtime": infoviewReactJsxRuntimeUrl,
  };
}

export function createLeanOfficialInfoviewHost(
  root: HTMLElement,
  options: LeanOfficialInfoviewHostOptions = {},
): LeanOfficialInfoviewHost {
  const serverSubscriptions = new Map<string, number>();
  const clientSubscriptions = new Map<string, number>();
  let infoview: InfoviewApi | null = null;
  let current: LeanInfoviewLocation | null = null;
  let initialized = false;
  let ready = false;
  let destroyed = false;
  let pendingInitializeResult: unknown = null;
  let pendingStoppedReason: { message: string; reason: string } | null = null;

  const editorApi: EditorApi = {
    async saveConfig() {},

    async sendClientRequest(_uri, method, params) {
      return asResult(
        await apiLean.lspRequest({ method, params, timeoutMs: 30_000 }),
      );
    },

    async sendClientNotification(_uri, method, params) {
      await apiLean.lspNotify({ method, params });
    },

    async subscribeServerNotifications(method) {
      serverSubscriptions.set(
        method,
        (serverSubscriptions.get(method) ?? 0) + 1,
      );
    },

    async unsubscribeServerNotifications(method) {
      const next = (serverSubscriptions.get(method) ?? 0) - 1;
      if (next > 0) serverSubscriptions.set(method, next);
      else serverSubscriptions.delete(method);
    },

    async subscribeClientNotifications(method) {
      clientSubscriptions.set(
        method,
        (clientSubscriptions.get(method) ?? 0) + 1,
      );
    },

    async unsubscribeClientNotifications(method) {
      const next = (clientSubscriptions.get(method) ?? 0) - 1;
      if (next > 0) clientSubscriptions.set(method, next);
      else clientSubscriptions.delete(method);
    },

    async copyToClipboard(text) {
      await navigator.clipboard?.writeText(text);
    },

    async insertText(text, kind, pos) {
      await options.insertText?.(text, kind, pos);
    },

    async applyEdit(edit) {
      await options.applyEdit?.(edit);
    },

    async showDocument(show) {
      await options.showDocument?.(show);
    },

    async restartFile(uri) {
      await options.restartFile?.(String(uri ?? ""));
    },

    async createRpcSession(uri) {
      const raw = await apiLean.createRpcSession({ uri });
      const result = raw as {
        ok?: boolean;
        sessionId?: string;
        message?: string;
      } | null;
      if (result?.ok === false || !result?.sessionId)
        throw new Error(result?.message || "Lean RPC session failed");
      return result.sessionId;
    },

    async closeRpcSession(sessionId) {
      await apiLean.closeRpcSession({ sessionId });
    },
  };

  const configureInfoview = (): void => {
    void infoview
      ?.changedInfoviewConfig({
        ...defaultInfoviewConfig,
        expectedTypeVisibility: "Expanded by default",
        showGoalNames: true,
        emphasizeFirstGoal: true,
        showTooltipOnHover: false,
      })
      .catch((err) =>
        console.warn("[lean-infoview] configure failed", err),
      );
  };

  const markContentSoon = (): void => {
    window.setTimeout(() => {
      if (destroyed) return;
      root.classList.toggle(
        "lean-official-infoview--active",
        Boolean(root.textContent?.trim()),
      );
      options.onContentChange?.();
    }, 120);
  };

  function publishLocation(location: LeanInfoviewLocation): Promise<void> {
    if (!infoview) return Promise.resolve();
    const loc = {
      uri: location.uri,
      range: {
        start: { line: location.line, character: location.character },
        end: { line: location.line, character: location.character },
      },
    };
    return (async () => {
      if (!initialized) {
        initialized = true;
        await infoview?.initialize(loc);
      }
      await infoview?.changedCursorLocation(loc);
      markContentSoon();
    })();
  }

  const restartInfoview = (initializeResult: unknown): void => {
    if (!infoview) {
      pendingInitializeResult = initializeResult;
      return;
    }
    pendingInitializeResult = null;
    pendingStoppedReason = null;
    void infoview
      .serverRestarted(normalizeInitializeResult(initializeResult))
      .then(() => {
        if (current)
          void publishLocation(current).catch((err) =>
            console.warn("[lean-infoview] publish location failed", err),
          );
        markContentSoon();
      })
      .catch((err) => {
        root.classList.add("lean-official-infoview--error");
        root.textContent =
          err instanceof Error
            ? err.message
            : "Lean infoview failed to start";
        options.onContentChange?.();
      });
  };

  const stopInfoview = (reason: { message: string; reason: string }): void => {
    if (!infoview) {
      pendingStoppedReason = reason;
      return;
    }
    pendingStoppedReason = null;
    initialized = false;
    void infoview
      .serverStopped(reason)
      .then(markContentSoon)
      .catch((err) =>
        console.warn("[lean-infoview] serverStopped failed", err),
      );
  };

  try {
    loadRenderInfoview(infoviewImports(), [editorApi, root], (loaded) => {
      if (destroyed) return;
      infoview = loaded;
      ready = true;
      root.classList.add("lean-official-infoview--ready");
      configureInfoview();
      if (pendingStoppedReason) stopInfoview(pendingStoppedReason);
      else if (pendingInitializeResult) restartInfoview(pendingInitializeResult);
      else if (current)
        void publishLocation(current).catch((err) =>
          console.warn("[lean-infoview] publish location failed", err),
        );
      options.onReady?.();
      markContentSoon();
    });
  } catch (err) {
    root.textContent =
      err instanceof Error ? err.message : "Lean infoview failed to load";
    root.classList.add("lean-official-infoview--error");
  }

  const unsubServer = apiLean.onNotification((raw) => {
    const data = raw as LeanNotification;
    const method = String(data.method ?? "");
    if (!method || !serverSubscriptions.has(method)) return;
    void infoview?.gotServerNotification(method, data.params);
  });

  const unsubClient = apiLean.onClientNotification((raw) => {
    const data = raw as LeanNotification;
    const method = String(data.method ?? "");
    if (!method || !clientSubscriptions.has(method)) return;
    void infoview?.sentClientNotification(method, data.params);
  });

  void apiLean
    .status()
    .then((raw) => {
      const data = raw as { running?: boolean; initializeResult?: unknown };
      if (data?.running && data.initializeResult)
        restartInfoview(data.initializeResult);
    })
    .catch((err) =>
      console.warn("[lean-infoview] status query failed", err),
    );

  const unsubStatus = apiLean.onStatus((raw) => {
    const data = raw as {
      message?: string;
      kind?: string;
      initializeResult?: unknown;
    };
    if (data.kind === "Normal" || data.kind === "Ready") {
      if (data.initializeResult) restartInfoview(data.initializeResult);
    } else if (data.kind === "Inactive" || data.kind === "Error") {
      stopInfoview({
        message: String(data.message ?? "Lean server stopped"),
        reason: String(data.kind ?? "stopped"),
      });
    }
  });

  return {
    setLocation(location) {
      if (sameLocation(current, location)) return;
      current = location;
      if (location)
        void publishLocation(location).catch((err) =>
          console.warn("[lean-infoview] publish location failed", err),
        );
      else
        void infoview
          ?.changedCursorLocation(undefined)
          .catch((err) =>
            console.warn("[lean-infoview] clear cursor location failed", err),
          );
    },
    refresh() {
      if (current)
        void publishLocation(current).catch((err) =>
          console.warn("[lean-infoview] refresh failed", err),
        );
    },
    requestAction(kind) {
      void infoview
        ?.requestedAction({ kind })
        .then(markContentSoon)
        .catch((err) =>
          console.warn(`[lean-infoview] action ${kind} failed`, err),
        );
    },
    hasContent() {
      return Boolean(root.textContent?.trim());
    },
    isReady() {
      return ready;
    },
    textContent() {
      return root.innerText || root.textContent || "";
    },
    destroy() {
      destroyed = true;
      unsubServer();
      unsubClient();
      unsubStatus();
      root.replaceChildren();
    },
  };
}
