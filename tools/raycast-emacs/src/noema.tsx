// Noema maintenance from Raycast: the index and each repository's Git sync.
// Every row and action is a call to the running host; no Emacs frame opens.
import { Action, ActionPanel, Color, Icon, List, showToast, Toast } from "@raycast/api";
import { useCallback, useEffect, useState } from "react";
import { noema, NoemaDown } from "./backend";

type IndexStatus = {
  ok: boolean;
  generation?: string;
  updatedAt?: string;
  lastMode?: string;
  lastReason?: string;
  lastError?: string;
  message?: string;
  inFlight?: boolean;
  queued?: number;
  dirty?: boolean;
  repositories?: Array<{ repositoryId: string; headSha?: string; scannedAt?: string }>;
};

type SyncState = {
  repositoryId: string;
  phase?: string;
  updatedAt?: string;
  message?: string;
  error?: string;
};

type SyncStatus = { repositories?: SyncState[]; automatic?: boolean; automaticIntervalMinutes?: number };

type Pages = { total: number; source?: string };

const ago = (iso?: string): string => {
  const then = iso ? Date.parse(iso) : NaN;
  if (Number.isNaN(then)) return "never";
  const minutes = Math.max(0, Math.round((Date.now() - then) / 60000));
  if (minutes < 1) return "just now";
  if (minutes < 60) return `${minutes} min ago`;
  if (minutes < 60 * 48) return `${Math.round(minutes / 60)} h ago`;
  return `${Math.round(minutes / 1440)} d ago`;
};

export default function Command() {
  const [index, setIndex] = useState<IndexStatus | null>(null);
  const [sync, setSync] = useState<SyncStatus>({});
  const [pages, setPages] = useState<Pages | null>(null);
  const [loading, setLoading] = useState(true);
  const [down, setDown] = useState(false);

  const reload = useCallback(async () => {
    setLoading(true);
    try {
      const [indexStatus, syncStatus, listed] = await Promise.all([
        noema<IndexStatus>("wiki:index-status"),
        noema<SyncStatus>("wiki:sync-status", [{}]),
        noema<Pages>("knowledge:search", [{ query: "", limit: 1 }]),
      ]);
      setIndex(indexStatus);
      setSync(syncStatus);
      setPages(listed);
      setDown(false);
    } catch (error) {
      setDown(true);
      if (!(error instanceof NoemaDown)) {
        await showToast({ style: Toast.Style.Failure, title: "Noema is not answering", message: String((error as Error)?.message ?? error) });
      }
    }
    setLoading(false);
  }, []);

  useEffect(() => {
    reload();
  }, [reload]);

  // A maintenance call may take a while; the toast says how it ended.
  const perform = async (title: string, channel: string, body: Record<string, unknown>, timeoutMs = 120_000) => {
    const toast = await showToast({ style: Toast.Style.Animated, title });
    try {
      await noema(channel, [body], timeoutMs);
      toast.style = Toast.Style.Success;
      toast.title = `${title}: done`;
    } catch (error) {
      toast.style = Toast.Style.Failure;
      toast.title = `${title}: failed`;
      toast.message = String((error as Error)?.message ?? error).split("\n")[0];
    }
    await reload();
  };

  const reloadAction = (
    <Action title="Reload" icon={Icon.ArrowClockwise} shortcut={{ modifiers: ["cmd"], key: "r" }} onAction={reload} />
  );
  const indexError = index?.lastError || (index && !index.ok ? index.message : "");
  const indexBusy = Boolean(index?.inFlight || index?.queued);
  const syncs = new Map((sync.repositories ?? []).map((state) => [state.repositoryId, state]));
  const repositories = [...new Set([...(index?.repositories ?? []).map((item) => item.repositoryId), ...syncs.keys()])].sort();

  return (
    <List isLoading={loading} searchBarPlaceholder="Noema maintenance">
      {down && <List.EmptyView icon={Icon.Plug} title="Noema is not running" description="Start Emacs; its Noema host answers here." />}
      {!down && index && (
        <List.Section title="Index">
          <List.Item
            title="Wiki index"
            subtitle={indexError || (indexBusy ? `indexing, ${index.queued ?? 0} queued` : `${pages?.total ?? "?"} pages`)}
            keywords={["index", "search", "rebuild", "refresh"]}
            icon={{
              source: Icon.CircleFilled,
              tintColor: indexError ? Color.Red : indexBusy || index.dirty ? Color.Orange : Color.Green,
            }}
            accessories={[
              { text: index.lastMode ? `${index.lastMode} · ${index.lastReason ?? ""}` : "", tooltip: "Last run" },
              { text: ago(index.updatedAt), icon: Icon.Clock, tooltip: index.updatedAt },
              ...(pages?.source && pages.source !== "kernel-wiki" ? [{ tag: { value: "kernel not answering", color: Color.Orange } }] : []),
            ]}
            actions={
              <ActionPanel>
                <Action
                  title="Refresh Index"
                  icon={Icon.ArrowClockwise}
                  onAction={() => perform("Refresh index", "wiki:refresh", { mode: "auto" })}
                />
                <Action
                  title="Rebuild Index"
                  icon={Icon.Hammer}
                  shortcut={{ modifiers: ["cmd", "shift"], key: "r" }}
                  onAction={() => perform("Rebuild index", "wiki:refresh", { mode: "full" }, 300_000)}
                />
                {reloadAction}
              </ActionPanel>
            }
          />
        </List.Section>
      )}
      {!down && repositories.length > 0 && (
        <List.Section
          title="Repositories"
          subtitle={sync.automatic ? `automatic sync every ${sync.automaticIntervalMinutes ?? "?"} min` : "automatic sync off"}
        >
          {repositories.map((repositoryId) => {
            const state = syncs.get(repositoryId);
            const indexed = index?.repositories?.find((item) => item.repositoryId === repositoryId);
            const failed = Boolean(state?.error) || state?.phase === "conflicted" || state?.phase === "error";
            return (
              <List.Item
                key={repositoryId}
                title={repositoryId}
                subtitle={state?.error || state?.message || ""}
                keywords={["git", "sync", ...repositoryId.split("/")]}
                icon={{ source: Icon.Folder, tintColor: failed ? Color.Red : state?.phase && state.phase !== "idle" ? Color.Orange : Color.SecondaryText }}
                accessories={[
                  { tag: state?.phase ?? "idle" },
                  { text: indexed?.headSha?.slice(0, 7) ?? "", tooltip: "Indexed HEAD" },
                  { text: ago(state?.updatedAt), icon: Icon.Clock, tooltip: `Last sync ${state?.updatedAt ?? ""}` },
                ]}
                actions={
                  <ActionPanel>
                    <Action
                      title="Sync Now"
                      icon={Icon.ArrowNe}
                      onAction={() => perform(`Sync ${repositoryId}`, "wiki:sync", { repositoryId })}
                    />
                    <Action
                      title="Checkpoint Local Changes"
                      icon={Icon.SaveDocument}
                      shortcut={{ modifiers: ["cmd"], key: "s" }}
                      onAction={() => perform(`Checkpoint ${repositoryId}`, "wiki:checkpoint", { repositoryId })}
                    />
                    {reloadAction}
                  </ActionPanel>
                }
              />
            );
          })}
        </List.Section>
      )}
    </List>
  );
}
