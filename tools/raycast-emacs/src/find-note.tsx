// Find a Noema page from Raycast.  Raycast is the whole front end: each key
// asks the backend, which answers from the Go kernel's search, and the list
// shows that answer unfiltered.  Emacs is involved only to open the chosen page.
import { Action, ActionPanel, Clipboard, closeMainWindow, Icon, List, PopToRootType, showToast, Toast } from "@raycast/api";
import { useEffect, useRef, useState } from "react";
import { emacs, emacsDetached } from "./client";
import { NoemaDown, Page, pageLink, SearchAnswer, searchPages } from "./backend";

const GROUPS: Array<{ title: (answer: SearchAnswer) => string; kinds: string[] }> = [
  { title: () => "Title", kinds: ["exact", "title", "alias", "path"] },
  { title: () => "Similar title", kinds: ["fuzzy"] },
  { title: () => "Text", kinds: ["content"] },
  { title: (answer) => `Text, reading it as “${answer.corrected?.to ?? ""}”`, kinds: ["corrected"] },
  { title: () => "Part of the query", kinds: ["partial"] },
  { title: () => "Recent", kinds: ["listed", ""] },
];

// Emacs opens files through its own whitelist; the action that takes a path
// is looked up there instead of being named here.
async function openInEmacs(file: string) {
  try {
    const actions = JSON.parse(await emacs("actions")) as Array<{ id: string; prompt: string }>;
    const open = actions.find((action) => action.prompt === "Path");
    if (!open) throw new Error("Emacs offers no action that opens a path");
    emacsDetached("act", open.id, file);
    await closeMainWindow({ clearRootSearch: true, popToRootType: PopToRootType.Immediate });
  } catch (error) {
    await showToast({
      style: Toast.Style.Failure,
      title: "Emacs is not answering",
      message: error instanceof Error ? error.message.split("\n")[0] : String(error),
    });
  }
}

const excerpt = (text = "") => text.replace(/\[\[(.*?)\]\]/g, "$1").trim();

export default function Command() {
  const [query, setQuery] = useState("");
  const [answer, setAnswer] = useState<SearchAnswer>({ items: [], total: 0 });
  const [loading, setLoading] = useState(true);
  const [down, setDown] = useState(false);
  const latest = useRef(0);

  useEffect(() => {
    const run = ++latest.current;
    setLoading(true);
    searchPages(query)
      .then((result) => {
        if (run !== latest.current) return;
        setAnswer(result);
        setDown(false);
      })
      .catch((error) => {
        if (run !== latest.current) return;
        setAnswer({ items: [], total: 0 });
        setDown(true);
        if (!(error instanceof NoemaDown)) {
          showToast({ style: Toast.Style.Failure, title: "Noema search failed", message: String(error?.message ?? error) });
        }
      })
      .finally(() => {
        if (run === latest.current) setLoading(false);
      });
  }, [query]);

  const row = (page: Page) => (
    <List.Item
      key={`${page.repositoryId}/${page.repositoryPath}`}
      title={page.title}
      subtitle={excerpt(page.excerpt)}
      icon={Icon.Document}
      accessories={[
        ...(page.via ? [{ tag: page.via }] : []),
        { text: page.namespace, tooltip: `${page.repositoryId}/${page.repositoryPath}` },
      ]}
      actions={
        <ActionPanel>
          <Action title="Open in Emacs" icon={Icon.ArrowRight} onAction={() => openInEmacs(page.file)} />
          <Action
            title="Copy Wiki Link"
            icon={Icon.Link}
            shortcut={{ modifiers: ["cmd"], key: "l" }}
            onAction={async () => {
              await Clipboard.copy(pageLink(page));
              await showToast({ style: Toast.Style.Success, title: "Copied", message: pageLink(page) });
            }}
          />
          <Action.ShowInFinder path={page.file} shortcut={{ modifiers: ["cmd", "shift"], key: "f" }} />
          <Action.CopyToClipboard title="Copy Path" content={page.file} shortcut={{ modifiers: ["cmd", "shift"], key: "c" }} />
        </ActionPanel>
      }
    />
  );

  const shown = new Set<Page>();
  return (
    <List
      isLoading={loading}
      filtering={false}
      throttle
      onSearchTextChange={setQuery}
      searchBarPlaceholder="Title, text, Namespace:title, tag:, path:, “a phrase”"
    >
      {down ? (
        <List.EmptyView icon={Icon.Plug} title="Noema is not running" description="Start Emacs; its Noema host answers this search." />
      ) : (
        <List.EmptyView
          icon={Icon.MagnifyingGlass}
          title={query.trim() ? "No page matches" : "Type to search"}
          description={(answer.explain ?? []).join(" · ")}
        />
      )}
      {GROUPS.map((group) => {
        const pages = answer.items.filter((page) => group.kinds.includes(page.matchKind ?? "") && !shown.has(page));
        pages.forEach((page) => shown.add(page));
        return pages.length ? (
          <List.Section key={group.kinds.join()} title={group.title(answer)} subtitle={String(pages.length)}>
            {pages.map(row)}
          </List.Section>
        ) : null;
      })}
    </List>
  );
}
