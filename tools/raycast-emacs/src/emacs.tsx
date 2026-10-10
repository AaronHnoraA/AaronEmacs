import { Action, ActionPanel, Cache, closeMainWindow, Color, Form, Icon, List, useNavigation } from "@raycast/api";
import { useCallback, useEffect, useState } from "react";
import { emacs, reportFailure } from "./client";

// The list of actions is Emacs's own whitelist (`my/background-actions`);
// this file names none of them.
type EmacsAction = { id: string; title: string; prompt: string };
type Frame = { id: string; title: string; buffers: string; visible: boolean };

const ACTIONS = "actions";
const cache = new Cache();

function cachedActions(): EmacsAction[] {
  try {
    return JSON.parse(cache.get(ACTIONS) ?? "[]");
  } catch {
    return [];
  }
}

async function run(action: EmacsAction, argument = "") {
  await closeMainWindow({ clearRootSearch: true });
  await emacs("act", action.id, argument).catch(reportFailure);
}

function ArgumentForm({ action }: { action: EmacsAction }) {
  return (
    <Form
      navigationTitle={action.title}
      actions={
        <ActionPanel>
          <Action.SubmitForm title={action.title} onSubmit={(values: { argument: string }) => run(action, values.argument)} />
        </ActionPanel>
      }
    >
      <Form.TextField id="argument" title={action.prompt} autoFocus />
    </Form>
  );
}

function Frames() {
  const [frames, setFrames] = useState<Frame[]>([]);
  const [loading, setLoading] = useState(true);

  const reload = useCallback(async () => {
    setLoading(true);
    try {
      setFrames(JSON.parse(await emacs("frames")));
    } catch (error) {
      setFrames([]);
      await reportFailure(error);
    }
    setLoading(false);
  }, []);

  useEffect(() => {
    reload();
  }, [reload]);

  // Emacs applies frame actions just after the client returns.
  const act = async (action: string, frame: Frame) => {
    await emacs("frame", action, frame.id).catch(reportFailure);
    await new Promise((resolve) => setTimeout(resolve, 250));
    await reload();
  };

  return (
    <List isLoading={loading} navigationTitle="Emacs Frames" searchBarPlaceholder="Emacs frames">
      {frames.map((frame) => (
        <List.Item
          key={frame.id}
          title={frame.title}
          subtitle={frame.buffers === frame.title ? "" : frame.buffers}
          icon={{ source: Icon.Window, tintColor: frame.visible ? Color.Green : Color.SecondaryText }}
          accessories={[{ tag: frame.visible ? "visible" : "hidden" }]}
          actions={
            <ActionPanel>
              <Action
                title="Focus Frame"
                icon={Icon.Eye}
                onAction={async () => {
                  await closeMainWindow();
                  await emacs("frame", "focus", frame.id).catch(reportFailure);
                }}
              />
              <Action
                title="Hide Frame"
                icon={Icon.EyeDisabled}
                shortcut={{ modifiers: ["cmd"], key: "h" }}
                onAction={() => act("hide", frame)}
              />
              <Action
                title="Close Frame"
                icon={Icon.XMarkCircle}
                style={Action.Style.Destructive}
                shortcut={{ modifiers: ["ctrl"], key: "x" }}
                onAction={() => act("close", frame)}
              />
            </ActionPanel>
          }
        />
      ))}
    </List>
  );
}

export default function Command() {
  const { push } = useNavigation();
  const [actions, setActions] = useState<EmacsAction[]>(cachedActions);
  const [running, setRunning] = useState(true);
  const [loading, setLoading] = useState(true);

  // The cached whitelist is on screen at once; the live one replaces it.
  useEffect(() => {
    emacs("actions")
      .then((json) => {
        setActions(JSON.parse(json));
        cache.set(ACTIONS, json);
      })
      .catch(() => setRunning(false))
      .finally(() => setLoading(false));
  }, []);

  return (
    <List isLoading={loading} searchBarPlaceholder="Emacs">
      {!running && (
        <List.Item
          title="Start Emacs in Background"
          icon={Icon.Play}
          actions={
            <ActionPanel>
              <Action
                title="Start Emacs"
                onAction={async () => {
                  await closeMainWindow();
                  await emacs("start", "--background").catch(reportFailure);
                }}
              />
            </ActionPanel>
          }
        />
      )}
      {running &&
        actions.map((action) => (
          <List.Item
            key={action.id}
            title={action.title}
            icon={action.prompt ? Icon.TextInput : Icon.ChevronRight}
            accessories={action.prompt ? [{ text: action.prompt }] : []}
            actions={
              <ActionPanel>
                {action.prompt ? (
                  <Action title={action.title} onAction={() => push(<ArgumentForm action={action} />)} />
                ) : (
                  <Action title={action.title} onAction={() => run(action)} />
                )}
              </ActionPanel>
            }
          />
        ))}
      {running && (
        <List.Item
          title="Frames"
          icon={Icon.AppWindowList}
          accessories={[{ text: "focus, hide, close" }]}
          actions={
            <ActionPanel>
              <Action title="Show Frames" onAction={() => push(<Frames />)} />
            </ActionPanel>
          }
        />
      )}
    </List>
  );
}
