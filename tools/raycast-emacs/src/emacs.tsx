import {
  Action,
  ActionPanel,
  Alert,
  closeMainWindow,
  Color,
  confirmAlert,
  Form,
  Icon,
  List,
  PopToRootType,
  useNavigation,
} from "@raycast/api";
import { useCallback, useEffect, useState } from "react";
import { emacs, emacsAction, emacsDetached, reportCommandFailure, reportFailure, reportPendingAction } from "./client";
import FindNote from "./find-note";
import Noema from "./noema";

// The list of actions is Emacs's own whitelist (`+background-actions`);
// this file names none of them.
type EmacsAction = { id: string; title: string; prompt: string };
type Frame = { id: string; title: string; buffers: string; visible: boolean };
type State = {
  status: "visible" | "background" | "busy" | "stopped" | "checking" | "unavailable";
  uptime?: string;
  memory?: number;
  buffers?: number;
  processes?: number;
  actions?: EmacsAction[];
  frames?: Frame[];
};

// Every use ends at Raycast's root, so the next launch starts fresh instead of
// on this list with the last search still in it.  Returning to the root
// unloads this command, so the call to Emacs is started first, on its own.
async function leaveWith(...args: string[]) {
  try {
    await emacsDetached(...args);
    await closeMainWindow({ clearRootSearch: true, popToRootType: PopToRootType.Immediate });
  } catch (error) {
    await reportCommandFailure(error);
  }
}

async function run(action: EmacsAction, argument = "") {
  try {
    const result = await emacsAction("act", action.id, argument);
    if (result === "pending") await reportPendingAction();
    await closeMainWindow({ clearRootSearch: true, popToRootType: PopToRootType.Immediate });
  } catch (error) {
    await reportCommandFailure(error);
  }
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

const STATUS = {
  visible: { text: "Visible", color: Color.Green },
  background: { text: "In the background", color: Color.Blue },
  busy: { text: "Not answering: starting, or waiting for an answer", color: Color.Orange },
  stopped: { text: "Not running", color: Color.SecondaryText },
  checking: { text: "Checking Emacs…", color: Color.SecondaryText },
  unavailable: { text: "Could not check Emacs", color: Color.Orange },
};

export default function Command() {
  const { push } = useNavigation();
  const [state, setState] = useState<State>({ status: "checking" });
  const [loading, setLoading] = useState(true);

  const reload = useCallback(async () => {
    setLoading(true);
    try {
      const json = await emacs("state");
      const next = JSON.parse(json) as State;
      if (!["visible", "background", "busy", "stopped"].includes(next?.status)) {
        throw new Error("Emacs returned an invalid state");
      }
      setState(next);
    } catch (error) {
      setState({ status: "unavailable" });
      await reportFailure(error);
    }
    setLoading(false);
  }, []);

  useEffect(() => {
    reload();
  }, [reload]);

  // Stay in the list so several frame operations can be applied in sequence.
  const stay = async (action: string, frame?: Frame) => {
    try {
      const result = await emacsAction("frame", action, frame?.id ?? "");
      if (result === "pending") {
        await reportPendingAction();
        return;
      }
    } catch (error) {
      await reportCommandFailure(error);
      return;
    }
    await reload();
  };

  // Actions that end in a frame taking the keyboard leave Raycast.
  const leave = async (action: string, frame?: Frame) => {
    try {
      const result = await emacsAction("frame", action, frame?.id ?? "");
      if (result === "pending") await reportPendingAction();
      await closeMainWindow({ clearRootSearch: true, popToRootType: PopToRootType.Immediate });
    } catch (error) {
      await reportCommandFailure(error);
    }
  };

  // Starting and quitting take seconds; Raycast is left at once.
  const lifecycle = (...args: string[]) => leaveWith(...args);

  const forceQuit = async () => {
    const confirmed = await confirmAlert({
      title: "Force quit Emacs?",
      message: "Unsaved changes are lost.",
      primaryAction: { title: "Force Quit", style: Alert.ActionStyle.Destructive },
    });
    if (confirmed) await lifecycle("kill");
  };

  const frames = state.frames ?? [];
  const visible = frames.filter((frame) => frame.visible).length;
  const answering = state.status === "visible" || state.status === "background";

  const frameActions = (
    <ActionPanel.Section title="All Frames">
      <Action
        title="New Frame"
        icon={Icon.PlusCircle}
        shortcut={{ modifiers: ["cmd"], key: "n" }}
        onAction={() => leave("new")}
      />
      <Action
        title="Show All Frames"
        icon={Icon.Eye}
        shortcut={{ modifiers: ["cmd", "shift"], key: "s" }}
        onAction={() => leave("show-all")}
      />
      <Action
        title="Hide All Frames"
        icon={Icon.EyeDisabled}
        shortcut={{ modifiers: ["cmd", "shift"], key: "h" }}
        onAction={() => stay("hide-all")}
      />
    </ActionPanel.Section>
  );

  const emacsActions = (
    <ActionPanel.Section title="Emacs">
      <Action title="Reload" icon={Icon.ArrowClockwise} shortcut={{ modifiers: ["cmd"], key: "r" }} onAction={reload} />
      {(answering || state.status === "busy") && (
        <Action
          title="Restart Emacs"
          icon={Icon.RotateClockwise}
          shortcut={{ modifiers: ["cmd", "shift"], key: "r" }}
          onAction={() => lifecycle("restart")}
        />
      )}
      {(answering || state.status === "busy") && (
        <Action title="Force Quit Emacs" icon={Icon.Bolt} style={Action.Style.Destructive} onAction={forceQuit} />
      )}
    </ActionPanel.Section>
  );

  const health = [
    state.uptime && { icon: Icon.Clock, text: state.uptime.trim(), tooltip: "Uptime" },
    state.memory !== undefined && { icon: Icon.MemoryChip, text: `${state.memory} MB`, tooltip: "Resident memory" },
    state.buffers !== undefined && { icon: Icon.Document, text: String(state.buffers), tooltip: "Buffers" },
    state.processes !== undefined && { icon: Icon.Terminal, text: String(state.processes), tooltip: "Subprocesses" },
  ].filter(Boolean) as List.Item.Accessory[];

  return (
    <List isLoading={loading} searchBarPlaceholder="Emacs actions and frames">
      {answering && (
        <List.Section title="Actions">
          {(state.actions ?? []).map((action) => (
            <List.Item
              key={`action-${action.id}`}
              title={action.title}
              icon={action.prompt ? Icon.TextInput : Icon.Bolt}
              accessories={action.prompt ? [{ text: action.prompt }] : []}
              actions={
                <ActionPanel>
                  {action.prompt ? (
                    <Action title={action.title} onAction={() => push(<ArgumentForm action={action} />)} />
                  ) : (
                    <Action title={action.title} onAction={() => run(action)} />
                  )}
                  {emacsActions}
                </ActionPanel>
              }
            />
          ))}
        </List.Section>
      )}
      {/* Noema is answered by its own backend: these stay in Raycast and open
          no Emacs frame. */}
      <List.Section title="Noema">
        <List.Item
          title="Find Note"
          subtitle="Search pages here, without showing Emacs"
          keywords={["noema", "wiki", "page", "search", "roam"]}
          icon={Icon.MagnifyingGlass}
          actions={
            <ActionPanel>
              <Action.Push title="Find Note" icon={Icon.MagnifyingGlass} target={<FindNote />} />
              {emacsActions}
            </ActionPanel>
          }
        />
        <List.Item
          title="Noema Maintenance"
          subtitle="Index and repository sync"
          keywords={["noema", "index", "sync", "git", "rebuild"]}
          icon={Icon.Gear}
          actions={
            <ActionPanel>
              <Action.Push title="Noema Maintenance" icon={Icon.Gear} target={<Noema />} />
              {emacsActions}
            </ActionPanel>
          }
        />
      </List.Section>
      {answering && (
        <List.Section title="Frames" subtitle={`${visible} visible · ${frames.length - visible} hidden`}>
          {frames.map((frame) => (
            <List.Item
              key={`frame-${frame.id}`}
              title={frame.title}
              subtitle={frame.buffers === frame.title ? "" : frame.buffers}
              keywords={["frame", ...frame.buffers.split(", ")]}
              icon={{ source: Icon.Window, tintColor: frame.visible ? Color.Green : Color.SecondaryText }}
              accessories={frame.visible ? [] : [{ tag: "hidden" }]}
              actions={
                <ActionPanel>
                  <ActionPanel.Section title={frame.title}>
                    <Action
                      title={frame.visible ? "Focus Frame" : "Show Frame"}
                      icon={Icon.Eye}
                      onAction={() => leave("focus", frame)}
                    />
                    {frame.visible && (
                      <Action
                        title="Hide Frame"
                        icon={Icon.EyeDisabled}
                        shortcut={{ modifiers: ["cmd"], key: "h" }}
                        onAction={() => stay("hide", frame)}
                      />
                    )}
                    <Action
                      title="Hide Other Frames"
                      icon={Icon.Minimize}
                      shortcut={{ modifiers: ["cmd"], key: "o" }}
                      onAction={() => leave("hide-others", frame)}
                    />
                    <Action
                      title="Close Frame"
                      icon={Icon.XMarkCircle}
                      style={Action.Style.Destructive}
                      shortcut={{ modifiers: ["ctrl"], key: "x" }}
                      onAction={() => stay("close", frame)}
                    />
                    <Action
                      title="Close Other Frames"
                      icon={Icon.Trash}
                      style={Action.Style.Destructive}
                      shortcut={{ modifiers: ["ctrl", "shift"], key: "x" }}
                      onAction={() => stay("close-others", frame)}
                    />
                  </ActionPanel.Section>
                  {frameActions}
                  {emacsActions}
                </ActionPanel>
              }
            />
          ))}
        </List.Section>
      )}
      <List.Section title="Emacs">
        <List.Item
          title="Emacs"
          subtitle={STATUS[state.status].text}
          keywords={["status", "restart", "start", "quit"]}
          icon={{ source: Icon.CircleFilled, tintColor: STATUS[state.status].color }}
          accessories={answering ? health : []}
          actions={
            <ActionPanel>
              {state.status === "stopped" && (
                <Action
                  title="Start Emacs"
                  icon={Icon.Play}
                  onAction={() => lifecycle("start", "--background")}
                />
              )}
              {state.status === "stopped" && (
                <Action title="Start Emacs and Show It" icon={Icon.Eye} onAction={() => lifecycle("start")} />
              )}
              {(state.status === "busy" || state.status === "unavailable") && (
                <Action
                  title="Show or Start Emacs"
                  icon={Icon.Eye}
                  onAction={() => leaveWith("show")}
                />
              )}
              {answering && (
                <Action
                  title={state.status === "visible" ? "Hide Emacs" : "Show Emacs"}
                  icon={state.status === "visible" ? Icon.EyeDisabled : Icon.Eye}
                  onAction={() => leaveWith("toggle")}
                />
              )}
              {answering && frameActions}
              {emacsActions}
            </ActionPanel>
          }
        />
      </List.Section>
    </List>
  );
}
