import {
  Action,
  ActionPanel,
  Alert,
  Color,
  confirmAlert,
  Form,
  Icon,
  List,
  open,
} from "@raycast/api";
import { useEffect, useState } from "react";
import { readState, run, type State } from "./client";
import FindNote from "./find-note";
import Noema from "./noema";

const app = "/Applications/Emacs.app";
const statusLabels = {
  visible: "Visible",
  background: "Hidden",
  stopped: "Not running",
  busy: "Not answering",
  checking: "Checking…",
  unavailable: "Status unavailable",
};

function OpenFile() {
  return (
    <Form
      navigationTitle="Open File in Emacs"
      actions={
        <ActionPanel>
          <Action.SubmitForm
            title="Open in Emacs"
            onSubmit={async ({ files }: { files: string[] }) => {
              if (files.length) await open(files[0], app);
            }}
          />
        </ActionPanel>
      }
    >
      <Form.FilePicker
        id="files"
        title="File or Folder"
        allowMultipleSelection={false}
        canChooseDirectories
      />
    </Form>
  );
}

export default function Command() {
  const [state, setState] = useState<State>();
  const [failed, setFailed] = useState(false);
  const [refresh, setRefresh] = useState(0);
  useEffect(() => {
    let current = true;
    setFailed(false);
    readState()
      .then((next) => {
        if (current) setState(next);
      })
      .catch(() => {
        if (current) {
          setState(undefined);
          setFailed(true);
        }
      });
    return () => {
      current = false;
    };
  }, [refresh]);

  const status = state?.status ?? (failed ? "unavailable" : "checking");
  const running = status === "visible" || status === "background" || status === "busy";
  const lifecycle = (
    <ActionPanel.Section title="Emacs">
      <Action
        title="Refresh Status"
        icon={Icon.ArrowClockwise}
        shortcut={{ modifiers: ["cmd"], key: "r" }}
        onAction={() => setRefresh((value) => value + 1)}
      />
      {running && (
        <Action
          title="Restart Emacs"
          icon={Icon.RotateClockwise}
          shortcut={{ modifiers: ["cmd", "shift"], key: "r" }}
          onAction={() => run("restart")}
        />
      )}
      {running && <Action title="Quit Emacs" icon={Icon.Power} onAction={() => run("quit")} />}
      {running && (
        <Action
          title="Force Quit Emacs"
          icon={Icon.Bolt}
          style={Action.Style.Destructive}
          onAction={async () => {
            if (
              await confirmAlert({
                title: "Force quit Emacs?",
                message: "Unsaved changes are lost.",
                primaryAction: { title: "Force Quit", style: Alert.ActionStyle.Destructive },
              })
            )
              await run("kill");
          }}
        />
      )}
    </ActionPanel.Section>
  );

  const commands = [
    { title: "New Terminal", icon: Icon.Terminal, command: "terminal" },
    { title: "New Frame", icon: Icon.PlusCircle, command: "new-frame" },
    { title: "Wiki Home", icon: Icon.House, command: "wiki-home" },
  ] as const;
  return (
    <List searchBarPlaceholder="Search Emacs and Noema actions">
      <List.Section title="Emacs">
        <List.Item
          title="Open Emacs"
          subtitle={statusLabels[status]}
          icon={Icon.AppWindow}
          keywords={["show", "unhide", "restore", "显示", "恢复"]}
          actions={
            <ActionPanel>
              <Action.Open title="Open Emacs" target={app} />
              {lifecycle}
            </ActionPanel>
          }
        />
        {commands.map(({ title, icon, command }) => (
          <List.Item
            key={command}
            title={title}
            icon={icon}
            actions={
              <ActionPanel>
                <Action title={title} onAction={() => run(command)} />
                {lifecycle}
              </ActionPanel>
            }
          />
        ))}
        <List.Item
          title="Open File"
          icon={Icon.Document}
          actions={
            <ActionPanel>
              <Action.Push title="Choose File" target={<OpenFile />} />
              {lifecycle}
            </ActionPanel>
          }
        />
        {status === "visible" && (
          <List.Item
            title="Hide Emacs"
            icon={Icon.EyeDisabled}
            actions={
              <ActionPanel>
                <Action title="Hide Emacs" onAction={() => run("hide")} />
                {lifecycle}
              </ActionPanel>
            }
          />
        )}
        {running && (
          <List.Item
            title="Quit Emacs"
            icon={Icon.Power}
            actions={
              <ActionPanel>
                <Action title="Quit Emacs" onAction={() => run("quit")} />
              </ActionPanel>
            }
          />
        )}
      </List.Section>
      <List.Section title="Noema">
        <List.Item
          title="Find Note"
          icon={Icon.MagnifyingGlass}
          actions={
            <ActionPanel>
              <Action.Push title="Find Note" target={<FindNote />} />
              {lifecycle}
            </ActionPanel>
          }
        />
        <List.Item
          title="Noema Maintenance"
          icon={Icon.Gear}
          actions={
            <ActionPanel>
              <Action.Push title="Noema Maintenance" target={<Noema />} />
              {lifecycle}
            </ActionPanel>
          }
        />
      </List.Section>
      <List.Section title="Status">
        <List.Item
          title="Emacs Status"
          subtitle={statusLabels[status]}
          icon={{
            source: Icon.CircleFilled,
            tintColor: running ? Color.Green : Color.SecondaryText,
          }}
          accessories={[
            ...(state?.uptime ? [{ text: state.uptime.trim(), tooltip: "Uptime" }] : []),
            ...(state?.memory !== undefined
              ? [{ text: state.memory + " MB", tooltip: "Resident memory" }]
              : []),
            ...(state?.buffers !== undefined
              ? [{ text: String(state.buffers), tooltip: "Buffers" }]
              : []),
            ...(state?.processes !== undefined
              ? [{ text: String(state.processes), tooltip: "Subprocesses" }]
              : []),
          ]}
          actions={<ActionPanel>{lifecycle}</ActionPanel>}
        />
      </List.Section>
      <List.Section title="Frames">
        {state?.frames?.map((frame) => (
          <List.Item
            key={frame.id}
            title={frame.title}
            subtitle={frame.buffers === frame.title ? "" : frame.buffers}
            icon={Icon.Window}
            accessories={frame.visible ? [] : [{ tag: "Hidden" }]}
            actions={
              <ActionPanel>
                <Action.CopyToClipboard title="Copy Buffer Names" content={frame.buffers} />
                {lifecycle}
              </ActionPanel>
            }
          />
        ))}
      </List.Section>
    </List>
  );
}
