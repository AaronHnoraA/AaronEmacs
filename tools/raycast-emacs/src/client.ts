import { closeMainWindow, PopToRootType, showToast, Toast } from "@raycast/api";
import { execFile, spawn } from "node:child_process";
import { homedir } from "node:os";
import { join } from "node:path";
import { promisify } from "node:util";

const launcher = join(homedir(), ".config/emacs/bin/emacs-app");
const exec = promisify(execFile);

export type State = {
  status: "visible" | "background" | "busy" | "stopped";
  uptime?: string;
  buffers?: number;
  processes?: number;
  memory?: number;
  frames?: { id: string; title: string; buffers: string; visible: boolean }[];
};

export async function readState(): Promise<State> {
  const { stdout } = await exec(launcher, ["state"], { timeout: 6500, maxBuffer: 8 * 1024 * 1024 });
  return JSON.parse(stdout);
}

export async function run(
  command: "terminal" | "wiki-home" | "new-frame" | "hide" | "quit" | "restart" | "kill",
) {
  try {
    const args = command === "quit" || command === "restart" ? [command, "--app"] : [command];
    await closeMainWindow({ clearRootSearch: true, popToRootType: PopToRootType.Immediate });
    // A save prompt or a cold start can outlive this menu.
    await new Promise<void>((resolve, reject) => {
      const child = spawn(launcher, args, { detached: true, stdio: "ignore" });
      child.once("error", reject);
      child.once("spawn", () => {
        child.unref();
        resolve();
      });
    });
  } catch (error) {
    await showToast({
      style: Toast.Style.Failure,
      title: "Could not run Emacs command",
      message: String(error),
    });
  }
}
