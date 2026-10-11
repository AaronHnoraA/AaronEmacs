// The one channel to Emacs: every command shells out to `bin/emacs-app`,
// which owns the emacsclient protocol and its timeout.
import { getPreferenceValues, showToast, Toast } from "@raycast/api";
import { execFile, spawn } from "node:child_process";
import { homedir } from "node:os";
import { promisify } from "node:util";

const run = promisify(execFile);

function script(): string {
  const { emacsApp } = getPreferenceValues<{ emacsApp?: string }>();
  return (emacsApp || "~/.config/emacs/bin/emacs-app").replace(/^~/, homedir());
}

export async function emacs(...args: string[]): Promise<string> {
  const { stdout } = await run(script(), args, { maxBuffer: 8 * 1024 * 1024 });
  return stdout;
}

// For a call that outlives the command: leaving Raycast unloads the extension
// and ends whatever it is still waiting for, so the script runs on its own.
export function emacsDetached(...args: string[]): void {
  spawn(script(), args, { detached: true, stdio: "ignore" }).unref();
}

export async function reportFailure(error: unknown): Promise<void> {
  await showToast({
    style: Toast.Style.Failure,
    title: "Emacs is not answering",
    message: error instanceof Error ? error.message.split("\n")[0] : String(error),
  });
}
