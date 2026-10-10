// The one channel to Emacs: every command shells out to `bin/emacs-app`,
// which owns the emacsclient protocol and its timeout.
import { getPreferenceValues, showToast, Toast } from "@raycast/api";
import { execFile } from "node:child_process";
import { homedir } from "node:os";
import { promisify } from "node:util";

const run = promisify(execFile);

export async function emacs(...args: string[]): Promise<string> {
  const { emacsApp } = getPreferenceValues<{ emacsApp?: string }>();
  const script = (emacsApp || "~/.config/emacs/bin/emacs-app").replace(/^~/, homedir());
  const { stdout } = await run(script, args, { maxBuffer: 8 * 1024 * 1024 });
  return stdout;
}

export async function reportFailure(error: unknown): Promise<void> {
  await showToast({
    style: Toast.Style.Failure,
    title: "Emacs is not answering",
    message: error instanceof Error ? error.message.split("\n")[0] : String(error),
  });
}
