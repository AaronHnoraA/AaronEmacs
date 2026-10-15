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
  const { stdout } = await run(script(), args, { maxBuffer: 8 * 1024 * 1024, timeout: 6500 });
  return stdout;
}

type ActionResult = { status: "pending" | "ok" | "error" | "unknown"; message?: string };

export class EmacsResultUnavailable extends Error {}

// The first call confirms acceptance and returns an ID. Polling is bounded:
// a command waiting for an Emacs prompt remains owned by Emacs after Raycast
// leaves, while quick failures are reported here before the list closes.
export async function emacsAction(...args: string[]): Promise<"ok" | "pending"> {
  const receipt = JSON.parse(await emacs(...args)) as { id?: string };
  if (!receipt?.id || !/^[0-9a-f]{24}$/.test(receipt.id)) {
    throw new Error("Emacs returned an invalid action receipt");
  }
  const deadline = Date.now() + 4500;
  while (true) {
    let reply: string;
    try {
      reply = await emacs("result", receipt.id);
    } catch {
      // A successful receipt followed by an unresponsive Emacs can mean that
      // the accepted action is reading the minibuffer. Do not resend it.
      if (Date.now() >= deadline) return "pending";
      await new Promise((resolve) => setTimeout(resolve, 150));
      continue;
    }
    let result: ActionResult;
    try {
      result = JSON.parse(reply) as ActionResult;
    } catch {
      throw new EmacsResultUnavailable("Emacs returned an invalid action result");
    }
    switch (result?.status) {
      case "ok":
        return "ok";
      case "error":
        throw new Error(result.message || "Emacs action failed");
      case "unknown":
        throw new EmacsResultUnavailable("Emacs restarted or the action result expired");
      case "pending":
        break;
      default:
        throw new EmacsResultUnavailable("Emacs returned an invalid action result");
    }
    if (Date.now() >= deadline) return "pending";
    await new Promise((resolve) => setTimeout(resolve, 150));
  }
}

export async function reportPendingAction(): Promise<void> {
  await showToast({
    style: Toast.Style.Success,
    title: "Request sent to Emacs",
    message: "Result not confirmed; check Emacs before retrying.",
  });
}

// For a call that outlives the command: leaving Raycast unloads the extension
// and ends whatever it is still waiting for, so the script runs on its own.
export function emacsDetached(...args: string[]): Promise<void> {
  return new Promise((resolve, reject) => {
    const child = spawn(script(), args, { detached: true, stdio: "ignore" });
    child.once("spawn", () => {
      child.unref();
      resolve();
    });
    child.once("error", reject);
  });
}

export async function reportFailure(error: unknown): Promise<void> {
  await showToast({
    style: Toast.Style.Failure,
    title: "Emacs is not answering",
    message: error instanceof Error ? error.message.split("\n")[0] : String(error),
  });
}

export async function reportCommandFailure(error: unknown): Promise<void> {
  const failure = error as Error & { stderr?: string };
  await showToast({
    style: Toast.Style.Failure,
    title: error instanceof EmacsResultUnavailable ? "Emacs result unavailable" : "Emacs command failed",
    message: failure?.stderr?.trim().split("\n")[0] || failure?.message?.split("\n")[0] || String(error),
  });
}
