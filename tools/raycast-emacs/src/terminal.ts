import { emacsDetached, reportCommandFailure } from "./client";

// Startup can outlive a Raycast command. The launcher owns waiting for Emacs
// and creates a fresh Portal frame even when Emacs is already running.
export default async function Command() {
  try {
    await emacsDetached("terminal");
  } catch (error) {
    await reportCommandFailure(error);
  }
}
