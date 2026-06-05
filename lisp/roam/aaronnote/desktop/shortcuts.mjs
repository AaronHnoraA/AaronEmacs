/**
 * Main-process shortcut chord definitions.
 *
 * Previously scattered across `shouldOwnShortcut` and `historyShortcutCommand`
 * in main.mjs. Centralising here gives a single place to see which keys the
 * Electron process intercepts before the renderer sees them.
 */

/**
 * Keys that the main process owns entirely (preventDefault + dispatch command).
 * Excludes Shift variants — those are handled by checking `input.shift` at call site.
 * All keys are lowercase.
 */
export const OWNED_KEYS = new Set(["j", "l", "r", "w"]);

/**
 * Returns true for Cmd+key combos (no Alt/Ctrl) that the main process should
 * consume before the renderer. Matches the semantics of the former
 * `shouldOwnShortcut(input)` function.
 */
export function shouldOwnShortcut(input) {
  if (input.alt || input.control) return false;
  if (!input.meta) return false;
  const key = input.key.toLowerCase();
  if (key === "j" || key === "w") return !input.shift;
  return key === "l" || key === "r";
}

/**
 * Maps a keyboard input to a history command name, or returns "".
 * Matches the semantics of the former `historyShortcutCommand(input)` function.
 */
export function historyShortcutCommand(input) {
  if (input.alt) return "";
  const key = String(input.key || "").toLowerCase();
  // macOS Ctrl+Z = redo (Emacs convention)
  if (process.platform === "darwin" && input.control && !input.meta && key === "z") return "redo";
  const primary = (input.meta && !input.control) || (input.control && !input.meta);
  if (!primary) return "";
  if (key === "z" && input.shift) return "redo";
  if (key === "z" && !input.shift) return "undo";
  if (key === "y" && !input.shift) return "redo";
  return "";
}
