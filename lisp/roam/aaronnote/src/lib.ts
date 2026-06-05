// Public API for the editor as a library.
//
// Consumers see only `createEditor` and the small `Editor` controller
// it returns. The controller's `view` getter is an opt-in CM6 escape
// hatch for advanced cases.

export { createEditor } from "./editor-api.ts";
export type {
  Editor,
  EditorBlockContext,
  EditorCommand,
  EditorOptions,
  QuickInsertContext,
  QuickInsertItem,
  QuickInsertProvider,
  WritingModeOptions,
} from "./editor-api.ts";
