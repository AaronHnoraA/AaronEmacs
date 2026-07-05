import { deleteCharBackward, deleteCharForward } from "@codemirror/commands";
import { EditorView } from "@codemirror/view";

type ControlKey = "Escape" | "Delete" | "Backspace";

export type AaronnoteCommandRunner = {
  execute: (id: string, args?: any) => Promise<unknown> | unknown;
  hasCommand?: (id: string) => boolean;
};

export type AaronnoteKeyboardOptions = {
  document?: Document;
  commands?: AaronnoteCommandRunner;
};

const NORMALIZED_EVENT_FLAG = "__aaronnoteNormalizedKeyboardEvent";
const RAW_CONTROL_KEYS = new Map<string, ControlKey>([
  ["\u001b", "Escape"],
  ["\u007f", "Delete"],
  ["\u0008", "Backspace"],
]);

function asElement(target: EventTarget | null): Element | null {
  if (target instanceof Element) return target;
  if (target instanceof Node) return target.parentElement;
  return null;
}

function isCodeMirrorElement(element: Element | null): boolean {
  return Boolean(element?.closest(".cm-editor, .cm-content"));
}

function isNativeTextTarget(element: Element | null): boolean {
  if (!element) return false;
  if (element.closest("input, textarea, select")) return true;
  const editable = element.closest<HTMLElement>("[contenteditable]");
  return Boolean(editable && !isCodeMirrorElement(editable));
}

function keyFromKeyboardEvent(event: KeyboardEvent): ControlKey | null {
  if (event.key === "Escape" || event.key === "Delete" || event.key === "Backspace") {
    return event.key;
  }
  if (event.key === "Esc") return "Escape";
  if (event.key === "Del" || event.key === "DeleteForward") return "Delete";
  return RAW_CONTROL_KEYS.get(event.key) ?? null;
}

function keyFromInputEvent(event: InputEvent): ControlKey | null {
  if (event.inputType === "deleteContentBackward") return "Backspace";
  if (event.inputType === "deleteContentForward") return "Delete";
  return event.data ? RAW_CONTROL_KEYS.get(event.data) ?? null : null;
}

function isRawKeyboardControl(event: KeyboardEvent): boolean {
  return RAW_CONTROL_KEYS.has(event.key) || event.key === "Esc" || event.key === "Del" || event.key === "DeleteForward";
}

function isRawInputControl(event: InputEvent): boolean {
  return Boolean(event.data && RAW_CONTROL_KEYS.has(event.data));
}

function hardStop(event: Event): void {
  event.preventDefault();
  event.stopPropagation();
  event.stopImmediatePropagation();
}

function queryEditorContent(doc: Document, selector: string): HTMLElement | null {
  return doc.querySelector<HTMLElement>(selector);
}

function contentFromEditorElement(element: Element): HTMLElement | null {
  const editor = element.closest(".cm-editor");
  if (editor) return editor.querySelector<HTMLElement>(".cm-content");
  if (element.classList.contains("cm-content")) return element as HTMLElement;
  return null;
}

function activeEditorContent(doc: Document, eventTarget: EventTarget | null = null): HTMLElement | null {
  const target = asElement(eventTarget);
  const targetContent = target ? contentFromEditorElement(target) : null;
  if (targetContent) return targetContent;

  const active = asElement(doc.activeElement);
  const activeContent = active ? contentFromEditorElement(active) : null;
  if (activeContent) return activeContent;

  return queryEditorContent(doc, ".cm-editor.cm-focused .cm-content")
    ?? queryEditorContent(doc, ".jp-mod-current .cm-editor .cm-content")
    ?? queryEditorContent(doc, ".jp-Notebook .jp-Cell.jp-mod-active .cm-content")
    ?? queryEditorContent(doc, ".jp-FileEditor .cm-content")
    ?? queryEditorContent(doc, ".cm-content");
}

function editorViewFromContent(content: HTMLElement): EditorView | null {
  const editor = content.closest<HTMLElement>(".cm-editor") ?? content;
  try {
    return EditorView.findFromDOM(editor) ?? EditorView.findFromDOM(content);
  } catch {
    return null;
  }
}

function dispatchNormalizedKey(content: HTMLElement, key: ControlKey): void {
  const event = new KeyboardEvent("keydown", {
    key,
    code: key,
    bubbles: true,
    cancelable: true,
  });
  Object.defineProperty(event, NORMALIZED_EVENT_FLAG, { value: true });
  content.dispatchEvent(event);
}

function focusEditorContent(content: HTMLElement): void {
  try {
    content.focus({ preventScroll: true });
  } catch {
    content.focus();
  }
}

function runDeleteKey(content: HTMLElement, key: "Delete" | "Backspace"): boolean {
  const view = editorViewFromContent(content);
  if (view) {
    return key === "Backspace" ? deleteCharBackward(view) : deleteCharForward(view);
  }
  dispatchNormalizedKey(content, key);
  return true;
}

function commandExists(commands: AaronnoteCommandRunner, id: string): boolean {
  return typeof commands.hasCommand !== "function" || commands.hasCommand(id);
}

async function runEscape(commands: AaronnoteCommandRunner | undefined, content: HTMLElement | null): Promise<void> {
  if (commands && commandExists(commands, "notebook:enter-command-mode")) {
    try {
      await commands.execute("notebook:enter-command-mode");
      return;
    } catch {
      // Fall through to a normalized DOM Escape for non-notebook contexts.
    }
  }

  if (content) {
    dispatchNormalizedKey(content, "Escape");
    return;
  }

  const active = document.activeElement;
  if (active instanceof HTMLElement) active.blur();
}

function shouldHandleKeyboardEvent(event: KeyboardEvent, doc: Document): ControlKey | null {
  if ((event as unknown as Record<string, unknown>)[NORMALIZED_EVENT_FLAG]) return null;
  if (event.defaultPrevented || event.isComposing) return null;
  if (event.ctrlKey || event.metaKey || event.altKey) return null;

  const key = keyFromKeyboardEvent(event);
  if (!key) return null;

  const target = asElement(event.target);
  if (isNativeTextTarget(target)) return null;

  const targetIsEditor = isCodeMirrorElement(target);
  if (isRawKeyboardControl(event)) return key;
  if (!targetIsEditor && activeEditorContent(doc, event.target)) return key;
  return null;
}

function shouldHandleInputEvent(event: InputEvent, doc: Document): ControlKey | null {
  if (event.defaultPrevented || event.isComposing) return null;

  const key = keyFromInputEvent(event);
  if (!key) return null;

  const target = asElement(event.target);
  if (isNativeTextTarget(target)) return null;

  const targetIsEditor = isCodeMirrorElement(target);
  if (isRawInputControl(event)) return key;
  if (!targetIsEditor && activeEditorContent(doc, event.target)) return key;
  return null;
}

export function handleAaronnoteJupyterKeydown(event: KeyboardEvent, options: AaronnoteKeyboardOptions = {}): boolean {
  const doc = options.document ?? document;
  const key = shouldHandleKeyboardEvent(event, doc);
  if (!key) return false;

  const content = activeEditorContent(doc, event.target);
  hardStop(event);

  if (key === "Escape") {
    void runEscape(options.commands, content);
    return true;
  }

  if (!content) return true;
  focusEditorContent(content);
  runDeleteKey(content, key);
  return true;
}

export function handleAaronnoteJupyterBeforeInput(event: InputEvent, options: AaronnoteKeyboardOptions = {}): boolean {
  const doc = options.document ?? document;
  const key = shouldHandleInputEvent(event, doc);
  if (!key) return false;

  const content = activeEditorContent(doc, event.target);
  hardStop(event);

  if (key === "Escape") {
    void runEscape(options.commands, content);
    return true;
  }

  if (!content) return true;
  focusEditorContent(content);
  runDeleteKey(content, key);
  return true;
}

export function installAaronnoteJupyterKeyboard(options: AaronnoteKeyboardOptions = {}): () => void {
  const doc = options.document ?? document;
  const keydown = (event: KeyboardEvent): void => {
    handleAaronnoteJupyterKeydown(event, options);
  };
  const beforeinput = (event: Event): void => {
    if (event instanceof InputEvent) handleAaronnoteJupyterBeforeInput(event, options);
  };

  doc.addEventListener("keydown", keydown, true);
  doc.addEventListener("beforeinput", beforeinput, true);
  Object.assign(globalThis, {
    __aaronnoteJupyterKeyboard: {
      installed: true,
      version: "0.1.0",
    },
  });

  return () => {
    doc.removeEventListener("keydown", keydown, true);
    doc.removeEventListener("beforeinput", beforeinput, true);
  };
}
