import "../src/styles/widgets.css";
import "../src/styles/theme-typora.css";
import "./style.css";

import { StateEffect, StateField } from "@codemirror/state";
import { Decoration, EditorView, type DecorationSet } from "@codemirror/view";
import { createEditor, type Editor } from "../src/lib.ts";

type EmacsEvent = {
  type: "goto";
  line: number;
  col: number;
};

declare global {
  interface Window {
    __aaronoteEditor?: Editor;
    aaronnoteEmacsBridge?: {
      sendEvent: (event: EmacsEvent) => Promise<Response>;
    };
  }
}

const revealLineEffect = StateEffect.define<number | null>();
const revealLineField = StateField.define<DecorationSet>({
  create: () => Decoration.none,
  update(decorations, transaction) {
    let next = decorations.map(transaction.changes);
    for (const effect of transaction.effects) {
      if (!effect.is(revealLineEffect)) continue;
      if (effect.value == null) return Decoration.none;
      const pos = Math.max(0, Math.min(effect.value, transaction.newDoc.length));
      const line = transaction.newDoc.lineAt(pos);
      next = Decoration.set([
        Decoration.line({ class: "aaronnote-reveal-line" }).range(line.from),
      ]);
    }
    return next;
  },
  provide: (field) => EditorView.decorations.from(field),
});

const root = document.querySelector<HTMLDivElement>("#app");
if (!root) throw new Error("Missing preview app root");

root.innerHTML = `
  <main class="aaronnote-shell aaronnote-preview-shell">
    <section class="aaronnote-body">
      <section class="aaronnote-editor" id="editor"></section>
    </section>
  </main>
`;

const host = root.querySelector<HTMLElement>("#editor");
if (!host) throw new Error("Missing preview editor host");

const editor = createEditor(host, {
  initialContent: "",
  readOnly: true,
});
editor.view.dispatch({
  effects: StateEffect.appendConfig.of(revealLineField),
});
window.__aaronoteEditor = editor;

let revealTimer = 0;

function revealSourcePosition(lineNumber: number, column: number): void {
  const doc = editor.view.state.doc;
  const safeLineNumber = Math.max(1, Math.min(Math.trunc(lineNumber) || 1, doc.lines));
  const line = doc.line(safeLineNumber);
  const pos = Math.max(line.from, Math.min(line.from + (Math.trunc(column) || 0), line.to));
  editor.view.dispatch({
    selection: { anchor: pos },
    effects: [
      EditorView.scrollIntoView(pos, { y: "center" }),
      revealLineEffect.of(line.from),
    ],
  });
  window.clearTimeout(revealTimer);
  revealTimer = window.setTimeout(() => {
    if (editor.view.dom.isConnected) {
      editor.view.dispatch({ effects: revealLineEffect.of(null) });
    }
  }, 1400);
}

window.addEventListener("aaronnote:reveal", (event) => {
  const detail = (event as CustomEvent<{ line?: unknown; col?: unknown }>).detail;
  revealSourcePosition(Number(detail?.line), Number(detail?.col));
});

host.addEventListener("mousedown", (event) => {
  if (event.button !== 0) return;
  const pos = editor.view.posAtCoords({ x: event.clientX, y: event.clientY });
  if (pos == null) return;
  event.preventDefault();
  event.stopImmediatePropagation();
  const line = editor.view.state.doc.lineAt(pos);
  void window.aaronnoteEmacsBridge?.sendEvent({
    type: "goto",
    line: line.number,
    col: pos - line.from,
  }).catch((err) => console.error("[emacs-preview] goto failed", err));
}, { capture: true });

for (const eventName of ["beforeinput", "paste", "drop", "keydown", "click", "contextmenu"]) {
  host.addEventListener(eventName, (event) => {
    event.preventDefault();
    event.stopImmediatePropagation();
  }, { capture: true });
}
