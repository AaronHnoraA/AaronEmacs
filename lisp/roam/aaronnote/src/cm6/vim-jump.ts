import { StateEffect, StateField, type Text } from "@codemirror/state";
import { Decoration, EditorView, type DecorationSet } from "@codemirror/view";

export type VimJumpDirection = 1 | -1;

export type VimJumpCandidate = {
  from: number;
  to: number;
  label: string;
};

export type VimJumpSession = {
  doc: Text;
  candidates: readonly VimJumpCandidate[];
};

const LABELS = "asdfghjklqwertyuiopzxcvbnm";

const setVimJumpHints = StateEffect.define<readonly VimJumpCandidate[]>();

function jumpDecorations(candidates: readonly VimJumpCandidate[]): DecorationSet {
  return Decoration.set(candidates.map((candidate) => Decoration.mark({
    class: "cm-vim-jump-label",
    attributes: {
      "data-vim-jump-label": candidate.label,
    },
  }).range(candidate.from, candidate.to)), true);
}

const vimJumpHintsField = StateField.define<DecorationSet>({
  create: () => Decoration.none,
  update(value, transaction) {
    for (const effect of transaction.effects) {
      if (effect.is(setVimJumpHints)) return jumpDecorations(effect.value);
    }
    if (transaction.docChanged) return Decoration.none;
    return value.map(transaction.changes);
  },
  provide: (field) => EditorView.decorations.from(field),
});

export const vimJumpExtension = vimJumpHintsField;

export function clearVimJump(view: EditorView): void {
  view.dispatch({ effects: setVimJumpHints.of([]) });
}

function candidatePositions(view: EditorView, needle: string): number[] {
  if (!needle) return [];
  const positions: number[] = [];
  for (const range of view.visibleRanges) {
    const text = view.state.doc.sliceString(range.from, range.to);
    let offset = 0;
    while (offset <= text.length - needle.length) {
      const found = text.indexOf(needle, offset);
      if (found < 0) break;
      positions.push(range.from + found);
      offset = found + Math.max(1, needle.length);
      if (positions.length >= LABELS.length) return positions;
    }
  }
  return positions;
}

function orderedPositions(positions: readonly number[], cursor: number, direction: VimJumpDirection): number[] {
  const forward = positions.filter((position) => position > cursor).sort((a, b) => a - b);
  const backward = positions.filter((position) => position < cursor).sort((a, b) => b - a);
  const current = positions.filter((position) => position === cursor);
  return direction > 0
    ? [...forward, ...backward, ...current]
    : [...backward, ...forward, ...current];
}

export function beginVimJump(view: EditorView, needle: string, direction: VimJumpDirection): VimJumpSession {
  const cursor = view.state.selection.main.head;
  const candidates = orderedPositions(candidatePositions(view, needle), cursor, direction)
    .slice(0, LABELS.length)
    .map((from, index) => ({ from, to: Math.min(from + 1, view.state.doc.length), label: LABELS[index]! }));
  view.dispatch({ effects: setVimJumpHints.of(candidates) });
  return { doc: view.state.doc, candidates };
}

export function applyVimJump(view: EditorView, session: VimJumpSession, label: string): boolean {
  if (view.state.doc !== session.doc) {
    clearVimJump(view);
    return false;
  }
  const candidate = session.candidates.find((entry) => entry.label === label.toLowerCase());
  clearVimJump(view);
  if (!candidate) return false;
  view.dispatch({
    selection: { anchor: candidate.from },
    effects: EditorView.scrollIntoView(candidate.from, { y: "nearest" }),
  });
  view.focus();
  return true;
}
