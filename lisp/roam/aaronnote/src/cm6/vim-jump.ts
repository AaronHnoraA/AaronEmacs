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

export const VIM_JUMP_LABELS = "asdfghjklqweruiop";

const setVimJumpHints = StateEffect.define<readonly VimJumpCandidate[]>();

function jumpDecorations(candidates: readonly VimJumpCandidate[]): DecorationSet {
  return Decoration.set(candidates.map((candidate) => Decoration.mark({
    class: candidate.label ? "cm-vim-jump-label" : "cm-vim-jump-preview",
    attributes: candidate.label
      ? { "data-vim-jump-label": candidate.label }
      : undefined,
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

// Generous safety cap so a pathological viewport cannot create an unbounded
// array. The real limit is VIM_JUMP_LABELS.length, applied in beginVimJump AFTER
// orderedPositions sorts by direction/proximity — capping here (before
// ordering) would fill the slots with whichever matches happen to scan first
// (top of viewport) and drop the nearest in-direction targets.
const MAX_SCAN_MATCHES = 4096;

function candidatePositions(view: EditorView, needle: string): number[] {
  if (!needle) return [];
  const positions: number[] = [];
  const foldedNeedle = needle.toLowerCase();
  for (const range of view.visibleRanges) {
    const text = view.state.doc.sliceString(range.from, range.to);
    const foldedText = text.toLowerCase();
    let offset = 0;
    while (offset <= text.length - needle.length) {
      const found = foldedText.indexOf(foldedNeedle, offset);
      if (found < 0) break;
      positions.push(range.from + found);
      offset = found + Math.max(1, needle.length);
      if (positions.length >= MAX_SCAN_MATCHES) return positions;
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

// Order all matches by direction/proximity, THEN keep the nearest LABELS.length.
// Capping before ordering (the previous bug) filled the slots with whatever
// matched first while scanning the viewport top-down, so the nearest in-direction
// targets — often all of them — were dropped.
export function selectJumpCandidates(
  positions: readonly number[],
  cursor: number,
  direction: VimJumpDirection,
  max: number = VIM_JUMP_LABELS.length,
): number[] {
  return orderedPositions(positions, cursor, direction).slice(0, max);
}

export function previewVimJump(view: EditorView, needle: string, direction: VimJumpDirection): number {
  const cursor = view.state.selection.main.head;
  const positions = orderedPositions(candidatePositions(view, needle), cursor, direction);
  view.dispatch({
    effects: setVimJumpHints.of(positions.map((from) => ({
      from,
      to: Math.min(from + needle.length, view.state.doc.length),
      label: "",
    }))),
  });
  return positions.length;
}

export function beginVimJump(view: EditorView, needle: string, direction: VimJumpDirection): VimJumpSession {
  const cursor = view.state.selection.main.head;
  const candidates = selectJumpCandidates(candidatePositions(view, needle), cursor, direction)
    .map((from, index) => ({
      from,
      to: Math.min(from + needle.length, view.state.doc.length),
      label: VIM_JUMP_LABELS[index]!,
    }));
  view.dispatch({ effects: setVimJumpHints.of(candidates) });
  return { doc: view.state.doc, candidates };
}

export function applyVimJump(view: EditorView, session: VimJumpSession, label: string): boolean {
  if (view.state.doc !== session.doc) {
    clearVimJump(view);
    return false;
  }
  const candidate = session.candidates.find((entry) => entry.label === label);
  clearVimJump(view);
  if (!candidate) return false;
  view.dispatch({
    selection: { anchor: candidate.from },
    effects: EditorView.scrollIntoView(candidate.from, { y: "nearest" }),
  });
  view.focus();
  return true;
}
