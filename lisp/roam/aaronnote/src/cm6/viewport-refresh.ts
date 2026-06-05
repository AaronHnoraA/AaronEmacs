import { StateEffect } from "@codemirror/state";
import type { EditorView, ViewUpdate } from "@codemirror/view";

export const refreshViewportDecorations = StateEffect.define<null>();

const scheduledRefreshFrames = new WeakMap<EditorView, number>();

export function scheduleViewportDecorationRefresh(view: EditorView): void {
  const previous = scheduledRefreshFrames.get(view);
  if (previous) window.cancelAnimationFrame(previous);
  const frame = window.requestAnimationFrame(() => {
    scheduledRefreshFrames.delete(view);
    if (!view.dom.isConnected) return;
    view.requestMeasure();
    const afterMeasure = window.requestAnimationFrame(() => {
      scheduledRefreshFrames.delete(view);
      if (view.dom.isConnected) view.dispatch({ effects: refreshViewportDecorations.of(null) });
    });
    scheduledRefreshFrames.set(view, afterMeasure);
  });
  scheduledRefreshFrames.set(view, frame);
}

export function hasViewportDecorationRefresh(update: ViewUpdate): boolean {
  return update.transactions.some((tr) =>
    tr.effects.some((effect) => effect.is(refreshViewportDecorations)));
}
