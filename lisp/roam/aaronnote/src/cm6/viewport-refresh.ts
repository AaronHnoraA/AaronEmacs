import { StateEffect } from "@codemirror/state";
import type { EditorView, ViewUpdate } from "@codemirror/view";
import { ensureSyntaxTree, forceParsing, syntaxTreeAvailable } from "@codemirror/language";

export const refreshViewportDecorations = StateEffect.define<null>();

// Active rAF handle per view (for cancellation).
const scheduledRefreshFrames = new WeakMap<EditorView, number>();
// Generation counter: incremented on each scheduleViewportDecorationRefresh call so
// an in-flight settle from a prior open can detect it has been superseded.
const viewGenerations = new WeakMap<EditorView, number>();

// Milliseconds to synchronously parse the visible viewport on first refresh.
const VIEWPORT_PARSE_BUDGET_MS = 50;
// Milliseconds per rAF tick spent parsing the rest of the document.
const PER_TICK_PARSE_MS = 30;
// Total wall-clock budget for the background settle; prevents blocking on huge docs.
const SETTLE_DEADLINE_MS = 1500;

function dispatchRefresh(view: EditorView): void {
  if (view.dom.isConnected) view.dispatch({ effects: refreshViewportDecorations.of(null) });
}

function cancelPendingSettle(view: EditorView): void {
  const handle = scheduledRefreshFrames.get(view);
  if (handle !== undefined) {
    window.cancelAnimationFrame(handle);
    scheduledRefreshFrames.delete(view);
  }
}

export function scheduleViewportDecorationRefresh(view: EditorView): void {
  cancelPendingSettle(view);
  // Bump generation so any in-flight settle from a prior open stops itself.
  const gen = (viewGenerations.get(view) ?? 0) + 1;
  viewGenerations.set(view, gen);

  const frame = window.requestAnimationFrame(() => {
    if (viewGenerations.get(view) !== gen) return;
    scheduledRefreshFrames.delete(view);
    if (!view.dom.isConnected) return;
    view.requestMeasure();

    const afterMeasure = window.requestAnimationFrame(() => {
      if (viewGenerations.get(view) !== gen) return;
      scheduledRefreshFrames.delete(view);
      if (!view.dom.isConnected) return;

      // Parse the visible viewport synchronously (budget-capped), then dispatch the
      // first refresh so all decorations render correctly on first paint.
      ensureSyntaxTree(view.state, view.viewport.to, VIEWPORT_PARSE_BUDGET_MS);
      dispatchRefresh(view);

      // If the whole document is already parsed, nothing more to do.
      if (syntaxTreeAvailable(view.state, view.state.doc.length)) return;

      // Bounded background settle: parse the rest of the doc in time-budgeted rAF
      // ticks and dispatch one final refresh when done (or when deadline is exceeded).
      const startTime = Date.now();

      function tick(): void {
        if (viewGenerations.get(view) !== gen) return;
        scheduledRefreshFrames.delete(view);
        if (!view.dom.isConnected) return;

        const fullyParsed = forceParsing(view, view.state.doc.length, PER_TICK_PARSE_MS);
        const overDeadline = Date.now() - startTime >= SETTLE_DEADLINE_MS;

        if (fullyParsed || overDeadline) {
          dispatchRefresh(view);
          return;
        }

        const next = window.requestAnimationFrame(tick);
        scheduledRefreshFrames.set(view, next);
      }

      const first = window.requestAnimationFrame(tick);
      scheduledRefreshFrames.set(view, first);
    });
    scheduledRefreshFrames.set(view, afterMeasure);
  });
  scheduledRefreshFrames.set(view, frame);
}

export function hasViewportDecorationRefresh(update: ViewUpdate): boolean {
  return update.transactions.some((tr) =>
    tr.effects.some((effect) => effect.is(refreshViewportDecorations)));
}
