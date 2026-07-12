import type { EditorState } from "@codemirror/state";
import type { RevealApi } from "reveal.js";

import { tocIndexFromState } from "../src/cm6/toc-index.ts";
import { renderMarkdownHTML } from "../src/render-html.ts";
import type { Editor } from "../src/lib.ts";
import { api } from "./api-client.ts";

export type Slide = {
  from: number;
  to: number;
  cursor: number;
  title: string;
  parentTitle: string;
  vertical: boolean;
};

export type SlideDeckController = {
  sync: (kind: string) => void;
  refresh: () => void;
  toggleView: () => void;
  isSlides: () => boolean;
  isRevealView: () => boolean;
  openMirror: () => Promise<void>;
  destroy: () => void;
};

export function slideRangesFromState(state: EditorState): Slide[] {
  let parentTitle = "";
  const headings = tocIndexFromState(state).headings
    .filter((heading) => heading.source === "markdown"
      && (heading.renderLevel === 1 || (heading.renderLevel === 2 && !heading.omit)))
    .map((heading) => {
      const vertical = heading.renderLevel === 2 && Boolean(parentTitle);
      if (heading.renderLevel === 1) parentTitle = heading.text || "Untitled slide";
      return {
      from: heading.markerFrom ?? heading.pos,
      cursor: heading.markerFrom ?? heading.pos,
      title: heading.text || "Untitled slide",
      parentTitle: vertical ? parentTitle : (heading.text || "Untitled slide"),
      vertical,
    };
    });
  return headings.map((heading, index) => ({
    ...heading,
    to: headings[index + 1]?.from ?? state.doc.length,
  }));
}

function activeSlideIndex(slides: readonly Slide[], position: number): number {
  if (slides.length === 0) return -1;
  const index = slides.findIndex((slide) => position >= slide.from && position < slide.to);
  return index >= 0 ? index : 0;
}

function normalizedKind(kind: string): string {
  return String(kind || "").trim().toLowerCase();
}

const REVEAL_MARKER_RE = /^\s*@@slides\(reveal\)\s*\[\s*\]\s*\r?\n?/im;
const VERTICAL_MARKER_RE = /^\s*@@slides\(vertical\)\s*\[\s*\]\s*\r?\n?/im;
const LEADING_H1_RE = /^\s{0,3}#\s+.*?(?:\r?\n|$)/;
const LEADING_SLIDE_HEADING_RE = /^\s{0,3}#{1,2}\s+.*?(?:\r?\n|$)/;

/**
 * Standard slides use Aaronnote's renderer.  A marker opts one page into raw
 * Reveal HTML: a top-level <section> is unwrapped into Reveal's own section so
 * data-background, data-auto-animate, fragments, and related directives work.
 */
function createRevealSlide(markdown: string, index: number): HTMLElement {
  const section = document.createElement("section");
  section.dataset.aaronnoteSlideIndex = String(index);
  const marker = REVEAL_MARKER_RE.exec(markdown);
  if (!marker) {
    // Editor.getHTML() uses this same renderer.  Keep the editor scope class so
    // org-env, tables, callouts, math and future editor styling are inherited
    // instead of maintaining a parallel slide stylesheet.
    const rendered = document.createElement("div");
    rendered.className = "cm-editor aaronnote-rendered-slide";
    // H1 belongs to the red deck bar. H2 remains in the body and is rendered
    // by Aaronnote itself, preserving the normal heading typography/widgets.
    const body = markdown.replace(VERTICAL_MARKER_RE, "").replace(LEADING_H1_RE, "");
    rendered.innerHTML = renderMarkdownHTML(body, { allowHtml: true });
    section.appendChild(rendered);
    return section;
  }

  const raw = markdown.replace(REVEAL_MARKER_RE, "").replace(LEADING_SLIDE_HEADING_RE, "").trim();
  const holder = document.createElement("div");
  holder.innerHTML = raw;
  const authoredSection = holder.querySelector(":scope > section");
  if (authoredSection) {
    for (const attribute of authoredSection.attributes) section.setAttribute(attribute.name, attribute.value);
    section.innerHTML = authoredSection.innerHTML;
  } else {
    section.innerHTML = raw;
  }
  return section;
}

export function createSlideDeckController(options: {
  root: HTMLElement;
  host: HTMLElement;
  editor: Editor;
  getCurrentFile: () => string;
}): SlideDeckController {
  const { editor } = options;
  const viewer = document.createElement("div");
  viewer.className = "aaronnote-reveal-view";
  viewer.hidden = true;
  viewer.tabIndex = -1;
  const revealRoot = document.createElement("div");
  revealRoot.className = "reveal";
  const revealSlides = document.createElement("div");
  revealSlides.className = "slides";
  revealRoot.appendChild(revealSlides);
  const chrome = document.createElement("div");
  chrome.className = "aaronnote-deck-chrome";
  const unswLogoUrl = new URL("./assets/unsw-logo.svg", import.meta.url).href;
  chrome.innerHTML = `<aside><div class="aaronnote-deck-brand"><img alt="UNSW Sydney" src="${unswLogoUrl}"><strong data-deck-file></strong></div><nav></nav></aside><header><span></span></header>`;
  viewer.append(chrome, revealRoot);
  options.root.appendChild(viewer);

  let enabled = false;
  let view: "reveal" | "edit" = "reveal";
  let slides: Slide[] = [];
  let active = -1;
  let reveal: RevealApi | null = null;
  let renderTimer = 0;
  let mirrorFile = "";
  let mirrorStyle: HTMLStyleElement | null = null;
  let coordinates: Array<{ h: number; v: number }> = [];

  const updateChrome = (): void => {
    const slide = slides[active];
    chrome.querySelector("header span")!.textContent = slide?.parentTitle || slide?.title || "Slides";
  };

  const renderChrome = (): void => {
    const filename = options.getCurrentFile().split(/[\\/]/).at(-1)?.replace(/\.[^.]+$/, "") || "slides";
    chrome.querySelector<HTMLElement>("[data-deck-file]")!.textContent = filename;
    const nav = chrome.querySelector("nav")!;
    nav.replaceChildren(...slides.map((slide, index) => {
      const button = document.createElement("button");
      button.type = "button";
      button.textContent = slide.title;
      button.classList.toggle("is-vertical", slide.vertical);
      button.addEventListener("click", () => {
        const point = coordinates[index];
        if (point) reveal?.slide(point.h, point.v);
      });
      return button;
    }));
    updateChrome();
  };

  const activateEditorSlide = (index: number, focus = false): void => {
    if (slides.length === 0) return;
    active = Math.max(0, Math.min(slides.length - 1, index));
    const slide = slides[active]!;
    editor.setMarkdownSelection(slide.cursor, slide.cursor, { scrollIntoView: false });
    if (focus) {
      editor.revealCursor();
      editor.focus();
    }
  };

  const destroyReveal = (): void => {
    reveal?.destroy();
    reveal = null;
    revealSlides.replaceChildren();
    mirrorStyle?.remove();
    mirrorStyle = null;
  };

  const loadRevealMirror = async (): Promise<{ js: string; css: string; jsFile: string } | null> => {
    const file = options.getCurrentFile();
    if (!file) return null;
    try {
      const mirror = await api.slides.mirror({ file });
      mirrorFile = mirror.jsFile;
      return mirror;
    } catch {
      mirrorFile = "";
      return null;
    }
  };

  const runRevealMirror = async (mirror: { js: string; css: string; jsFile: string } | null): Promise<void> => {
    if (!mirror || !reveal || !enabled || view !== "reveal") return;
    mirrorStyle = document.createElement("style");
    mirrorStyle.dataset.aaronnoteRevealMirror = options.getCurrentFile();
    mirrorStyle.textContent = mirror.css;
    viewer.appendChild(mirrorStyle);
    if (!mirror.js.trim()) return;
    const url = URL.createObjectURL(new Blob([mirror.js], { type: "text/javascript" }));
    try {
      const module = await import(/* @vite-ignore */ url) as { default?: (context: { Reveal: RevealApi; root: HTMLElement; file: string }) => unknown };
      await module.default?.({ Reveal: reveal, root: revealRoot, file: options.getCurrentFile() });
    } finally {
      URL.revokeObjectURL(url);
    }
  };

  const buildReveal = async (): Promise<void> => {
    if (!enabled || view !== "reveal") return;
    const preserved = Math.max(0, active);
    destroyReveal();
    coordinates = [];
    let horizontal = -1;
    let stack: HTMLElement | null = null;
    let lastHorizontalSection: HTMLElement | null = null;
    for (let index = 0; index < slides.length; index += 1) {
      const slide = slides[index]!;
      const markdown = editor.markdownBetween(slide.from, slide.to);
      const vertical = (slide.vertical || VERTICAL_MARKER_RE.test(markdown)) && lastHorizontalSection !== null;
      const section = createRevealSlide(markdown, index);
      section.classList.toggle("aaronnote-vertical-slide", vertical);
      if (!vertical) {
        horizontal += 1;
        coordinates[index] = { h: horizontal, v: 0 };
        revealSlides.appendChild(section);
        lastHorizontalSection = section;
        stack = null;
      } else {
        if (!stack) {
          stack = document.createElement("section");
          revealSlides.replaceChild(stack, lastHorizontalSection!);
          stack.appendChild(lastHorizontalSection!);
        }
        coordinates[index] = { h: horizontal, v: stack.children.length };
        stack.appendChild(section);
      }
    }
    renderChrome();
    if (slides.length === 0) {
      revealSlides.innerHTML = "<section><h2>No slides yet</h2><p>Use an H1 heading to start a slide.</p></section>";
    }
    const [runtime, mirror] = await Promise.all([
      Promise.all([
        import("reveal.js"),
        import("reveal.js/reveal.css"),
      ]),
      loadRevealMirror(),
    ]);
    const [{ default: RevealRuntime }] = runtime;
    if (!enabled || view !== "reveal") return;
    const instance = new RevealRuntime(revealRoot, {
      controls: true,
      controlsLayout: "bottom-right",
      controlsTutorial: false,
      progress: true,
      slideNumber: "c/t",
      hash: false,
      keyboard: true,
      touch: true,
      center: false,
      transition: "slide",
      backgroundTransition: "fade",
      width: 1280,
      height: 720,
      margin: 0.04,
      minScale: 0.1,
      maxScale: 4,
    });
    reveal = await instance.initialize();
    const updateFromReveal = (event?: { indexh?: number; indexv?: number }): void => {
      if (!enabled || view !== "reveal") return;
      const indices = reveal?.getIndices() as { h?: number; v?: number } | undefined;
      const h = Number.isFinite(event?.indexh) ? Number(event?.indexh) : (Number(indices?.h) || 0);
      const v = Number.isFinite(event?.indexv) ? Number(event?.indexv) : (Number(indices?.v) || 0);
      const sourceIndex = coordinates.findIndex((point) => point.h === h && point.v === v);
      const horizontalFallback = coordinates.findIndex((point) => point.h === h);
      const next = sourceIndex >= 0 ? sourceIndex : horizontalFallback;
      if (next >= 0) active = next;
      updateChrome();
    };
    reveal.on("slidechanged", (event) => updateFromReveal(event as { indexh?: number; indexv?: number }));
    const point = coordinates[Math.min(preserved, Math.max(0, slides.length - 1))] ?? { h: 0, v: 0 };
    reveal.slide(point.h, point.v);
    updateFromReveal();
    await runRevealMirror(mirror);
    reveal.sync();
    viewer.focus({ preventScroll: true });
  };

  const scheduleRevealRender = (): void => {
    if (!enabled || view !== "reveal") return;
    window.clearTimeout(renderTimer);
    renderTimer = window.setTimeout(() => { void buildReveal(); }, 140);
  };

  const applyView = (): void => {
    const revealView = enabled && view === "reveal";
    viewer.hidden = !revealView;
    options.host.hidden = revealView;
    options.root.classList.toggle("aaronnote-slides-reveal", revealView);
    options.root.classList.toggle("aaronnote-slides-edit", enabled && !revealView);
    if (revealView) {
      scheduleRevealRender();
    } else if (enabled) {
      if (slides.length > 0) activateEditorSlide(active >= 0 ? active : 0, true);
      else editor.focus();
    }
  };

  const refresh = (): void => {
    if (!enabled) return;
    slides = slideRangesFromState(editor.view.state);
    // Selection drives Reveal only while editing.  Reveal navigation merely
    // records its horizontal index and writes the editor selection once, when
    // the user leaves presentation mode.
    if (view === "edit" || active < 0 || active >= slides.length) {
      active = activeSlideIndex(slides, editor.getMarkdownSelection().from);
    }
    if (view === "reveal") {
      scheduleRevealRender();
      return;
    }
    // The edit side is the ordinary continuous Aaronnote document.  Refreshing
    // must never steal focus or collapse it to the current slide.
  };

  viewer.addEventListener("pointerdown", () => viewer.focus({ preventScroll: true }));

  return {
    sync(kind: string): void {
      const nextEnabled = normalizedKind(kind) === "slides";
      if (enabled !== nextEnabled) {
        enabled = nextEnabled;
        options.root.classList.toggle("aaronnote-slides", enabled);
        if (!enabled) {
          window.clearTimeout(renderTimer);
          destroyReveal();
          viewer.hidden = true;
          options.host.hidden = false;
          options.root.classList.remove("aaronnote-slides-reveal", "aaronnote-slides-edit");
          return;
        }
        view = "reveal";
      }
      refresh();
      applyView();
    },
    refresh,
    toggleView(): void {
      if (!enabled) return;
      view = view === "reveal" ? "edit" : "reveal";
      applyView();
    },
    isSlides: () => enabled,
    isRevealView: () => enabled && view === "reveal",
    async openMirror(): Promise<void> {
      if (!enabled) return;
      if (!mirrorFile) await loadRevealMirror();
      if (mirrorFile) await api.emacs.open({ file: mirrorFile });
    },
    destroy(): void {
      window.clearTimeout(renderTimer);
      destroyReveal();
      viewer.remove();
    },
  };
}
