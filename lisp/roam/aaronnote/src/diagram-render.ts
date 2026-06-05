import DOMPurify from "dompurify";
import { safeHref } from "./url-safety.ts";
export { supportedDiagramLang } from "./diagram-langs.ts";

type DiagramCacheValue = { html: string; error?: string };

const MERMAID_CACHE_LIMIT = 96;
const MERMAID_CACHE_BYTES = 8_000_000; // 8 MB
const MAX_MERMAID_SOURCE_CHARS = 80_000;
const mermaidCache = new Map<string, DiagramCacheValue>();
let mermaidCacheBytes = 0;
let renderSeq = 0;

function mermaidEntryBytes(v: DiagramCacheValue): number {
  return (v.html.length + (v.error?.length ?? 0)) * 2;
}

function cachedMermaid(key: string): DiagramCacheValue | undefined {
  const cached = mermaidCache.get(key);
  if (!cached) return undefined;
  mermaidCache.delete(key);
  mermaidCache.set(key, cached);
  return cached;
}

function rememberMermaid(key: string, value: DiagramCacheValue): void {
  if (mermaidCache.has(key)) return;
  mermaidCache.set(key, value);
  mermaidCacheBytes += mermaidEntryBytes(value);
  while (mermaidCache.size > MERMAID_CACHE_LIMIT || mermaidCacheBytes > MERMAID_CACHE_BYTES) {
    const oldest = mermaidCache.keys().next().value as string | undefined;
    if (oldest == null) break;
    const old = mermaidCache.get(oldest)!;
    mermaidCacheBytes -= mermaidEntryBytes(old);
    mermaidCache.delete(oldest);
  }
}

export function clearDiagramRenderCache(): void {
  mermaidCache.clear();
  mermaidCacheBytes = 0;
}

export function diagramRenderCacheSize(): number {
  return mermaidCache.size;
}

export function disposeDiagramRuntime(): void {
  clearDiagramRenderCache();
}

function sanitizeSvg(svg: string): string {
  return DOMPurify.sanitize(svg, {
    USE_PROFILES: { svg: true, svgFilters: true },
    // foreignObject is needed for Mermaid mindmap node labels (div.nodeLabel inside foreignObject).
    // DOMPurify sanitizes the HTML content inside foreignObject using its HTML rules,
    // so scripts/iframes/event-handlers are still stripped.
    ADD_TAGS: ["foreignObject"],
    ADD_ATTR: ["href", "xlink:href", "target", "title", "requiredExtensions", "xmlns", "style"],
  });
}

function sanitizeDiagramLinks(element: HTMLElement): void {
  element.querySelectorAll<SVGElement>("a").forEach((anchor) => {
    const href = anchor.getAttribute("href")
      || anchor.getAttribute("xlink:href")
      || anchor.getAttributeNS("http://www.w3.org/1999/xlink", "href")
      || "";
    if (!href || !safeHref(href)) {
      anchor.removeAttribute("href");
      anchor.removeAttribute("xlink:href");
      anchor.removeAttributeNS("http://www.w3.org/1999/xlink", "href");
      return;
    }
    anchor.setAttribute("href", href);
    anchor.setAttribute("target", "_blank");
    anchor.setAttribute("rel", "noopener noreferrer");
  });
}

const MERMAID_START_RE = /^(?:mindmap|flowchart|graph|sequenceDiagram|classDiagram|stateDiagram(?:-v2)?|erDiagram|journey|gantt|pie|gitGraph|timeline|quadrantChart|sankey-beta|xychart-beta|block-beta|packet-beta)\b/i;
const MINDMAP_LANGS = new Set(["mindmap", "marmind", "markmind"]);
const AARON_MINDMAP_LANGS = new Set(["marmind", "markmind"]);

function diagramLang(info = ""): string {
  return String(info || "").trim().toLowerCase().split(/\s+/, 1)[0] ?? "";
}

function cleanMindmapText(value: string): string {
  return value
    .trim()
    .replace(/^#{1,6}\s+/, "")
    .replace(/^[-*+]\s+/, "")
    .trim();
}

function normalizeMindmapSource(source: string): string {
  const lines = String(source || "").replace(/\t/g, "  ").split(/\r?\n/);
  const meaningful = lines.filter((line) => line.trim());
  if (meaningful.length === 0) return "";
  if (MERMAID_START_RE.test(meaningful[0]!.trim())) return source.trim();

  const normalized = meaningful.map((line, index) => {
    const rawIndent = line.match(/^\s*/)?.[0].length ?? 0;
    const heading = line.match(/^\s*(#{1,6})\s+(.+)$/);
    const bullet = line.match(/^(\s*)([-*+]|\d+[.)])\s+(.+)$/);
    const level = heading
      ? heading[1]!.length - 1
      : bullet
        ? Math.floor((bullet[1]?.length ?? 0) / 2)
        : Math.floor(rawIndent / 2);
    const listText = bullet && /^\d/.test(bullet[2]!)
      ? `${bullet[2]} ${bullet[3]}`
      : bullet?.[3];
    const text = cleanMindmapText(heading?.[2] ?? listText ?? line);
    return `${"  ".repeat(Math.max(1, level + 1))}${text || `Node ${index + 1}`}`;
  });
  return ["mindmap", ...normalized].join("\n");
}

export function normalizeMermaidSource(source: string, info = ""): string {
  return MINDMAP_LANGS.has(diagramLang(info)) ? normalizeMindmapSource(source) : source;
}

export function staticAaronMindmap(info = ""): boolean {
  return AARON_MINDMAP_LANGS.has(diagramLang(info));
}

function aaronMindmapThemeSource(source: string): string {
  return [
    "---",
    "config:",
    "  theme: base",
    "  themeVariables:",
    "    background: '#f7f4ed'",
    "    primaryColor: '#f3ead7'",
    "    primaryBorderColor: '#9b8770'",
    "    primaryTextColor: '#1e1a16'",
    "    secondaryColor: '#e7eee6'",
    "    secondaryBorderColor: '#71816f'",
    "    secondaryTextColor: '#1e1a16'",
    "    tertiaryColor: '#f7f4ed'",
    "    tertiaryBorderColor: '#b9ab98'",
    "    tertiaryTextColor: '#1e1a16'",
    "    lineColor: '#867560'",
    "    textColor: '#1e1a16'",
    "    fontFamily: 'Avenir Next, Inter, system-ui, sans-serif'",
    "---",
    source,
  ].join("\n");
}

function diagramHrefFromAnchor(anchor: SVGElement): string {
  return anchor.getAttribute("href")
    || anchor.getAttribute("xlink:href")
    || anchor.getAttributeNS("http://www.w3.org/1999/xlink", "href")
    || "";
}

function primaryLinkModifier(event: MouseEvent): boolean {
  if (event.metaKey && !event.ctrlKey) return true;
  return !/Mac/.test(navigator.platform) && event.ctrlKey && !event.metaKey;
}

function dispatchDiagramLink(element: HTMLElement, event: MouseEvent, href: string): void {
  if (!safeHref(href)) return;
  event.preventDefault();
  event.stopPropagation();
  const openEvent = new CustomEvent("aaronnote:open-url", {
    bubbles: true,
    cancelable: true,
    detail: { href, newWindow: event.button === 1 || primaryLinkModifier(event) },
  });
  element.dispatchEvent(openEvent);
  if (!openEvent.defaultPrevented) {
    window.open(href, "_blank", "noopener,noreferrer");
  }
}

function selectedDiagramNode(target: EventTarget | null): SVGElement | null {
  if (!(target instanceof SVGElement)) return null;
  return target.closest<SVGElement>("a, g.node, g.mindmap-node, g[class*='node'], g[id]")
    ?? target.closest<SVGElement>("text");
}

export function enableDiagramInteraction(element: HTMLElement): void {
  const svg = element.querySelector<SVGSVGElement>("svg");
  if (!svg) return;

  element.classList.add("cm-diagram-interactive");
  element.style.overflow = "hidden";
  svg.style.maxWidth = "none";
  svg.style.transformOrigin = "0 0";
  sanitizeDiagramLinks(element);

  if (element.dataset.diagramInteractionBound === "true") return;
  element.dataset.diagramInteractionBound = "true";

  let scale = 1;
  let panX = 0;
  let panY = 0;
  let drag: { x: number; y: number; panX: number; panY: number; moved: boolean } | null = null;
  let suppressNextClick = false;

  const currentSvg = (): SVGSVGElement | null => element.querySelector<SVGSVGElement>("svg");
  const applyTransform = (): void => {
    const activeSvg = currentSvg();
    if (!activeSvg) return;
    activeSvg.style.maxWidth = "none";
    activeSvg.style.transformOrigin = "0 0";
    activeSvg.style.transform = `translate(${panX}px, ${panY}px) scale(${scale})`;
  };
  const applyScale = (next: number, originX = element.clientWidth / 2, originY = element.clientHeight / 2): void => {
    const prev = scale;
    scale = Math.min(2.4, Math.max(0.55, next));
    if (prev > 0 && prev !== scale) {
      const factor = scale / prev;
      panX = originX - (originX - panX) * factor;
      panY = originY - (originY - panY) * factor;
    }
    applyTransform();
  };

  element.addEventListener("mousedown", (event) => {
    const target = event.target;
    if (
      target instanceof Element
      && target.closest("svg")
    ) {
      event.preventDefault();
      event.stopPropagation();
    }
  });

  element.addEventListener("pointerdown", (event) => {
    if (event.button !== 0) return;
    const target = event.target;
    if (!(target instanceof Element) || !target.closest("svg")) return;
    event.preventDefault();
    event.stopPropagation();
    drag = { x: event.clientX, y: event.clientY, panX, panY, moved: false };
    if (Number.isFinite(event.pointerId)) element.setPointerCapture?.(event.pointerId);
    element.classList.add("is-panning");
  });
  element.addEventListener("pointermove", (event) => {
    if (!drag) return;
    event.preventDefault();
    event.stopPropagation();
    const dx = event.clientX - drag.x;
    const dy = event.clientY - drag.y;
    if (Math.abs(dx) + Math.abs(dy) > 4) drag.moved = true;
    panX = drag.panX + dx;
    panY = drag.panY + dy;
    applyTransform();
  });
  const endDrag = (): void => {
    suppressNextClick = Boolean(drag?.moved);
    drag = null;
    element.classList.remove("is-panning");
    if (suppressNextClick) window.setTimeout(() => { suppressNextClick = false; }, 0);
  };
  element.addEventListener("pointerup", endDrag);
  element.addEventListener("pointercancel", endDrag);
  element.addEventListener("click", (event) => {
    if (suppressNextClick) {
      event.preventDefault();
      event.stopPropagation();
      suppressNextClick = false;
      return;
    }
    const anchor = (event.target as Element | null)?.closest<SVGElement>("a");
    if (anchor) {
      const href = diagramHrefFromAnchor(anchor);
      if (href) dispatchDiagramLink(element, event, href);
      return;
    }
    const node = selectedDiagramNode(event.target);
    if (!node) return;
    element.querySelectorAll(".cm-diagram-selected").forEach((selected) => {
      selected.classList.remove("cm-diagram-selected");
    });
    node.classList.add("cm-diagram-selected");
  });
  element.addEventListener("dblclick", (event) => {
    event.preventDefault();
    event.stopPropagation();
    scale = 1;
    panX = 0;
    panY = 0;
    applyTransform();
  });
  element.addEventListener("wheel", (event) => {
    if (!event.ctrlKey && !event.metaKey) return;
    event.preventDefault();
    const rect = element.getBoundingClientRect();
    applyScale(
      scale + (event.deltaY < 0 ? 0.12 : -0.12),
      event.clientX - rect.left,
      event.clientY - rect.top,
    );
  }, { passive: false });
}

export function renderMermaidLazy(
  source: string,
  element: HTMLElement,
  onError: (message: string) => void,
  options: { lang?: string; onRender?: () => void } = {},
): void {
  const trimmed = normalizeMermaidSource(source, options.lang).trim();
  const staticMindmap = staticAaronMindmap(options.lang);
  const renderSource = staticMindmap ? aaronMindmapThemeSource(trimmed) : trimmed;
  const key = `mermaid\n${staticMindmap ? "aaron-mindmap" : "interactive"}\n${renderSource}`;
  element.setAttribute("data-diagram-render-key", key);
  element.classList.remove("aaronnote-diagram-error");
  element.classList.toggle("cm-aaron-mindmap", staticMindmap);
  if (!trimmed) {
    element.replaceChildren();
    options.onRender?.();
    return;
  }
  if (trimmed.length > MAX_MERMAID_SOURCE_CHARS) {
    onError("Diagram is too large to render inline");
    options.onRender?.();
    return;
  }

  const cached = cachedMermaid(key);
  if (cached) {
    if (cached.error) {
      onError(cached.error);
      options.onRender?.();
    } else {
      element.innerHTML = cached.html;
      if (!staticMindmap) enableDiagramInteraction(element);
      options.onRender?.();
    }
    return;
  }

  const seq = ++renderSeq;
  element.textContent = "Rendering diagram...";
  void (async () => {
    await new Promise<void>((resolve) => {
      const idle = window.requestIdleCallback ?? ((cb: IdleRequestCallback) => window.setTimeout(() => cb({ didTimeout: false, timeRemaining: () => 0 }), 16));
      idle(() => resolve(), { timeout: 180 });
    });
    if (element.getAttribute("data-diagram-render-key") !== key) return;
    try {
      const mermaid = (await import("mermaid")).default;
      // Aaron mindmap (marmind/markmind): antiscript lets the per-diagram frontmatter
      // ---config--- block take effect (strict blocks it). DOMPurify is our sanitizer anyway.
      // Interactive diagrams keep strict for defence-in-depth.
      if (staticMindmap) {
        mermaid.initialize({ startOnLoad: false, securityLevel: "antiscript" });
      } else {
        mermaid.initialize({ startOnLoad: false, securityLevel: "strict", theme: "default" });
      }
      const id = `aaronnote-mermaid-${Date.now()}-${seq}`;
      const result = await mermaid.render(id, renderSource);
      if (element.getAttribute("data-diagram-render-key") !== key) return;
      const html = sanitizeSvg(result.svg);
      rememberMermaid(key, { html });
      element.innerHTML = html;
      if (!staticMindmap) enableDiagramInteraction(element);
      options.onRender?.();
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      rememberMermaid(key, { html: "", error: message });
      if (element.getAttribute("data-diagram-render-key") !== key) return;
      onError(message);
      options.onRender?.();
    }
  })();
}
