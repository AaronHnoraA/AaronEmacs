import type { NoteSummary } from "./types.ts";
import { CoalescedTimer } from "../src/coalesced-timer.ts";

type OpenNoteOptions = { newWindow?: boolean };

type LocalGraphPanelOptions = {
  root: HTMLElement;
  toggleButton: HTMLButtonElement;
  depthInput: HTMLInputElement;
  depthLabel: HTMLElement;
  refsInput: HTMLInputElement;
  backlinksInput: HTMLInputElement;
  tagsInput: HTMLInputElement;
  canvas: HTMLElement;
  status: HTMLElement;
  getNotes: () => NoteSummary[];
  getCurrentNote: () => NoteSummary | undefined;
  getMarkdown: () => string;
  resolveNoteRef: (ref: string) => NoteSummary | undefined;
  openNote: (note: NoteSummary, options?: OpenNoteOptions) => void;
  openTag: (tag: string) => void;
};

export type LocalGraphPanel = {
  toggle: () => void;
  collapse: () => void;
  update: (force?: boolean) => void;
  invalidate: () => void;
};

type LocalNode = {
  id: string;
  label: string;
  type: "current" | "note" | "tag";
  depth: number;
  note?: NoteSummary;
  tag?: string;
  x: number;
  y: number;
  vx: number;
  vy: number;
  fx?: number;
  fy?: number;
};

type LocalLink = {
  id: string;
  source: string;
  target: string;
  type: "ref" | "backlink" | "tag";
};

const MAX_LOCAL_GRAPH_NODES = 72;
const MAX_LOCAL_GRAPH_LINKS = 160;

function noteKey(note: NoteSummary | undefined): string {
  return note?.key || note?.id || note?.path || note?.file || note?.title || "";
}

function displayTitle(note: NoteSummary): string {
  return note.title || note.id || note.path || note.file || "Untitled";
}

function normalizeLookup(value: string): string {
  return String(value || "")
    .trim()
    .replace(/^roam:\/\//i, "")
    .replace(/^#/, "")
    .replace(/\\/g, "/")
    .replace(/\.md$/i, "")
    .toLowerCase();
}

function safeDecode(value: string): string {
  try {
    return decodeURIComponent(value);
  } catch {
    return value;
  }
}

function unique<T>(items: T[], key: (item: T) => string): T[] {
  const seen = new Set<string>();
  const out: T[] = [];
  for (const item of items) {
    const value = key(item);
    if (!value || seen.has(value)) continue;
    seen.add(value);
    out.push(item);
  }
  return out;
}

function noteTags(note: NoteSummary | undefined): string[] {
  return unique([...(note?.tags ?? []), ...(note?.inlineTags ?? [])]
    .map((tag) => String(tag || "").trim().replace(/^#/, ""))
    .filter(Boolean), (tag) => tag.toLowerCase());
}

function markdownRefs(markdown: string): string[] {
  const refs: string[] = [];
  const text = String(markdown || "");
  const wiki = /\[\[([^\]\n]+)\]\]/g;
  let match: RegExpExecArray | null;
  while ((match = wiki.exec(text)) !== null) {
    const ref = String(match[1] || "").split("|", 1)[0]!.split("#", 1)[0]!.trim();
    if (ref) refs.push(ref);
  }
  const roam = /roam:\/\/([^\s)\]'"<>]+)/gi;
  while ((match = roam.exec(text)) !== null) {
    const ref = safeDecode(String(match[1] || "").split(/[?#@]/, 1)[0] || "").trim();
    if (ref) refs.push(ref);
  }
  const markdownLink = /\[[^\]\n]*\]\(([^)\s]+(?:\.md|\.markdown|\.typ)(?:#[^)]+)?)\)/gi;
  while ((match = markdownLink.exec(text)) !== null) {
    const ref = String(match[1] || "").split("#", 1)[0]!.trim();
    if (ref) refs.push(ref);
  }
  return unique(refs, normalizeLookup);
}

function markdownTags(markdown: string): string[] {
  const tags: string[] = [];
  const text = String(markdown || "");
  const inline = /@@tag\[([^\]\n]+)\]/g;
  let match: RegExpExecArray | null;
  while ((match = inline.exec(text)) !== null) {
    const tag = String(match[1] || "").trim().replace(/^#/, "");
    if (tag) tags.push(tag);
  }
  const hash = /(^|[\s([{])#([\p{L}\p{N}_/-]{2,})/gu;
  while ((match = hash.exec(text)) !== null) {
    const tag = String(match[2] || "").trim();
    if (tag) tags.push(tag);
  }
  return unique(tags, (tag) => tag.toLowerCase());
}

function labelFit(label: string, max = 22): string {
  const text = String(label || "").trim();
  if (text.length <= max) return text;
  return `${text.slice(0, Math.max(4, max - 1))}…`;
}

function seededPosition(index: number, depth: number, width: number, height: number): { x: number; y: number } {
  if (depth === 0) return { x: width / 2, y: height / 2 };
  const radius = Math.min(width, height) * (depth === 1 ? 0.27 : 0.41);
  const angle = index * 2.399963229728653 + depth * 0.7;
  return {
    x: width / 2 + Math.cos(angle) * radius,
    y: height / 2 + Math.sin(angle) * radius,
  };
}

function nodeIdentifiers(note: NoteSummary): string[] {
  return [
    note.key,
    note.id,
    note.title,
    note.file,
    note.path,
    ...(note.aliases ?? []),
  ].map((value) => String(value || "")).filter(Boolean);
}

function resolveRef(ref: string, notesByLookup: Map<string, NoteSummary>, resolveNoteRef: (ref: string) => NoteSummary | undefined): NoteSummary | undefined {
  return resolveNoteRef(ref) || notesByLookup.get(normalizeLookup(ref));
}

function buildLookup(notes: NoteSummary[]): Map<string, NoteSummary> {
  const lookup = new Map<string, NoteSummary>();
  for (const note of notes) {
    for (const id of nodeIdentifiers(note)) {
      const key = normalizeLookup(id);
      if (key && !lookup.has(key)) lookup.set(key, note);
    }
  }
  return lookup;
}

export function createLocalGraphPanel(options: LocalGraphPanelOptions): LocalGraphPanel {
  let renderKey = "";
  let animationFrame = 0;
  const resizeTimer = new CoalescedTimer(40);
  let expandedOnce = false;

  function isCollapsed(): boolean {
    return options.root.classList.contains("is-collapsed");
  }

  function settings(): { depth: number; refs: boolean; backlinks: boolean; tags: boolean } {
    const depth = Math.max(1, Math.min(2, Number(options.depthInput.value) || 1));
    return {
      depth,
      refs: options.refsInput.checked,
      backlinks: options.backlinksInput.checked,
      tags: options.tagsInput.checked,
    };
  }

  function clearGraph(): void {
    window.cancelAnimationFrame(animationFrame);
    resizeTimer.cancel();
    animationFrame = 0;
    options.canvas.replaceChildren();
  }

  function noteSignature(note: NoteSummary | undefined): string {
    if (!note) return "";
    return [
      noteKey(note),
      displayTitle(note),
      (note.refs ?? []).join(","),
      (note.backlinks ?? []).join(","),
      noteTags(note).join(","),
    ].join("\t");
  }

  function dataSignature(): string {
    const config = settings();
    const current = options.getCurrentNote();
    return [
      noteKey(current),
      config.depth,
      config.refs ? "refs" : "",
      config.backlinks ? "backlinks" : "",
      config.tags ? "tags" : "",
      noteSignature(current),
      options.getNotes().map(noteSignature).join("\n"),
      expandedOnce ? options.getMarkdown().length : "",
    ].join("\n");
  }

  function buildGraph(width: number, height: number): { nodes: LocalNode[]; links: LocalLink[]; truncated: boolean } {
    const config = settings();
    const current = options.getCurrentNote();
    if (!current) return { nodes: [], links: [], truncated: false };

    const notes = unique([current, ...options.getNotes()], (note) => noteKey(note));
    const byLookup = buildLookup(notes);
    const currentKey = noteKey(current);
    const currentTags = unique([...noteTags(current), ...markdownTags(options.getMarkdown())], (tag) => tag.toLowerCase());
    const outgoing = new Map<string, NoteSummary[]>();
    const incoming = new Map<string, NoteSummary[]>();
    const tagsByNote = new Map<string, string[]>();
    const markdownOut = markdownRefs(options.getMarkdown());

    for (const note of notes) {
      const key = noteKey(note);
      if (!key) continue;
      const refs = key === currentKey ? unique([...(note.refs ?? []), ...markdownOut], normalizeLookup) : note.refs ?? [];
      const resolved = unique(refs
        .map((ref) => resolveRef(ref, byLookup, options.resolveNoteRef))
        .filter((target): target is NoteSummary => Boolean(target && noteKey(target) && noteKey(target) !== key)), noteKey);
      outgoing.set(key, resolved);
      tagsByNote.set(key, key === currentKey ? currentTags : noteTags(note));
    }

    for (const note of notes) {
      const key = noteKey(note);
      if (!key) continue;
      for (const target of outgoing.get(key) ?? []) {
        const targetKey = noteKey(target);
        if (!targetKey) continue;
        const list = incoming.get(targetKey) ?? [];
        list.push(note);
        incoming.set(targetKey, unique(list, noteKey));
      }
      for (const ref of note.backlinks ?? []) {
        const source = resolveRef(ref, byLookup, options.resolveNoteRef);
        if (!source || noteKey(source) === key) continue;
        const list = incoming.get(key) ?? [];
        list.push(source);
        incoming.set(key, unique(list, noteKey));
      }
    }

    const nodes = new Map<string, LocalNode>();
    const links = new Map<string, LocalLink>();
    let index = 0;
    let truncated = false;

    function addNote(note: NoteSummary, depth: number, type: "current" | "note" = "note"): boolean {
      const id = noteKey(note);
      if (!id) return false;
      const existing = nodes.get(id);
      if (existing) {
        existing.depth = Math.min(existing.depth, depth);
        if (type === "current") existing.type = "current";
        return true;
      }
      if (nodes.size >= MAX_LOCAL_GRAPH_NODES) {
        truncated = true;
        return false;
      }
      const pos = seededPosition(index++, depth, width, height);
      nodes.set(id, {
        id,
        label: displayTitle(note),
        type,
        depth,
        note,
        x: pos.x,
        y: pos.y,
        vx: 0,
        vy: 0,
      });
      return true;
    }

    function addTag(tag: string, depth: number): boolean {
      const clean = String(tag || "").trim().replace(/^#/, "");
      if (!clean) return false;
      const id = `tag:${clean.toLowerCase()}`;
      if (nodes.has(id)) {
        nodes.get(id)!.depth = Math.min(nodes.get(id)!.depth, depth);
        return true;
      }
      if (nodes.size >= MAX_LOCAL_GRAPH_NODES) {
        truncated = true;
        return false;
      }
      const pos = seededPosition(index++, depth, width, height);
      nodes.set(id, {
        id,
        label: `#${clean}`,
        type: "tag",
        tag: clean,
        depth,
        x: pos.x,
        y: pos.y,
        vx: 0,
        vy: 0,
      });
      return true;
    }

    function addLink(source: string, target: string, type: LocalLink["type"]): void {
      if (!source || !target || source === target) return;
      if (links.size >= MAX_LOCAL_GRAPH_LINKS) {
        truncated = true;
        return;
      }
      const id = `${source}\n${target}\n${type}`;
      if (!links.has(id)) links.set(id, { id, source, target, type });
    }

    addNote(current, 0, "current");
    const queue: Array<{ note: NoteSummary; depth: number }> = [{ note: current, depth: 0 }];
    const expanded = new Set<string>();

    while (queue.length > 0) {
      const item = queue.shift()!;
      const key = noteKey(item.note);
      if (!key || expanded.has(`${key}:${item.depth}`) || item.depth >= config.depth) continue;
      expanded.add(`${key}:${item.depth}`);
      const nextDepth = item.depth + 1;

      if (config.refs) {
        for (const target of outgoing.get(key) ?? []) {
          const targetKey = noteKey(target);
          if (!addNote(target, nextDepth) || !targetKey) continue;
          addLink(key, targetKey, "ref");
          if (nextDepth < config.depth) queue.push({ note: target, depth: nextDepth });
        }
      }

      if (config.backlinks) {
        for (const source of incoming.get(key) ?? []) {
          const sourceKey = noteKey(source);
          if (!addNote(source, nextDepth) || !sourceKey) continue;
          addLink(sourceKey, key, "backlink");
          if (nextDepth < config.depth) queue.push({ note: source, depth: nextDepth });
        }
      }

      if (config.tags) {
        for (const tag of tagsByNote.get(key) ?? []) {
          const tagId = `tag:${tag.toLowerCase()}`;
          if (!addTag(tag, nextDepth)) continue;
          addLink(key, tagId, "tag");
          if (nextDepth >= config.depth) continue;
          const lower = tag.toLowerCase();
          for (const taggedNote of notes) {
            const taggedKey = noteKey(taggedNote);
            if (!taggedKey || taggedKey === key) continue;
            if (!(tagsByNote.get(taggedKey) ?? []).some((value) => value.toLowerCase() === lower)) continue;
            if (!addNote(taggedNote, nextDepth + 1)) continue;
            addLink(tagId, taggedKey, "tag");
          }
        }
      }
    }

    return { nodes: [...nodes.values()], links: [...links.values()], truncated };
  }

  function renderGraph(): void {
    clearGraph();
    const rect = options.canvas.getBoundingClientRect();
    const width = Math.max(300, Math.round(rect.width || options.canvas.clientWidth || 360));
    const height = Math.max(240, Math.round(rect.height || options.canvas.clientHeight || 260));
    const graph = buildGraph(width, height);
    const { nodes, links } = graph;
    if (nodes.length === 0) {
      const empty = document.createElement("div");
      empty.className = "aaronnote-local-graph-empty";
      empty.textContent = "No local graph";
      options.canvas.replaceChildren(empty);
      options.status.textContent = "";
      return;
    }

    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`);
    svg.setAttribute("role", "img");
    svg.setAttribute("aria-label", "Local graph");
    svg.classList.add("aaronnote-local-graph-svg");

    const linkLayer = document.createElementNS(svg.namespaceURI, "g");
    linkLayer.classList.add("aaronnote-local-graph-links");
    const nodeLayer = document.createElementNS(svg.namespaceURI, "g");
    nodeLayer.classList.add("aaronnote-local-graph-nodes");
    svg.append(linkLayer, nodeLayer);
    options.canvas.replaceChildren(svg);

    const nodeMap = new Map(nodes.map((node) => [node.id, node]));
    const linkEls = links.map((link) => {
      const line = document.createElementNS(svg.namespaceURI, "line");
      line.classList.add("aaronnote-local-graph-link", `is-${link.type}`);
      line.dataset.linkType = link.type;
      line.setAttribute("stroke-linecap", "round");
      linkLayer.appendChild(line);
      return { link, line };
    });

    let dragNode: LocalNode | null = null;
    let dragMoved = false;

    const nodeEls = nodes.map((node) => {
      const group = document.createElementNS(svg.namespaceURI, "g");
      group.classList.add("aaronnote-local-graph-node", `is-${node.type}`, `depth-${Math.min(2, node.depth)}`);
      group.setAttribute("tabindex", "0");
      group.setAttribute("role", "button");
      group.setAttribute("aria-label", node.label);
      const circle = document.createElementNS(svg.namespaceURI, "circle");
      circle.setAttribute("r", node.type === "current" ? "9" : node.type === "tag" ? "5.5" : "7");
      const text = document.createElementNS(svg.namespaceURI, "text");
      text.textContent = labelFit(node.label);
      text.setAttribute("y", node.type === "tag" ? "17" : "20");
      group.append(circle, text);
      group.addEventListener("pointerdown", (event) => {
        event.preventDefault();
        dragNode = node;
        dragMoved = false;
        node.fx = node.x;
        node.fy = node.y;
        group.setPointerCapture(event.pointerId);
      });
      group.addEventListener("pointermove", (event) => {
        if (dragNode !== node) return;
        const point = svg.createSVGPoint();
        point.x = event.clientX;
        point.y = event.clientY;
        const matrix = svg.getScreenCTM();
        if (!matrix) return;
        const local = point.matrixTransform(matrix.inverse());
        if (Math.abs(local.x - node.x) + Math.abs(local.y - node.y) > 3) dragMoved = true;
        node.fx = Math.max(14, Math.min(width - 14, local.x));
        node.fy = Math.max(14, Math.min(height - 22, local.y));
        node.x = node.fx;
        node.y = node.fy;
        applyPositions();
      });
      group.addEventListener("pointerup", (event) => {
        if (dragNode !== node) return;
        group.releasePointerCapture(event.pointerId);
        dragNode = null;
        node.fx = undefined;
        node.fy = undefined;
      });
      group.addEventListener("click", (event) => {
        if (dragMoved) return;
        if (node.type === "tag" && node.tag) {
          options.openTag(node.tag);
          return;
        }
        if (node.note?.file) options.openNote(node.note, { newWindow: event.metaKey || event.altKey });
      });
      group.addEventListener("auxclick", (event) => {
        if (event.button !== 1 || !node.note?.file) return;
        event.preventDefault();
        options.openNote(node.note, { newWindow: true });
      });
      nodeLayer.appendChild(group);
      return { node, group };
    });

    function applyPositions(): void {
      for (const { link, line } of linkEls) {
        const source = nodeMap.get(link.source);
        const target = nodeMap.get(link.target);
        if (!source || !target) continue;
        line.setAttribute("x1", String(source.x));
        line.setAttribute("y1", String(source.y));
        line.setAttribute("x2", String(target.x));
        line.setAttribute("y2", String(target.y));
      }
      for (const { node, group } of nodeEls) {
        group.setAttribute("transform", `translate(${node.x.toFixed(1)} ${node.y.toFixed(1)})`);
      }
    }

    let tick = 0;
    function step(): void {
      if (isCollapsed()) return;
      const alpha = Math.max(0.018, 0.13 * (1 - tick / 120));
      for (const { link } of linkEls) {
        const source = nodeMap.get(link.source);
        const target = nodeMap.get(link.target);
        if (!source || !target) continue;
        const dx = target.x - source.x;
        const dy = target.y - source.y;
        const distance = Math.max(1, Math.hypot(dx, dy));
        const desired = link.type === "tag" ? 64 : 92;
        const strength = (distance - desired) / distance * (link.type === "tag" ? 0.018 : 0.024) * alpha;
        const fx = dx * strength;
        const fy = dy * strength;
        if (source.fx == null) {
          source.vx += fx;
          source.vy += fy;
        }
        if (target.fx == null) {
          target.vx -= fx;
          target.vy -= fy;
        }
      }
      for (let i = 0; i < nodes.length; i += 1) {
        const a = nodes[i]!;
        for (let j = i + 1; j < nodes.length; j += 1) {
          const b = nodes[j]!;
          const dx = b.x - a.x || 0.01;
          const dy = b.y - a.y || 0.01;
          const distance = Math.max(12, Math.hypot(dx, dy));
          const strength = (a.type === "tag" || b.type === "tag" ? 44 : 66) / (distance * distance) * alpha;
          const fx = dx * strength;
          const fy = dy * strength;
          if (a.fx == null) {
            a.vx -= fx;
            a.vy -= fy;
          }
          if (b.fx == null) {
            b.vx += fx;
            b.vy += fy;
          }
        }
      }
      for (const node of nodes) {
        const targetRadius = Math.min(width, height) * (node.depth === 0 ? 0 : node.depth === 1 ? 0.24 : 0.39);
        const dx = node.x - width / 2;
        const dy = node.y - height / 2;
        const distance = Math.max(1, Math.hypot(dx, dy));
        const tx = width / 2 + dx / distance * targetRadius;
        const ty = height / 2 + dy / distance * targetRadius;
        if (node.fx == null) {
          node.vx += (tx - node.x) * 0.006 * alpha;
          node.vy += (ty - node.y) * 0.006 * alpha;
          if (node.type === "current") {
            node.vx += (width / 2 - node.x) * 0.03 * alpha;
            node.vy += (height / 2 - node.y) * 0.03 * alpha;
          }
          node.x = Math.max(18, Math.min(width - 18, node.x + node.vx));
          node.y = Math.max(20, Math.min(height - 26, node.y + node.vy));
          node.vx *= 0.82;
          node.vy *= 0.82;
        } else {
          node.x = node.fx;
          node.y = node.fy ?? node.y;
        }
      }
      applyPositions();
      tick += 1;
      if (tick < 140 || dragNode) animationFrame = window.requestAnimationFrame(step);
    }

    applyPositions();
    animationFrame = window.requestAnimationFrame(step);
    options.status.textContent = `${nodes.length} nodes · ${links.length} links${graph.truncated ? " · capped" : ""}`;
  }

  function update(force = false): void {
    if (isCollapsed()) return;
    expandedOnce = true;
    options.depthLabel.textContent = options.depthInput.value;
    const key = dataSignature();
    if (!force && key === renderKey) return;
    renderKey = key;
    renderGraph();
  }

  function scheduleUpdate(delay = 40): void {
    if (isCollapsed()) return;
    resizeTimer.schedule(() => update(true), undefined, delay);
  }

  function collapse(): void {
    options.root.classList.add("is-collapsed");
    options.toggleButton.setAttribute("aria-expanded", "false");
    clearGraph();
  }

  function toggle(): void {
    const collapsed = isCollapsed();
    options.root.classList.toggle("is-collapsed", !collapsed);
    options.toggleButton.setAttribute("aria-expanded", collapsed ? "true" : "false");
    if (collapsed) {
      window.requestAnimationFrame(() => update(true));
    } else {
      clearGraph();
    }
  }

  function invalidate(): void {
    renderKey = "";
    if (!isCollapsed()) scheduleUpdate();
  }

  options.toggleButton.addEventListener("click", toggle);
  for (const input of [options.depthInput, options.refsInput, options.backlinksInput, options.tagsInput]) {
    input.addEventListener("input", () => update(true));
    input.addEventListener("change", () => update(true));
  }
  window.addEventListener("resize", () => scheduleUpdate(120));

  return { toggle, collapse, update, invalidate };
}
