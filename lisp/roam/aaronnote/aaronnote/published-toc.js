const STORAGE_KEY = "aaronnote-published-toc-collapsed";
const BOOK_STORAGE_KEY = "aaronnote-published-book-toc-collapsed";
const HEADING_SELECTOR = ".aaronnote-section-heading, h1:not(.title), h2, h3, h4, h5, h6";

function normalizeText(value) {
  return String(value || "").replace(/\s+/g, " ").trim();
}

function slugify(value) {
  const slug = normalizeText(value)
    .toLowerCase()
    .replace(/[\u0000-\u001f\u007f]/g, "")
    .replace(/[!"#$%&'()*+,./:;<=>?@[\\\]^`{|}~]/g, "")
    .replace(/\s+/g, "-")
    .replace(/-+/g, "-")
    .replace(/^-|-$/g, "");
  return slug || "section";
}

function ensureHeadingIds(headings) {
  const used = new Set();
  headings.forEach((heading) => {
    let id = heading.id.trim();
    if (!id || used.has(id)) {
      const base = slugify(heading.textContent);
      id = base;
      let suffix = 2;
      while (used.has(id) || document.getElementById(id)) {
        id = `${base}-${suffix}`;
        suffix += 1;
      }
      heading.id = id;
    }
    used.add(id);
  });
}

function collectHeadings(article) {
  const headings = Array.from(article.querySelectorAll(HEADING_SELECTOR))
    .filter((heading) => heading instanceof HTMLElement)
    .filter((heading) => !heading.closest("[hidden], [aria-hidden='true']"));
  const hasSemantic = headings.some((heading) => heading.classList.contains("aaronnote-section-heading"));
  ensureHeadingIds(headings);
  return headings.map((heading) => ({
    id: heading.id,
    level: heading.classList.contains("aaronnote-section-heading")
      ? Number(heading.dataset.outlineLevel) || 2
      : (hasSemantic ? 5 : 0) + (Number(heading.tagName.slice(1)) || 1),
    text: heading.classList.contains("aaronnote-section-heading")
      ? normalizeText(heading.querySelector(".aaronnote-section-title")?.textContent || heading.textContent) || "Untitled"
      : normalizeText(heading.textContent) || "Untitled",
    element: heading,
  }));
}

function scrollToHeading(heading, behavior = "smooth") {
  const reducedMotion = window.matchMedia?.("(prefers-reduced-motion: reduce)").matches;
  const top = heading.getBoundingClientRect().top + window.scrollY - 82;
  window.scrollTo({
    top: Math.max(0, top),
    behavior: reducedMotion ? "auto" : behavior,
  });
}

function publishedBookData() {
  const el = document.getElementById("aaronnote-book-toc-data");
  if (!(el instanceof HTMLScriptElement)) return null;
  try {
    const data = JSON.parse(el.textContent || "{}");
    return Array.isArray(data?.toc) && data.toc.length > 0 ? data : null;
  } catch {
    return null;
  }
}

function bookPathKey(value) {
  return String(value || "").replace(/\\/g, "/").replace(/^\.\/+/, "").replace(/^roam\//, "");
}

function bookNodeKey(item, index) {
  return [item.path || "", item.slug || "", item.text || "", String(index)].join("::");
}

function buildBookTree(items) {
  const roots = [];
  const stack = [];
  items.forEach((item, index) => {
    const level = Math.max(1, Math.min(12, Number(item.level) || 1));
    const node = { item, key: bookNodeKey(item, index), level, children: [] };
    while (stack.length > 0 && stack[stack.length - 1].level >= level) stack.pop();
    if (stack.length > 0) stack[stack.length - 1].children.push(node);
    else roots.push(node);
    stack.push(node);
  });
  return roots;
}

function samePageHref(href) {
  try {
    const url = new URL(href || "#", window.location.href);
    return url.origin === window.location.origin && url.pathname === window.location.pathname;
  } catch {
    return false;
  }
}

function hrefHash(href) {
  try {
    const url = new URL(href || "#", window.location.href);
    return decodeURIComponent(url.hash.replace(/^#/, ""));
  } catch {
    return "";
  }
}

function currentLocationHash() {
  return decodeURIComponent(window.location.hash.replace(/^#/, ""));
}

function initPublishedBookToc(article, genericToc, bookData) {
  collectHeadings(article);

  const page = document.querySelector(".published-note-page");
  if (page instanceof HTMLElement) page.classList.add("has-published-book-toc");

  const trigger = document.createElement("button");
  trigger.type = "button";
  trigger.className = "aaronnote-published-book-trigger";
  trigger.setAttribute("aria-expanded", "false");
  trigger.title = "Toggle book contents";
  trigger.textContent = "Book";

  const drawer = document.createElement("aside");
  drawer.className = "aaronnote-published-book-toc is-collapsed";
  drawer.setAttribute("aria-label", "Book contents");
  const list = document.createElement("nav");
  list.setAttribute("aria-label", "Book contents");
  drawer.append(list);
  document.body.append(drawer, trigger);

  const expanded = new Set();
  const items = bookData.toc || [];

  function setCollapsed(collapsed) {
    drawer.classList.toggle("is-collapsed", collapsed);
    trigger.setAttribute("aria-expanded", collapsed ? "false" : "true");
    document.body.classList.toggle("published-book-toc-open", !collapsed);
    window.localStorage?.setItem(BOOK_STORAGE_KEY, String(collapsed));
  }

  function itemIsActive(item) {
    return bookPathKey(item.path) === bookPathKey(bookData.currentPath)
      && item.slug
      && item.slug === currentLocationHash();
  }

  function renderNode(parent, node, depth) {
    const { item } = node;
    const row = document.createElement("div");
    row.className = "aaronnote-published-book-toc-row";
    row.style.setProperty("--book-depth", String(depth));

    if (node.children.length > 0) {
      const branch = document.createElement("button");
      branch.type = "button";
      branch.className = "aaronnote-published-book-toc-branch";
      branch.setAttribute("aria-expanded", expanded.has(node.key) ? "true" : "false");
      branch.textContent = expanded.has(node.key) ? "▾" : "▸";
      branch.addEventListener("click", () => {
        if (expanded.has(node.key)) expanded.delete(node.key);
        else expanded.add(node.key);
        render();
      });
      row.append(branch);
    } else {
      const spacer = document.createElement("span");
      spacer.className = "aaronnote-published-book-toc-spacer";
      row.append(spacer);
    }

    const link = document.createElement("a");
    link.className = "aaronnote-published-book-toc-item";
    link.href = item.href || "#";
    link.title = [item.text || "", item.path || ""].filter(Boolean).join(" · ");
    link.textContent = item.text || item.path || "Untitled";
    if (bookPathKey(item.path) === bookPathKey(bookData.currentPath)) link.classList.add("is-current-file");
    if (itemIsActive(item)) {
      link.classList.add("is-active");
      link.setAttribute("aria-current", "location");
    }
    link.addEventListener("click", (event) => {
      if (!samePageHref(link.getAttribute("href") || "")) return;
      const targetId = hrefHash(link.getAttribute("href") || "");
      const target = targetId ? document.getElementById(targetId) : null;
      if (!(target instanceof HTMLElement)) return;
      event.preventDefault();
      scrollToHeading(target);
      window.history.replaceState(null, "", `#${encodeURIComponent(targetId)}`);
      render();
    });
    row.append(link);
    parent.append(row);

    if (expanded.has(node.key)) {
      for (const child of node.children) renderNode(parent, child, depth + 1);
    }
  }

  function render() {
    const frag = document.createDocumentFragment();
    const status = document.createElement("header");
    status.className = "aaronnote-published-book-toc-status";
    const title = document.createElement("strong");
    title.textContent = bookData.title || "Book";
    const count = document.createElement("span");
    count.textContent = `${items.length} headings`;
    status.append(title, count);
    frag.append(status);
    for (const node of buildBookTree(items)) renderNode(frag, node, 0);
    list.replaceChildren(frag);
  }

  const stored = window.localStorage?.getItem(BOOK_STORAGE_KEY);
  setCollapsed(stored === null ? true : stored === "true");
  trigger.addEventListener("click", () => setCollapsed(!drawer.classList.contains("is-collapsed")));
  window.addEventListener("hashchange", render);
  render();

  if (window.location.hash) {
    const target = document.getElementById(currentLocationHash());
    if (target instanceof HTMLElement && article.contains(target)) {
      window.requestAnimationFrame(() => scrollToHeading(target, "auto"));
    }
  }
}

function initPublishedToc() {
  const article = document.getElementById("content");
  const toc = document.querySelector("[data-published-toc]");
  const list = toc?.querySelector("[data-toc-list]");
  const toggle = toc?.querySelector("[data-toc-toggle]");
  if (!(article instanceof HTMLElement) || !(toc instanceof HTMLElement) || !(list instanceof HTMLElement) || !(toggle instanceof HTMLButtonElement)) {
    return;
  }

  const bookData = publishedBookData();
  if (bookData) {
    initPublishedBookToc(article, toc, bookData);
  }

  let headings = [];
  let activeId = "";
  let renderKey = "";
  let frame = 0;

  function setCollapsed(collapsed) {
    toc.classList.toggle("is-collapsed", collapsed);
    toggle.setAttribute("aria-expanded", collapsed ? "false" : "true");
    window.localStorage?.setItem(STORAGE_KEY, String(collapsed));
  }

  function activeHeadingId() {
    if (headings.length === 0) return "";
    let current = headings[0].id;
    headings.forEach((heading) => {
      if (heading.element.getBoundingClientRect().top <= 96) {
        current = heading.id;
      }
    });
    return current;
  }

  function updateActive() {
    activeId = activeHeadingId();
    list.querySelectorAll(".aaronnote-toc-item").forEach((button) => {
      const active = button.getAttribute("data-heading-id") === activeId;
      button.classList.toggle("is-active", active);
      if (active) button.setAttribute("aria-current", "location");
      else button.removeAttribute("aria-current");
    });
  }

  function scheduleActiveUpdate() {
    if (frame) return;
    frame = window.requestAnimationFrame(() => {
      frame = 0;
      updateActive();
    });
  }

  function render() {
    headings = collectHeadings(article);
    const key = headings.map((heading) => `${heading.level}:${heading.id}:${heading.text}`).join("\n");
    if (key === renderKey) {
      updateActive();
      return;
    }
    renderKey = key;
    toc.hidden = headings.length === 0;
    toggle.textContent = headings.length > 0 ? `Page ${headings.length}` : "Page";
    if (headings.length === 0) {
      list.replaceChildren();
      return;
    }

    const frag = document.createDocumentFragment();
    const status = document.createElement("div");
    status.className = "aaronnote-toc-status";
    status.textContent = `${headings.length} headings`;
    frag.appendChild(status);

    headings.forEach((heading) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "aaronnote-toc-item";
      button.style.setProperty("--toc-depth", String(Math.max(0, heading.level - 1)));
      button.dataset.headingId = heading.id;
      button.dataset.level = String(heading.level);
      button.title = heading.text;
      button.textContent = heading.text;
      button.addEventListener("click", () => {
        scrollToHeading(heading.element);
        window.history.replaceState(null, "", `#${encodeURIComponent(heading.id)}`);
        updateActive();
      });
      frag.appendChild(button);
    });

    list.replaceChildren(frag);
    updateActive();
  }

  function scheduleRender() {
    window.requestAnimationFrame(render);
  }

  const stored = window.localStorage?.getItem(STORAGE_KEY);
  setCollapsed(stored === null ? true : stored === "true");
  toggle.addEventListener("click", () => setCollapsed(!toc.classList.contains("is-collapsed")));
  window.addEventListener("scroll", scheduleActiveUpdate, { passive: true });
  window.addEventListener("resize", scheduleActiveUpdate);
  window.addEventListener("aaronnote:kind-ready", scheduleRender);

  const observer = new MutationObserver(scheduleRender);
  observer.observe(article, { childList: true, subtree: true, characterData: true });
  render();

  if (window.location.hash) {
    const targetId = decodeURIComponent(window.location.hash.slice(1));
    const target = document.getElementById(targetId);
    if (target instanceof HTMLElement && article.contains(target)) {
      window.requestAnimationFrame(() => scrollToHeading(target, "auto"));
    }
  }
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", initPublishedToc, { once: true });
} else {
  initPublishedToc();
}
