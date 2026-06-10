const CONTROL_CLASS = "aaronnote-slides-controls";
const PROGRESS_CLASS = "aaronnote-slides-progress";
const TOC_CLASS = "aaronnote-slides-toc";
const RUNTIME_STYLE_ATTR = "data-aaronnote-slides-runtime";
const GLOBAL_CONFIG_KINDS = new Set(["slidegconfig", "slide-g-config", "slidesconfig", "slideconfig"]);
const LOCAL_CONFIG_KINDS = new Set(["slidelconfig", "slide-l-config", "slidelocalconfig", "slide-local-config", "lconfig"]);

function slideSurface(context) {
  const article = context?.article;
  if (article instanceof HTMLElement) return article;
  return context?.host?.querySelector?.(".cm-editor")
    || document.querySelector("#content")
    || document.querySelector(".cm-editor");
}

function orgEnvKind(child) {
  return child instanceof HTMLElement && child.matches("org-env-block")
    ? String(child.dataset.kind || "").toLowerCase()
    : "";
}

function isMetaBlock(child) {
  return orgEnvKind(child) === "meta";
}

function isGlobalConfigBlock(child) {
  return GLOBAL_CONFIG_KINDS.has(orgEnvKind(child));
}

function isLocalConfigBlock(child) {
  return LOCAL_CONFIG_KINDS.has(orgEnvKind(child));
}

function isSlidesChrome(child) {
  return child.classList.contains(CONTROL_CLASS)
    || child.classList.contains(PROGRESS_CLASS)
    || child.classList.contains(TOC_CLASS);
}

function displayChildren(surface) {
  return Array.from(surface.children).filter((child) => {
    if (!(child instanceof HTMLElement)) return false;
    if (isSlidesChrome(child)) return false;
    if (isMetaBlock(child) || isGlobalConfigBlock(child) || isLocalConfigBlock(child)) return false;
    if (child.matches("script, style")) return false;
    return true;
  });
}

function emptySlide() {
  return { items: [], configBlocks: [] };
}

function buildSlides(surface) {
  const slides = [];
  let current = emptySlide();
  for (const child of Array.from(surface.children)) {
    if (!(child instanceof HTMLElement)) continue;
    if (isSlidesChrome(child) || child.matches("script, style")) continue;
    if (isMetaBlock(child) || isGlobalConfigBlock(child)) continue;
    if (isLocalConfigBlock(child)) {
      current.configBlocks.push(child);
      continue;
    }

    const isBreak = child.matches("hr");
    const isHeadingStart = child.matches("h1") && current.items.length > 0;
    if (isBreak || isHeadingStart) {
      if (current.items.length > 0) slides.push(current);
      current = emptySlide();
      if (isBreak) continue;
    }
    current.items.push(child);
  }
  if (current.items.length > 0) slides.push(current);
  return slides.length > 0 ? slides : [{ items: displayChildren(surface), configBlocks: [] }];
}

function textConfigValue(value) {
  return String(value || "").trim();
}

function boolConfigValue(value) {
  return /^(1|true|yes|on)$/i.test(textConfigValue(value));
}

function normalizeConfigKey(key) {
  return String(key || "").trim().toLowerCase().replace(/[_\s]+/g, "-");
}

function parseConfigText(text) {
  const config = {};
  const css = [];
  let collectingCss = false;
  for (const rawLine of String(text || "").split(/\r?\n/)) {
    const pair = rawLine.match(/^([A-Za-z][\w -]*)\s*:\s*(.*)$/);
    if (pair) {
      const key = normalizeConfigKey(pair[1]);
      const value = textConfigValue(pair[2]);
      collectingCss = key === "css";
      if (key === "toc") config.toc = boolConfigValue(value);
      else if (key === "title-position") config.titlePosition = value.toLowerCase();
      else if (key === "title-align") config.titleAlign = value.toLowerCase();
      else if (key === "css" && value) css.push(value);
      else if (key === "class") config.className = value;
      continue;
    }
    if (collectingCss) css.push(rawLine.replace(/^\s{2}/, ""));
  }
  if (css.length > 0) config.css = css.join("\n").trim();
  return config;
}

function mergeConfig(base, next) {
  return Object.assign({}, base, next || {});
}

function configBlocks(surface, predicate) {
  return Array.from(surface.children).filter((child) => child instanceof HTMLElement && predicate(child));
}

function globalConfig(surface) {
  return configBlocks(surface, isGlobalConfigBlock)
    .map((block) => parseConfigText(block.textContent || ""))
    .reduce((config, next) => mergeConfig(config, next), {});
}

function slideConfig(base, slide) {
  return slide.configBlocks
    .map((block) => parseConfigText(block.textContent || ""))
    .reduce((config, next) => mergeConfig(config, next), base);
}

function makeControls() {
  const controls = document.createElement("div");
  controls.className = CONTROL_CLASS;
  controls.innerHTML = `
    <button type="button" data-slide-action="prev">Prev</button>
    <span class="aaronnote-slides-counter" data-slide-counter></span>
    <button type="button" data-slide-action="next">Next</button>
  `;

  const progress = document.createElement("div");
  progress.className = PROGRESS_CLASS;
  progress.innerHTML = "<span></span>";

  document.body.append(controls, progress);
  return { controls, progress };
}

function makeToc() {
  const toc = document.createElement("nav");
  toc.className = TOC_CLASS;
  toc.setAttribute("aria-label", "Slides");
  document.body.append(toc);
  return toc;
}

function editableTarget(event) {
  const target = event.target;
  return target instanceof HTMLElement
    && Boolean(target.closest('input, textarea, select, [contenteditable="true"]'));
}

function surfaceSelectors(surface) {
  if (surface.id && /^[A-Za-z][\w-]*$/.test(surface.id)) return [`body[data-note-kind="slides"] #${surface.id}`];
  if (surface.classList.contains("cm-editor")) return ['body[data-note-kind="slides"] .cm-editor'];
  if (surface.matches("#content")) return ['body[data-note-kind="slides"] #content'];
  return [
    'body[data-note-kind="slides"] .cm-editor',
    'body[data-note-kind="slides"] #content',
  ];
}

function childNumber(surface, child) {
  return Array.prototype.indexOf.call(surface.children, child) + 1;
}

function slideTitle(slide, fallback) {
  const heading = slide.items.find((item) => item.matches("h1, h2"));
  const title = heading?.textContent?.trim();
  return title || fallback;
}

function titleRules(selector, slide, config) {
  const position = String(config.titlePosition || "").toLowerCase();
  const align = String(config.titleAlign || "").toLowerCase();
  if (!position && !align) return [];
  const declarations = [];
  if (["center", "middle"].includes(position)) declarations.push("margin-top: 22vh");
  if (position === "bottom") declarations.push("margin-top: 44vh");
  if (["left", "center", "right"].includes(position)) declarations.push(`text-align: ${position}`);
  if (["left", "center", "right"].includes(align)) declarations.push(`text-align: ${align}`);
  if (declarations.length === 0) return [];
  return slide.items
    .filter((item) => item.matches("h1"))
    .map((item) => childNumber(item.parentElement, item))
    .filter((n) => n > 0)
    .map((n) => `${selector} > h1:nth-child(${n}) { ${declarations.join("; ")}; }`);
}

export default function setup(initialContext = {}) {
  let context = initialContext;
  let surface = null;
  let controls = null;
  let progress = null;
  let toc = null;
  let runtimeStyle = null;
  let slides = [];
  let config = {};
  let index = 0;
  let observer = null;
  let rebuildTimer = 0;

  function ensureRuntimeStyle() {
    if (runtimeStyle?.isConnected) return runtimeStyle;
    runtimeStyle = document.createElement("style");
    runtimeStyle.setAttribute(RUNTIME_STYLE_ATTR, "true");
    document.head.appendChild(runtimeStyle);
    return runtimeStyle;
  }

  function removeRuntimeStyle() {
    runtimeStyle?.remove();
    runtimeStyle = null;
  }

  function hiddenElementIndexes() {
    const hidden = new Set();
    slides.forEach((slide, slideIndex) => {
      if (slideIndex === index) return;
      for (const child of slide.items) hidden.add(child);
    });
    return [...hidden]
      .map((child) => childNumber(surface, child))
      .filter((n) => n > 0);
  }

  function updateVisibility() {
    if (!surface) {
      removeRuntimeStyle();
      return;
    }
    const style = ensureRuntimeStyle();
    const selectors = surfaceSelectors(surface);
    const activeSlide = slides[index] || emptySlide();
    const activeConfig = slideConfig(config, activeSlide);
    const rules = [];
    for (const selector of selectors) {
      rules.push(`${selector} > org-env-block[data-kind="meta"] { display: none !important; }`);
      for (const kind of [...GLOBAL_CONFIG_KINDS, ...LOCAL_CONFIG_KINDS]) {
        rules.push(`${selector} > org-env-block[data-kind="${kind}"] { display: none !important; }`);
      }
      rules.push(`${selector} > hr { display: none !important; }`);
      for (const childIndex of hiddenElementIndexes()) {
        rules.push(`${selector} > :nth-child(${childIndex}) { display: none !important; }`);
      }
      rules.push(...titleRules(selector, activeSlide, activeConfig));
    }
    if (config.css) rules.push(String(config.css));
    if (activeConfig.css && activeConfig.css !== config.css) rules.push(String(activeConfig.css));
    style.textContent = rules.join("\n");
    updateToc(activeConfig);
  }

  function ensureControls() {
    if (controls && progress) return;
    const created = makeControls();
    controls = created.controls;
    progress = created.progress;
    controls.querySelector('[data-slide-action="prev"]')?.addEventListener("click", () => show(index - 1));
    controls.querySelector('[data-slide-action="next"]')?.addEventListener("click", () => show(index + 1));
  }

  function removeControls() {
    controls?.remove();
    progress?.remove();
    controls = null;
    progress = null;
  }

  function updateToc(activeConfig) {
    if (!boolConfigValue(activeConfig.toc)) {
      toc?.remove();
      toc = null;
      return;
    }
    if (!toc?.isConnected) toc = makeToc();
    toc.replaceChildren(...slides.map((slide, slideIndex) => {
      const button = document.createElement("button");
      button.type = "button";
      button.textContent = slideTitle(slide, `Slide ${slideIndex + 1}`);
      button.className = slideIndex === index ? "is-active" : "";
      button.addEventListener("click", () => show(slideIndex));
      return button;
    }));
  }

  function removeToc() {
    toc?.remove();
    toc = null;
  }

  function show(nextIndex) {
    if (!surface || slides.length === 0) return;
    index = Math.max(0, Math.min(nextIndex, slides.length - 1));
    updateVisibility();
    const counter = controls?.querySelector("[data-slide-counter]");
    if (counter) counter.textContent = `${index + 1} / ${slides.length}`;
    controls?.querySelector('[data-slide-action="prev"]')?.toggleAttribute("disabled", index === 0);
    controls?.querySelector('[data-slide-action="next"]')?.toggleAttribute("disabled", index === slides.length - 1);
    progress?.style.setProperty("--slides-progress", `${((index + 1) / slides.length) * 100}%`);
  }

  function watchSurface() {
    observer?.disconnect();
    observer = null;
    if (!surface) return;
    observer = new MutationObserver(() => {
      scheduleRebuild();
    });
    observer.observe(surface, { childList: true, subtree: false });
  }

  function scheduleRebuild() {
    window.clearTimeout(rebuildTimer);
    rebuildTimer = window.setTimeout(() => rebuild(context, false), 30);
  }

  function rebuild(nextContext = context, resetIndex = true) {
    context = nextContext;
    if (resetIndex) index = 0;
    surface = slideSurface(context);
    if (!surface) {
      removeControls();
      removeRuntimeStyle();
      removeToc();
      return;
    }
    config = globalConfig(surface);
    slides = buildSlides(surface).filter((slide) => slide.items.length > 0);
    watchSurface();
    if (slides.length <= 1) {
      removeControls();
    } else {
      ensureControls();
    }
    show(Math.min(index, Math.max(0, slides.length - 1)));
  }

  function onKindReady(event) {
    if (event?.detail?.kind === "slides") rebuild(event.detail);
  }

  function onKeydown(event) {
    if (editableTarget(event) && !event.altKey) return;
    if (event.key === "ArrowRight" || event.key === "PageDown" || event.key === " ") {
      event.preventDefault();
      show(index + 1);
    } else if (event.key === "ArrowLeft" || event.key === "PageUp") {
      event.preventDefault();
      show(index - 1);
    } else if (event.key === "Home") {
      event.preventDefault();
      show(0);
    } else if (event.key === "End") {
      event.preventDefault();
      show(slides.length - 1);
    }
  }

  window.addEventListener("aaronnote:kind-ready", onKindReady);
  window.addEventListener("keydown", onKeydown);
  rebuild(context);
  window.requestAnimationFrame(() => rebuild(context, false));
  window.requestAnimationFrame(() => window.requestAnimationFrame(() => rebuild(context, false)));
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", scheduleRebuild, { once: true });
  }

  return () => {
    window.removeEventListener("aaronnote:kind-ready", onKindReady);
    window.removeEventListener("keydown", onKeydown);
    document.removeEventListener("DOMContentLoaded", scheduleRebuild);
    observer?.disconnect();
    window.clearTimeout(rebuildTimer);
    removeRuntimeStyle();
    removeControls();
    removeToc();
  };
}
