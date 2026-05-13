(function () {
  function escapeHtml(value) {
    return String(value)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
  }

  function resolveElement(target, fallbackId) {
    if (target && typeof target !== "string") {
      return target;
    }

    const id = typeof target === "string" && target ? target : fallbackId;
    return id ? document.getElementById(id) : null;
  }

  function initKnowledgeGraph(options = {}) {
    const knowledge = options.knowledge || window.KNOWLEDGE_DATA;
    const container = resolveElement(options.container, options.containerId || "graph-container");
    const focusPanel = resolveElement(options.focusPanel, options.focusPanelId || "graph-focus");
    const linkPrefix = String(options.linkPrefix || "");
    const emptyMessage = String(options.emptyMessage || "Select a node to inspect its links.");
    const listenForGlobalFilters = options.listenForGlobalFilters !== false;
    const dispatchTagEvents = options.dispatchTagEvents !== false;
    const dispatchFocusEvents = options.dispatchFocusEvents !== false;
    const onNoteOpen = typeof options.onNoteOpen === "function" ? options.onNoteOpen : null;
    const wantToolbar = options.toolbar === true
      || (container && container.dataset && container.dataset.graphToolbar === "true");

    if (!container) {
      return null;
    }

    if (container.dataset.graphMounted === "true") {
      return container._knowledgeGraph || null;
    }

    if (typeof d3 === "undefined") {
      container.innerHTML = '<div class="graph-message">D3 failed to load.</div>';
      return null;
    }

    if (!knowledge || knowledge.notes.length === 0) {
      container.innerHTML = '<div class="graph-message">No notes available yet.</div>';
      return null;
    }

    const noteNodes = knowledge.notes.map((note) => ({
      id: note.key,
      key: note.key,
      label: note.title,
      kind: "note",
      note,
    }));

    const tagNodes = knowledge.tags.map((tag) => ({
      id: `tag:${tag.name}`,
      label: tag.name,
      kind: "tag",
      tag,
    }));

    const nodes = [...noteNodes, ...tagNodes];
    const links = [];
    const linkKeys = new Set();
    const nodeById = new Map(nodes.map((node) => [node.id, node]));

    function pushLink(source, target, kind) {
      const key = `${source}::${target}::${kind}`;
      if (linkKeys.has(key)) {
        return;
      }
      linkKeys.add(key);
      links.push({ source, target, kind });
    }

    knowledge.notes.forEach((note) => {
      note.refs.forEach((targetKey) => {
        pushLink(note.key, targetKey, "reference");
      });

      note.tags.forEach((tag) => {
        pushLink(note.key, `tag:${tag}`, "tag");
      });
    });

    const adjacency = new Map();
    nodes.forEach((node) => adjacency.set(node.id, new Set()));
    links.forEach((link) => {
      adjacency.get(link.source)?.add(link.target);
      adjacency.get(link.target)?.add(link.source);
    });

    const defaultVisibleKeys = Array.isArray(options.initialVisibleKeys) && options.initialVisibleKeys.length > 0
      ? options.initialVisibleKeys
      : knowledge.notes.map((note) => note.key);
    const visibleKeys = new Set(defaultVisibleKeys.filter((key) => nodeById.has(key)));

    let selectedId = "";
    let width = 0;
    let height = 0;
    let resizeFrame = 0;
    let svg;
    let canvas;
    let linkSelection;
    let nodeSelection;
    let labelSelection;
    let simulation;
    let zoomBehavior;

    const groupPalette = d3.scaleOrdinal()
      .domain(knowledge.groups.map((group) => group.key))
      .range(["#5a79c7", "#f0a45b", "#6aa6a0", "#8c84d8", "#7a9a5d", "#d47a72", "#6378a5"]);

    function buildNoteHref(note) {
      return `${linkPrefix}${note.link}`;
    }

    function dispatchEvent(name, detail) {
      document.dispatchEvent(new CustomEvent(name, { detail }));
    }

    function nodeColor(node) {
      if (node.kind === "tag") {
        return "#d7a15a";
      }

      return groupPalette(node.note.groupKey);
    }

    function updateFocusPanel(node) {
      if (!focusPanel) {
        return;
      }

      if (!node) {
        focusPanel.classList.add("empty");
        focusPanel.innerHTML = `
          <p class="graph-focus-copy">${escapeHtml(emptyMessage)}</p>
        `;
        return;
      }

      focusPanel.classList.remove("empty");

      if (node.kind === "tag") {
        const related = node.tag.notes
          .filter((key) => visibleKeys.has(key))
          .slice(0, 6)
          .map((key) => knowledge.byKey.get(key))
          .filter(Boolean)
          .map(
            (note) => `<a class="graph-related-link" href="${escapeHtml(buildNoteHref(note))}">${escapeHtml(note.title)}</a>`,
          )
          .join("");

        focusPanel.innerHTML = `
          <div class="graph-focus-header">
            <span class="graph-focus-type">Tag</span>
            <strong>#${escapeHtml(node.label)}</strong>
          </div>
          <p class="graph-focus-copy">${node.tag.count} notes currently use this tag.</p>
          <div class="graph-related-list">${related || "<span class='graph-related-empty'>No linked notes.</span>"}</div>
        `;
        return;
      }

      const note = node.note;
      const tags = note.tags
        .map((tag) => `<button type="button" class="graph-inline-tag" data-tag="${escapeHtml(tag)}">#${escapeHtml(tag)}</button>`)
        .join("");

      focusPanel.innerHTML = `
        <div class="graph-focus-header">
          <span class="graph-focus-type">${escapeHtml(note.groupLabel)}</span>
          <strong>${escapeHtml(note.title)}</strong>
        </div>
        <p class="graph-focus-copy">${escapeHtml(note.summary || "No summary yet.")}</p>
        <div class="graph-focus-meta">
          <span>${escapeHtml(note.date || "--")}</span>
          <span>${note.refs.length} refs</span>
          <span>${note.backlinks.length} backlinks</span>
        </div>
        <div class="graph-inline-tags">${tags}</div>
        <a class="graph-open-link" href="${escapeHtml(buildNoteHref(note))}" data-note-key="${escapeHtml(note.key)}">Open note</a>
      `;

      focusPanel.querySelectorAll(".graph-inline-tag").forEach((button) => {
        button.addEventListener("click", () => {
          if (dispatchTagEvents) {
            dispatchEvent("knowledge:apply-tag", {
              tag: button.dataset.tag || "",
            });
          }
        });
      });

      if (onNoteOpen) {
        const openLink = focusPanel.querySelector(".graph-open-link");
        if (openLink) {
          openLink.addEventListener("click", (event) => {
            event.preventDefault();
            onNoteOpen(note);
          });
        }
      }
    }

    function isNodeVisible(node) {
      if (!node) {
        return false;
      }

      if (node.kind === "tag") {
        return node.tag.notes.some((key) => visibleKeys.has(key));
      }

      return visibleKeys.has(node.key);
    }

    function applyStyles() {
      const activeNeighbors = selectedId ? adjacency.get(selectedId) || new Set() : new Set();

      nodeSelection
        .style("opacity", (node) => {
          if (!isNodeVisible(node)) return 0.18;
          if (!selectedId) return 1;
          if (node.id === selectedId || activeNeighbors.has(node.id)) return 1;
          return 0.28;
        })
        .attr("r", (node) => {
          if (node.kind === "tag") {
            return selectedId === node.id ? 11 : 8;
          }

          const base = 7 + Math.min(node.note.refs.length + node.note.backlinks.length, 4);
          return selectedId === node.id ? base + 3 : base;
        })
        .attr("fill", nodeColor)
        .attr("stroke", (node) => (selectedId === node.id ? "#162338" : "#ffffff"))
        .attr("stroke-width", (node) => (selectedId === node.id ? 2.4 : 1.4));

      labelSelection.style("opacity", (node) => {
        if (!isNodeVisible(node)) return 0.15;
        if (!selectedId) return node.kind === "note" ? 0.9 : 0.72;
        if (node.id === selectedId || activeNeighbors.has(node.id)) return 1;
        return 0.2;
      });

      linkSelection
        .style("opacity", (link) => {
          const sourceId = typeof link.source === "string" ? link.source : link.source.id;
          const targetId = typeof link.target === "string" ? link.target : link.target.id;
          const visible = isNodeVisible(nodeById.get(sourceId)) && isNodeVisible(nodeById.get(targetId));

          if (!visible) return 0.05;
          if (!selectedId) return link.kind === "reference" ? 0.55 : 0.22;
          if (sourceId === selectedId || targetId === selectedId) return 0.9;
          if (activeNeighbors.has(sourceId) && activeNeighbors.has(targetId)) return 0.5;
          return 0.08;
        })
        .attr("stroke-width", (link) => (link.kind === "reference" ? 1.8 : 1.1))
        .attr("stroke", (link) => (link.kind === "reference" ? "#70829d" : "#c9d4e5"));
    }

    function setSelected(node, { dispatch = true } = {}) {
      selectedId = node ? node.id : "";
      updateFocusPanel(node || null);
      applyStyles();

      if (dispatch && node && node.kind === "note" && dispatchFocusEvents) {
        dispatchEvent("knowledge:focus-note", { key: node.key });
      }
    }

    function fitToNodes(predicate) {
      const matching = nodes.filter((node) => predicate(node) && typeof node.x === "number" && typeof node.y === "number");
      if (matching.length === 0) {
        return;
      }

      const minX = d3.min(matching, (node) => node.x);
      const maxX = d3.max(matching, (node) => node.x);
      const minY = d3.min(matching, (node) => node.y);
      const maxY = d3.max(matching, (node) => node.y);
      const boxWidth = Math.max(maxX - minX, 80);
      const boxHeight = Math.max(maxY - minY, 80);
      const scale = Math.min(2.2, 0.88 / Math.max(boxWidth / width, boxHeight / height));
      const translateX = width / 2 - scale * (minX + maxX) / 2;
      const translateY = height / 2 - scale * (minY + maxY) / 2;

      svg.transition().duration(300).call(
        zoomBehavior.transform,
        d3.zoomIdentity.translate(translateX, translateY).scale(scale),
      );
    }

    function refitGraph(predicate, clearSelection = false) {
      if (clearSelection) {
        setSelected(null, { dispatch: false });
      }

      simulation.alpha(0.55).restart();
      window.setTimeout(() => fitToNodes(predicate), 180);
    }

    function measureContainerSize() {
      const nextWidth = Math.max(320, Math.round(container.clientWidth || 900));
      const nextHeight = Math.max(240, Math.round(container.clientHeight || 520));
      return { width: nextWidth, height: nextHeight };
    }

    let toolbarEl = null;
    let toolbarSearchInput = null;
    let toolbarStatus = null;
    let toolbarSuggestionBox = null;
    let toolbarSuggestions = [];
    let toolbarActiveSuggestionIndex = -1;
    let toolbarClickListener = null;
    let searchFilterTimer = 0;

    function uniqueSorted(values) {
      return Array.from(new Set(values.map((value) => String(value || "").trim()).filter(Boolean)))
        .sort((a, b) => a.localeCompare(b));
    }

    function quoteSearchValue(value) {
      const text = String(value || "");
      if (/[\s"]/u.test(text)) {
        return `"${text.replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"`;
      }
      return text;
    }

    function graphSuggestionSources() {
      const notes = Array.isArray(knowledge.notes) ? knowledge.notes : [];
      return {
        tag: uniqueSorted((knowledge.tags || []).map((tag) => tag.name)),
        alias: uniqueSorted(notes.flatMap((note) => note.aliases || [])),
        path: uniqueSorted(notes.map((note) => note.path).filter(Boolean)),
        title: uniqueSorted(notes.map((note) => note.title).filter(Boolean)),
        group: uniqueSorted(notes.flatMap((note) => [note.groupKey, note.groupLabel]).filter(Boolean)),
        section: uniqueSorted(notes.map((note) => note.section).filter(Boolean)),
      };
    }

    function graphSuggestionField(token) {
      const raw = String(token || "");
      const lower = raw.toLowerCase();
      if (lower.startsWith("#")) {
        return { field: "tag", query: lower.slice(1), sigil: "#" };
      }

      const match = lower.match(/^([a-z]+):(.*)$/);
      if (!match) {
        return null;
      }

      const aliases = {
        tags: "tag",
        aka: "alias",
        aliases: "alias",
        file: "path",
        folder: "group",
      };
      const field = aliases[match[1]] || match[1];
      if (!["tag", "alias", "path", "title", "group", "section"].includes(field)) {
        return null;
      }

      return { field, query: match[2], sigil: "" };
    }

    function currentSearchToken() {
      if (!toolbarSearchInput) {
        return null;
      }

      const value = toolbarSearchInput.value;
      const cursor = toolbarSearchInput.selectionStart ?? value.length;
      let start = cursor;
      let end = cursor;

      while (start > 0 && !/\s/u.test(value[start - 1])) start -= 1;
      while (end < value.length && !/\s/u.test(value[end])) end += 1;

      return {
        value,
        start,
        end,
        token: value.slice(start, end),
      };
    }

    function buildGraphSuggestions() {
      const tokenInfo = currentSearchToken();
      if (!tokenInfo) {
        return [];
      }

      const token = tokenInfo.token;
      const lowerToken = token.toLowerCase();
      const sources = graphSuggestionSources();
      const field = graphSuggestionField(token);
      const fieldLabels = [
        { field: "tag", label: "tag:", detail: "filter by tag" },
        { field: "alias", label: "alias:", detail: "filter by alias" },
        { field: "path", label: "path:", detail: "filter by path" },
        { field: "title", label: "title:", detail: "filter by title" },
        { field: "group", label: "group:", detail: "filter by folder/group" },
        { field: "section", label: "section:", detail: "filter by section" },
      ];

      if (field) {
        return (sources[field.field] || [])
          .filter((value) => value.toLowerCase().includes(field.query))
          .slice(0, 10)
          .map((value) => ({
            field: field.field,
            label: field.sigil ? `${field.sigil}${value}` : `${field.field}:${value}`,
            detail: field.field,
            replacement: field.sigil
              ? `${field.sigil}${quoteSearchValue(value)}`
              : `${field.field}:${quoteSearchValue(value)}`,
          }));
      }

      const fieldSuggestions = fieldLabels
        .filter((item) => item.label.startsWith(lowerToken))
        .map((item) => ({
          ...item,
          replacement: item.label,
        }));

      if (!lowerToken) {
        return fieldSuggestions;
      }

      const valueSuggestions = [
        ...sources.tag
          .filter((value) => value.toLowerCase().includes(lowerToken))
          .slice(0, 5)
          .map((value) => ({
            field: "tag",
            label: `tag:${value}`,
            detail: "tag",
            replacement: `tag:${quoteSearchValue(value)}`,
          })),
        ...sources.alias
          .filter((value) => value.toLowerCase().includes(lowerToken))
          .slice(0, 3)
          .map((value) => ({
            field: "alias",
            label: `alias:${value}`,
            detail: "alias",
            replacement: `alias:${quoteSearchValue(value)}`,
          })),
        ...sources.path
          .filter((value) => value.toLowerCase().includes(lowerToken))
          .slice(0, 3)
          .map((value) => ({
            field: "path",
            label: `path:${value}`,
            detail: "path",
            replacement: `path:${quoteSearchValue(value)}`,
          })),
      ];

      return [...fieldSuggestions, ...valueSuggestions].slice(0, 12);
    }

    function closeToolbarSuggestions() {
      toolbarSuggestions = [];
      toolbarActiveSuggestionIndex = -1;
      if (toolbarSuggestionBox) {
        toolbarSuggestionBox.hidden = true;
        toolbarSuggestionBox.innerHTML = "";
      }
      if (toolbarSearchInput) {
        toolbarSearchInput.setAttribute("aria-expanded", "false");
      }
    }

    function setActiveToolbarSuggestion(index) {
      if (!toolbarSuggestionBox || toolbarSuggestions.length === 0) {
        toolbarActiveSuggestionIndex = -1;
        return;
      }

      toolbarActiveSuggestionIndex = (index + toolbarSuggestions.length) % toolbarSuggestions.length;
      toolbarSuggestionBox.querySelectorAll(".graph-toolbar-suggestion").forEach((button, buttonIndex) => {
        const active = buttonIndex === toolbarActiveSuggestionIndex;
        button.classList.toggle("is-active", active);
        button.setAttribute("aria-selected", active ? "true" : "false");
      });
    }

    function applyToolbarSuggestion(suggestion) {
      const tokenInfo = currentSearchToken();
      if (!tokenInfo || !suggestion || !toolbarSearchInput) {
        return;
      }

      const before = tokenInfo.value.slice(0, tokenInfo.start);
      const after = tokenInfo.value.slice(tokenInfo.end).replace(/^\s+/u, "");
      const needsTrailingSpace = suggestion.replacement.endsWith(":") ? "" : " ";
      const nextValue = `${before}${suggestion.replacement}${needsTrailingSpace}${after}`;
      const nextCursor = before.length + suggestion.replacement.length + needsTrailingSpace.length;

      toolbarSearchInput.value = nextValue;
      toolbarSearchInput.focus();
      toolbarSearchInput.setSelectionRange(nextCursor, nextCursor);
      closeToolbarSuggestions();
      applySearchFilter(nextValue);
    }

    function showToolbarSuggestions() {
      if (!toolbarSuggestionBox || !toolbarSearchInput) {
        return;
      }

      toolbarSuggestions = buildGraphSuggestions();
      toolbarActiveSuggestionIndex = -1;

      if (toolbarSuggestions.length === 0) {
        closeToolbarSuggestions();
        return;
      }

      toolbarSuggestionBox.innerHTML = toolbarSuggestions
        .map((suggestion, index) => `
          <button type="button" class="graph-toolbar-suggestion" data-index="${index}" role="option" aria-selected="false">
            <span>${escapeHtml(suggestion.label)}</span>
            <small>${escapeHtml(suggestion.detail)}</small>
          </button>
        `)
        .join("");
      toolbarSuggestionBox.hidden = false;
      toolbarSearchInput.setAttribute("aria-expanded", "true");

      toolbarSuggestionBox.querySelectorAll(".graph-toolbar-suggestion").forEach((button) => {
        button.addEventListener("mousedown", (event) => event.preventDefault());
        button.addEventListener("click", () => {
          applyToolbarSuggestion(toolbarSuggestions[Number(button.dataset.index)]);
        });
      });
    }

    function applySearchFilter(rawText) {
      const text = String(rawText || "").trim();
      if (!text) {
        setVisibleKeys(knowledge.notes.map((note) => note.key));
        if (toolbarStatus) toolbarStatus.textContent = `${knowledge.notes.length} notes`;
        return;
      }
      const matched = knowledge.filterNotes
        ? knowledge.filterNotes({ text, includeHidden: true })
        : knowledge.notes;
      setVisibleKeys(matched.map((note) => note.key));
      if (toolbarStatus) toolbarStatus.textContent = `${matched.length} notes`;
    }

    function buildToolbar() {
      if (!wantToolbar) {
        return;
      }
      if (window.getComputedStyle(container).position === "static") {
        container.style.position = "relative";
      }
      toolbarEl = document.createElement("div");
      toolbarEl.className = "graph-toolbar";
      toolbarEl.innerHTML = `
        <input type="search" class="graph-toolbar-input" placeholder="Search title, body, tag:, alias:, path:" autocomplete="off" aria-autocomplete="list" aria-expanded="false" />
        <span class="graph-toolbar-status">${knowledge.notes.length} notes</span>
        <button type="button" class="graph-toolbar-btn" data-action="center" title="Center on visible">Center</button>
        <button type="button" class="graph-toolbar-btn" data-action="reset" title="Reset filter">Reset</button>
        <div class="graph-toolbar-suggestions" role="listbox" hidden></div>
      `;
      container.appendChild(toolbarEl);

      toolbarSearchInput = toolbarEl.querySelector(".graph-toolbar-input");
      toolbarStatus = toolbarEl.querySelector(".graph-toolbar-status");
      toolbarSuggestionBox = toolbarEl.querySelector(".graph-toolbar-suggestions");
      toolbarSearchInput.addEventListener("input", (event) => {
        const value = event.target.value;
        window.clearTimeout(searchFilterTimer);
        searchFilterTimer = window.setTimeout(() => applySearchFilter(value), 140);
        showToolbarSuggestions();
      });
      toolbarSearchInput.addEventListener("keydown", (event) => {
        if (event.key === "ArrowDown" && toolbarSuggestions.length > 0) {
          event.preventDefault();
          setActiveToolbarSuggestion(toolbarActiveSuggestionIndex + 1);
          return;
        }
        if (event.key === "ArrowUp" && toolbarSuggestions.length > 0) {
          event.preventDefault();
          setActiveToolbarSuggestion(toolbarActiveSuggestionIndex - 1);
          return;
        }
        if (event.key === "Enter" && toolbarActiveSuggestionIndex >= 0) {
          event.preventDefault();
          applyToolbarSuggestion(toolbarSuggestions[toolbarActiveSuggestionIndex]);
          return;
        }
        if (event.key === "Escape") {
          event.preventDefault();
          if (toolbarSuggestionBox && !toolbarSuggestionBox.hidden) {
            closeToolbarSuggestions();
            return;
          }
          toolbarSearchInput.value = "";
          applySearchFilter("");
        }
      });
      toolbarSearchInput.addEventListener("focus", showToolbarSuggestions);
      if (toolbarClickListener) {
        document.removeEventListener("click", toolbarClickListener);
      }
      toolbarClickListener = (event) => {
        if (toolbarEl && !toolbarEl.contains(event.target)) {
          closeToolbarSuggestions();
        }
      };
      document.addEventListener("click", toolbarClickListener);

      toolbarEl.querySelector('[data-action="center"]').addEventListener("click", () => {
        refitGraph((node) => isNodeVisible(node), false);
      });
      toolbarEl.querySelector('[data-action="reset"]').addEventListener("click", () => {
        if (toolbarSearchInput) toolbarSearchInput.value = "";
        closeToolbarSuggestions();
        applySearchFilter("");
        refitGraph((node) => isNodeVisible(node), true);
      });
    }

    function buildGraph() {
      container.innerHTML = "";
      buildToolbar();
      ({ width, height } = measureContainerSize());

      svg = d3.select(container)
        .append("svg")
        .attr("width", width)
        .attr("height", height)
        .attr("viewBox", [0, 0, width, height]);

      canvas = svg.append("g");

      zoomBehavior = d3.zoom().scaleExtent([0.35, 3]).on("zoom", (event) => {
        canvas.attr("transform", event.transform);
      });

      svg.call(zoomBehavior);
      svg.on("click", () => setSelected(null, { dispatch: false }));

      simulation = d3.forceSimulation(nodes)
        .force("link", d3.forceLink(links).id((node) => node.id).distance((link) => (link.kind === "reference" ? 96 : 68)))
        .force("charge", d3.forceManyBody().strength((node) => (node.kind === "tag" ? -340 : -460)))
        .force("center", d3.forceCenter(width / 2, height / 2))
        .force("collide", d3.forceCollide().radius((node) => (node.kind === "tag" ? 18 : 22)))
        .alpha(1)
        .alphaDecay(0.06);

      linkSelection = canvas.append("g")
        .attr("class", "graph-links")
        .selectAll("line")
        .data(links)
        .join("line");

      nodeSelection = canvas.append("g")
        .attr("class", "graph-nodes")
        .selectAll("circle")
        .data(nodes)
        .join("circle")
        .attr("tabindex", 0)
        .attr("cursor", "pointer")
        .on("click", (event, node) => {
          event.stopPropagation();

          if (node.kind === "tag" && dispatchTagEvents) {
            dispatchEvent("knowledge:apply-tag", {
              tag: node.label,
            });
          }

          setSelected(node);
        })
        .on("dblclick", (event, node) => {
          event.stopPropagation();
          if (node.kind !== "note") {
            return;
          }
          if (onNoteOpen) {
            onNoteOpen(node.note);
          } else {
            window.location.href = buildNoteHref(node.note);
          }
        })
        .call(
          d3.drag()
            .on("start", (event) => {
              if (!event.active) simulation.alphaTarget(0.18).restart();
              event.subject.fx = event.subject.x;
              event.subject.fy = event.subject.y;
            })
            .on("drag", (event) => {
              event.subject.fx = event.x;
              event.subject.fy = event.y;
            })
            .on("end", (event) => {
              if (!event.active) simulation.alphaTarget(0);
              event.subject.fx = null;
              event.subject.fy = null;
            }),
        );

      labelSelection = canvas.append("g")
        .attr("class", "graph-labels")
        .selectAll("text")
        .data(nodes)
        .join("text")
        .text((node) => node.label)
        .attr("font-size", (node) => (node.kind === "tag" ? 10 : 11))
        .attr("font-weight", (node) => (node.kind === "tag" ? 500 : 600))
        .attr("pointer-events", "none");

      simulation.on("tick", () => {
        linkSelection
          .attr("x1", (link) => link.source.x)
          .attr("y1", (link) => link.source.y)
          .attr("x2", (link) => link.target.x)
          .attr("y2", (link) => link.target.y);

        nodeSelection
          .attr("cx", (node) => node.x)
          .attr("cy", (node) => node.y);

        labelSelection
          .attr("x", (node) => node.x + (node.kind === "tag" ? 11 : 13))
          .attr("y", (node) => node.y + 4);
      });

      applyStyles();
      updateFocusPanel(null);

      const initialSelectedId = String(options.initialSelectedId || "");
      if (initialSelectedId && nodeById.has(initialSelectedId)) {
        window.setTimeout(() => {
          setSelected(nodeById.get(initialSelectedId), { dispatch: false });
          refitGraph((node) => {
            if (!isNodeVisible(node)) {
              return false;
            }

            if (node.id === initialSelectedId) {
              return true;
            }

            return (adjacency.get(initialSelectedId) || new Set()).has(node.id);
          }, false);
        }, 240);
      } else {
        window.setTimeout(() => refitGraph((node) => isNodeVisible(node), true), 240);
      }
    }

    function resizeGraph() {
      if (!simulation || !svg) {
        return;
      }

      const nextSize = measureContainerSize();
      if (nextSize.width === width && nextSize.height === height) {
        return;
      }

      width = nextSize.width;
      height = nextSize.height;
      svg.attr("width", width).attr("height", height);
      svg.attr("viewBox", [0, 0, width, height]);
      simulation.force("center", d3.forceCenter(width / 2, height / 2));
      simulation.alpha(0.4).restart();
    }

    function setVisibleKeys(nextKeys, { refit = true } = {}) {
      visibleKeys.clear();
      (Array.isArray(nextKeys) ? nextKeys : []).forEach((key) => {
        if (nodeById.has(key)) {
          visibleKeys.add(key);
        }
      });

      if (!visibleKeys.has(selectedId) && selectedId && !String(selectedId).startsWith("tag:")) {
        setSelected(null, { dispatch: false });
      } else {
        applyStyles();
      }

      if (refit) {
        refitGraph((node) => isNodeVisible(node), false);
      }
    }

    function selectById(id, { fit = true, dispatch = false } = {}) {
      const node = nodeById.get(id);
      if (!node) {
        return;
      }

      setSelected(node, { dispatch });

      if (fit) {
        refitGraph((candidate) => {
          if (!isNodeVisible(candidate)) {
            return false;
          }

          if (candidate.id === id) {
            return true;
          }

          return (adjacency.get(id) || new Set()).has(candidate.id);
        }, false);
      }
    }

    function recenter() {
      refitGraph((node) => isNodeVisible(node), false);
    }

    const resizeObserver = new ResizeObserver(() => {
      if (resizeFrame) {
        window.cancelAnimationFrame(resizeFrame);
      }
      resizeFrame = window.requestAnimationFrame(() => {
        resizeFrame = 0;
        resizeGraph();
      });
    });
    resizeObserver.observe(container);

    let filtersListener = null;
    if (listenForGlobalFilters) {
      filtersListener = (event) => {
        setVisibleKeys(
          event.detail && Array.isArray(event.detail.visibleKeys)
            ? event.detail.visibleKeys
            : knowledge.notes.map((note) => note.key),
          { refit: false },
        );

        window.clearTimeout(refitGraph.timerId);
        refitGraph.timerId = window.setTimeout(() => {
          refitGraph((node) => isNodeVisible(node), false);
        }, 120);
      };

      document.addEventListener("knowledge:filters-changed", filtersListener);
    }

    buildGraph();

    const api = {
      container,
      focusPanel,
      setVisibleKeys,
      selectById,
      recenter,
      destroy() {
        resizeObserver.disconnect();
        if (resizeFrame) {
          window.cancelAnimationFrame(resizeFrame);
        }
        if (filtersListener) {
          document.removeEventListener("knowledge:filters-changed", filtersListener);
        }
        if (toolbarClickListener) {
          document.removeEventListener("click", toolbarClickListener);
          toolbarClickListener = null;
        }
        if (simulation) {
          simulation.stop();
        }
        delete container._knowledgeGraph;
        delete container.dataset.graphMounted;
      },
    };

    container.dataset.graphMounted = "true";
    container._knowledgeGraph = api;
    return api;
  }

  function injectToolbarStyles() {
    if (document.getElementById("graph-toolbar-styles")) {
      return;
    }
    const style = document.createElement("style");
    style.id = "graph-toolbar-styles";
    style.textContent = `
      .graph-toolbar {
        position: absolute; top: 8px; left: 8px; z-index: 5;
        display: flex; gap: 6px; align-items: center;
        max-width: calc(100% - 16px);
        background: rgba(255,255,255,0.92);
        border: 1px solid rgba(0,0,0,0.12);
        border-radius: 4px;
        padding: 4px 6px;
        font: 12px system-ui, -apple-system, "Helvetica Neue", sans-serif;
        box-shadow: 0 1px 3px rgba(0,0,0,0.08);
      }
      .graph-toolbar-input {
        border: 1px solid rgba(0,0,0,0.18);
        border-radius: 3px;
        padding: 3px 6px;
        font: inherit;
        outline: none;
        width: min(320px, 42vw);
      }
      .graph-toolbar-input:focus { border-color: #5a79c7; }
      .graph-toolbar-status {
        color: rgba(0,0,0,0.58);
        white-space: nowrap;
        font-variant-numeric: tabular-nums;
      }
      .graph-toolbar-btn {
        border: 1px solid rgba(0,0,0,0.18);
        background: #f7f7f5;
        border-radius: 3px;
        padding: 3px 8px;
        cursor: pointer;
        font: inherit;
      }
      .graph-toolbar-btn:hover { background: #ebebe8; }
      .graph-toolbar-btn:active { background: #dcdcd8; }
      .graph-toolbar-suggestions {
        position: absolute;
        top: calc(100% + 4px);
        left: 4px;
        width: min(360px, calc(100vw - 24px));
        max-height: min(320px, calc(100vh - 96px));
        overflow: auto;
        background: rgba(255,255,255,0.98);
        border: 1px solid rgba(0,0,0,0.14);
        border-radius: 4px;
        box-shadow: 0 8px 24px rgba(0,0,0,0.16);
        padding: 4px;
      }
      .graph-toolbar-suggestion {
        display: flex;
        align-items: baseline;
        justify-content: space-between;
        gap: 12px;
        width: 100%;
        border: 0;
        border-radius: 3px;
        background: transparent;
        color: inherit;
        cursor: pointer;
        font: inherit;
        padding: 5px 7px;
        text-align: left;
      }
      .graph-toolbar-suggestion small {
        color: rgba(0,0,0,0.48);
        flex: 0 0 auto;
      }
      .graph-toolbar-suggestion:hover,
      .graph-toolbar-suggestion.is-active {
        background: #eceff7;
      }
      @media (max-width: 560px) {
        .graph-toolbar {
          right: 8px;
          flex-wrap: wrap;
        }
        .graph-toolbar-input {
          width: 100%;
          min-width: 0;
        }
        .graph-toolbar-suggestions {
          right: 4px;
          width: auto;
        }
      }
    `;
    document.head.appendChild(style);
  }

  injectToolbarStyles();

  window.initKnowledgeGraph = initKnowledgeGraph;

  document.addEventListener("DOMContentLoaded", () => {
    if (window.__GRAPH_NO_AUTO_INIT__) {
      return;
    }
    initKnowledgeGraph();
  });
})();
