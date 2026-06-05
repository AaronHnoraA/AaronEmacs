function compactMenu(items) {
  const compact = [];
  for (const item of items) {
    if (!item) continue;
    if (item.type === "separator" && (compact.length === 0 || compact.at(-1)?.type === "separator")) continue;
    compact.push(item);
  }
  if (compact.at(-1)?.type === "separator") compact.pop();
  return compact;
}

function commandItem(label, actions, command, detail = {}) {
  return {
    label,
    click: () => actions.command(command, detail),
  };
}

export function editorContextMenuTemplate(options = {}, actions) {
  const href = String(options.linkHref || "");
  const tableCommands = new Set(Array.isArray(options.blockCommands) ? options.blockCommands : []);
  const contextItems = [
    ...(href ? [{
      label: "Link",
      submenu: [
        commandItem("Preview Link", actions, "preview-link", { href, x: options.x, y: options.y }),
        commandItem("Open Link", actions, "open-link", { href }),
        commandItem("Open Link in New Window", actions, "open-link", { href, newWindow: true }),
        { type: "separator" },
        commandItem("Copy Link Address", actions, "copy-text", { text: href }),
      ],
    }] : []),
    ...(options.blockType === "table_cell" ? [{
      label: "Table",
      submenu: [
        tableCommands.has("table-insert-row") && commandItem("Insert Row Below", actions, "editor-command", { editorCommand: "table-insert-row" }),
        tableCommands.has("table-insert-column") && commandItem("Insert Column Right", actions, "editor-command", { editorCommand: "table-insert-column" }),
        tableCommands.has("table-delete-row") && commandItem("Delete Current Row", actions, "editor-command", { editorCommand: "table-delete-row" }),
        tableCommands.has("table-delete-column") && commandItem("Delete Current Column", actions, "editor-command", { editorCommand: "table-delete-column" }),
      ].filter(Boolean),
    }] : []),
    ...(options.blockType === "code_block" ? [
      commandItem("Copy Code", actions, "editor-command", { editorCommand: "copy-code" }),
    ] : []),
  ];

  const noteItems = [
    ...(options.hasSelection ? [commandItem("Find Selected Text", actions, "find")] : []),
    ...(options.hasSelection && options.allowRoamIdlink
      ? [commandItem("Insert Roam Idlink...", actions, "insert-roam-idlink")]
      : []),
    commandItem("Check Spelling and Prose", actions, "check-prose"),
  ];

  return compactMenu([
    ...contextItems,
    contextItems.length && { type: "separator" },
    { role: "undo" },
    { role: "redo" },
    { type: "separator" },
    { role: "cut" },
    { role: "copy" },
    commandItem("Copy as Markdown", actions, "copy-markdown"),
    { role: "paste" },
    commandItem("Paste as Plain Text", actions, "paste-plain-text"),
    { role: "selectAll" },
    { type: "separator" },
    {
      label: "Format",
      submenu: [
        commandItem("Bold", actions, "editor-command", { editorCommand: "bold" }),
        commandItem("Italic", actions, "editor-command", { editorCommand: "italic" }),
        commandItem("Highlight", actions, "editor-command", { editorCommand: "highlight" }),
        commandItem("Strikethrough", actions, "editor-command", { editorCommand: "strike" }),
        commandItem("Inline Code", actions, "editor-command", { editorCommand: "code" }),
        commandItem("Link", actions, "editor-command", { editorCommand: "link" }),
      ],
    },
    { type: "separator" },
    ...noteItems,
  ]);
}

export function attachmentContextMenuTemplate(options = {}, actions) {
  const href = String(options.href || "");
  const file = String(options.file || "");
  return compactMenu([
    ...(options.jupyter ? [commandItem("Open Jupyter Preview", actions, "open-jupyter-preview", { href })] : []),
    options.jupyter && { type: "separator" },
    { label: "System Open", click: actions.open },
    { label: "Show in Finder", click: actions.reveal },
    { type: "separator" },
    { label: "Copy Markdown Path", click: () => actions.copy(href) },
    { label: "Copy File Path", click: () => actions.copy(file) },
  ]);
}
