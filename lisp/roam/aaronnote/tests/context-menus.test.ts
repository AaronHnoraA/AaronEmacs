import { describe, expect, test } from "@voidzero-dev/vite-plus-test";

// @ts-ignore Desktop menu templates are plain ESM consumed by Electron.
import { attachmentContextMenuTemplate, editorContextMenuTemplate } from "../desktop/context-menus.mjs";

type MenuItem = {
  label?: string;
  role?: string;
  type?: string;
  submenu?: MenuItem[];
  click?: () => void;
};

function labels(items: MenuItem[]): string[] {
  return items.flatMap((item) => [
    item.label || item.role || item.type || "",
    ...(item.submenu ? labels(item.submenu) : []),
  ]);
}

describe("desktop context menu templates", () => {
  test("builds editing, format, link, table, and roam actions", () => {
    const calls: Array<{ command: string; detail: Record<string, unknown> }> = [];
    const menu = editorContextMenuTemplate({
      linkHref: "target.md",
      x: 10,
      y: 20,
      hasSelection: true,
      allowRoamIdlink: true,
      blockType: "table_cell",
      blockCommands: ["table-insert-row", "table-insert-column", "table-delete-row", "table-delete-column"],
    }, {
      command: (command: string, detail: Record<string, unknown> = {}) => calls.push({ command, detail }),
    }) as MenuItem[];

    expect(labels(menu)).toEqual(expect.arrayContaining([
      "Link",
      "Preview Link",
      "Open Link in New Window",
      "Copy Link Address",
      "Table",
      "Insert Row Below",
      "Delete Current Column",
      "undo",
      "copy",
      "Copy as Markdown",
      "Paste as Plain Text",
      "Format",
      "Highlight",
      "Strikethrough",
      "Find Selected Text",
      "Insert Roam Idlink...",
    ]));

    menu.find((item) => item.label === "Link")?.submenu
      ?.find((item) => item.label === "Open Link in New Window")?.click?.();
    expect(calls).toContainEqual({ command: "open-link", detail: { href: "target.md", newWindow: true } });
  });

  test("shows code copy without roam selection actions when unavailable", () => {
    const menu = editorContextMenuTemplate({
      blockType: "code_block",
      hasSelection: false,
      allowRoamIdlink: false,
    }, { command: () => {} }) as MenuItem[];
    const all = labels(menu);
    expect(all).toContain("Copy Code");
    expect(all).not.toContain("Find Selected Text");
    expect(all).not.toContain("Insert Roam Idlink...");
  });

  test("builds complete attachment actions and gates Jupyter preview", () => {
    const menu = attachmentContextMenuTemplate({
      file: "/tmp/demo.ipynb",
      href: "./attachments/demo.ipynb",
      jupyter: true,
    }, {
      command: () => {},
      open: () => {},
      reveal: () => {},
      copy: () => {},
    }) as MenuItem[];
    expect(labels(menu)).toEqual(expect.arrayContaining([
      "Open Jupyter Preview",
      "System Open",
      "Show in Finder",
      "Copy Markdown Path",
      "Copy File Path",
    ]));

    const plain = attachmentContextMenuTemplate({
      file: "/tmp/demo.pdf",
      href: "./attachments/demo.pdf",
    }, {
      command: () => {},
      open: () => {},
      reveal: () => {},
      copy: () => {},
    }) as MenuItem[];
    expect(labels(plain)).not.toContain("Open Jupyter Preview");
  });
});
