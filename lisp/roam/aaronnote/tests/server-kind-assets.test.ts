import { beforeEach, describe, expect, test } from "@voidzero-dev/vite-plus-test";
import { mkdir, mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

// @ts-ignore The server is a Node ESM module outside the TS app graph.
import { activeKindFromContent, kindFromContent, scanSnippets } from "../server/lib/index.mjs";
// @ts-ignore The server is a Node ESM module outside the TS app graph.
import { configure } from "../server/lib/state.mjs";

type ServerSnippet = {
  key?: string;
  mode?: string;
  kind?: string;
  body?: string;
  source?: string;
};

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const workspaceRoot = resolve(repoRoot, "..");

function resetRuntimeConfig() {
  configure({
    root: join(workspaceRoot, "roam"),
    workspaceRoot,
    pluginRoot: join(workspaceRoot, "plugin"),
  });
}

describe("server note kind assets", () => {
  beforeEach(resetRuntimeConfig);

  test("reads active kind from org meta", () => {
    expect(kindFromContent("#+begin meta\nkind: slides\n#+end meta\n\n# Talk")).toBe("slides");
    expect(activeKindFromContent("#+begin meta\nkind: slides\n#+end meta")).toBe("slides");
  });

  test("reads active kind from YAML front matter", () => {
    expect(kindFromContent("---\nkind: assignment\n---\n\n# Work")).toBe("assignment");
  });

  test("normalizes default and unsafe kinds", () => {
    expect(kindFromContent("")).toBe("default");
    expect(activeKindFromContent("#+begin meta\nkind: default\n#+end meta")).toBe("");
    expect(kindFromContent("#+begin meta\nkind: note\n#+end meta")).toBe("default");
    expect(kindFromContent("#+begin meta\nkind: ../slides\n#+end meta")).toBe("default");
  });

  test("loads kind-specific snippets from kinds/name/snippet", async () => {
    const snippets = await scanSnippets({ force: true }) as ServerSnippet[];
    const slide = snippets.find((snippet) => snippet.kind === "slides" && snippet.key === "slide");
    expect(slide?.mode).toBe("markdown-mode");
    expect(slide?.body).toContain("# ${1:Slide title}");
  });

  test("loads the html org env snippet", async () => {
    const snippets = await scanSnippets({ force: true }) as ServerSnippet[];
    const html = snippets.find((snippet) => snippet.kind === "" && snippet.key === "html");
    expect(html?.mode).toBe("markdown-mode");
    expect(html?.body).toContain("#+begin html\n$1\n#+end html");
  });

  test("loads the tikz org env snippet", async () => {
    const snippets = await scanSnippets({ force: true }) as ServerSnippet[];
    const tikz = snippets.find((snippet) => snippet.kind === "" && snippet.key === "tikz");
    expect(tikz?.mode).toBe("markdown-mode");
    expect(tikz?.body).toContain("#+ begin tikz");
    expect(tikz?.body).toContain("\\draw[->]");
    expect(tikz?.body).toContain("#+ end tikz");
  });

  test("strips duplicate snippet body delimiters", async () => {
    const snippets = await scanSnippets({ force: true }) as ServerSnippet[];
    const mat = snippets.find((snippet) => snippet.kind === "" && snippet.mode === "tex-mode" && snippet.key === "mat");
    expect(mat?.body?.startsWith("# --")).toBe(false);
    expect(mat?.body).toContain("\\begin{${1:p/b/v/V/B/small}matrix}");
  });

  test("prefers live workspace snippets over bundled snippets", async () => {
    const temp = await mkdtemp(join(tmpdir(), "aaronnote-snippets-"));
    try {
      const snippetFile = join(temp, "Aaronnote", "snippets", "markdown-mode", "html");
      await mkdir(dirname(snippetFile), { recursive: true });
      await writeFile(snippetFile, [
        "# -*- mode: snippet -*-",
        "# name: Live HTML",
        "# key: html",
        "# --",
        "live workspace snippet",
      ].join("\n"), "utf8");

      configure({ root: join(temp, "roam"), workspaceRoot: temp, pluginRoot: join(temp, "plugin") });
      const snippets = await scanSnippets({ force: true }) as ServerSnippet[];
      const html = snippets.find((snippet) => snippet.kind === "" && snippet.mode === "markdown-mode" && snippet.key === "html");
      const definition = snippets.find((snippet) => snippet.kind === "" && snippet.mode === "markdown-mode" && snippet.key === "def");
      expect(html?.source).toBe(snippetFile);
      expect(html?.body).toBe("live workspace snippet");
      expect(definition?.source).toBe(join(repoRoot, "snippets", "markdown-mode", "definition"));
    } finally {
      resetRuntimeConfig();
      await rm(temp, { recursive: true, force: true });
    }
  });

  test("falls back to bundled snippets when no live workspace snippets exist", async () => {
    const temp = await mkdtemp(join(tmpdir(), "aaronnote-no-live-snippets-"));
    try {
      configure({ root: join(temp, "roam"), workspaceRoot: temp, pluginRoot: join(temp, "plugin") });
      const snippets = await scanSnippets({ force: true }) as ServerSnippet[];
      const html = snippets.find((snippet) => snippet.kind === "" && snippet.mode === "markdown-mode" && snippet.key === "html");
      expect(html?.source).toBe(join(repoRoot, "snippets", "markdown-mode", "html"));
      expect(html?.body).toContain("#+begin html");
    } finally {
      resetRuntimeConfig();
      await rm(temp, { recursive: true, force: true });
    }
  });
});
