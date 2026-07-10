import { afterEach, describe, expect, test } from "@voidzero-dev/vite-plus-test";
import { mkdtemp, mkdir, rm, symlink, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";

// @ts-ignore The server module is Node ESM outside the TS app graph.
import { bibliographyCompletions, bibliographyForDocument, configureBibliography, parseBibTeX } from "../server/lib/bibliography.mjs";

const roots: string[] = [];

afterEach(async () => {
  await Promise.all(roots.splice(0).map((root) => rm(root, { recursive: true, force: true })));
});

describe("bibliography", () => {
  test("parses braced BibTeX values with apostrophes", () => {
    const parsed = parseBibTeX([
      "@string{crelle = {Journal für die reine und angewandte Mathematik (Crelle's Journal)}}",
      "@article{Str87,",
      "  author = {Strassen, Volker},",
      "  title = {Relative Bilinear Complexity and Matrix Multiplication},",
      "  journal = crelle,",
      "  year = {1987},",
      "  pages = {406-443},",
      "  doi = {10.1515/crll.1987.375-376.406}",
      "}",
    ].join("\n"));

    expect(parsed.diagnostics).toEqual([]);
    expect(parsed.entries[0]?.key).toBe("Str87");
    expect(parsed.entries[0]?.fields.journal).toContain("Crelle's Journal");
  });

  test("indexes only declared local bib directories and resolves citations", async () => {
    const root = await mkdtemp(join(tmpdir(), "aaronnote-bib-"));
    roots.push(root);
    const noteDir = join(root, "project", "iso");
    await mkdir(join(noteDir, "bib"), { recursive: true });
    await writeFile(join(noteDir, "bib", "iso.bib"), [
      "@article{Str87,",
      "  author = {Strassen, Volker},",
      "  title = {Relative Bilinear Complexity and Matrix Multiplication},",
      "  journal = {Journal für die reine und angewandte Mathematik (Crelle's Journal)},",
      "  year = {1987},",
      "  pages = {406-443},",
      "  doi = {10.1515/crll.1987.375-376.406}",
      "}",
    ].join("\n"), "utf8");
    const file = join(noteDir, "GraphTensor.md");
    const content = [
      "#+begin meta",
      "title: Graph Tensor",
      "bib: ./bib",
      "#+end meta",
      "",
      "As in @@cite(iso) [Str87] {locator: p. 406}.",
    ].join("\n");
    await writeFile(file, content, "utf8");

    configureBibliography({ root });
    const result = await bibliographyForDocument({ file, content });
    expect(result.diagnostics).toEqual([]);
    expect(result.citations?.[0]?.diagnostics).toEqual([]);
    expect(result.references).toHaveLength(1);
    expect(result.references?.[0]?.text).toContain("Volker Strassen");

    const namespaces = await bibliographyCompletions({ file, content, kind: "namespaces" });
    expect(namespaces.items?.find((item: { key?: string }) => item.key === "iso")).toMatchObject({
      body: "iso",
      detail: "project/iso/bib/iso.bib",
    });

    const keys = await bibliographyCompletions({ file, content, kind: "keys", namespace: "iso" });
    expect(keys.items?.find((item: { key?: string }) => item.key === "Str87")).toMatchObject({
      body: "Str87",
      source: "project/iso/bib/iso.bib",
    });
  });

  test("resolves a real note path when the configured Aaronnote root is a symlink", async () => {
    const container = await mkdtemp(join(tmpdir(), "aaronnote-bib-symlink-"));
    roots.push(container);
    const realRoot = join(container, "AaronNote");
    const linkedRoot = join(container, ".roam");
    const noteDir = join(realRoot, "project", "iso");
    await mkdir(join(noteDir, "bib"), { recursive: true });
    await symlink(realRoot, linkedRoot, "dir");
    await writeFile(join(noteDir, "bib", "iso.bib"), "@article{Str87, author={Strassen, Volker}, title={Relative Bilinear Complexity}, year={1987}}", "utf8");
    const file = join(noteDir, "GraphTensor.md");
    const content = [
      "#+begin meta",
      "bib: ./bib",
      "#+end meta",
      "",
      "As in @@cite(iso) [Str87].",
    ].join("\n");
    await writeFile(file, content, "utf8");

    configureBibliography({ root: linkedRoot });
    const result = await bibliographyForDocument({ file, content });

    expect(result.diagnostics).toEqual([]);
    expect(result.citations?.[0]?.diagnostics).toEqual([]);
    expect(result.references).toHaveLength(1);
    expect(result.references?.[0]?.entry?.path).toBe("project/iso/bib/iso.bib");
  });
});
