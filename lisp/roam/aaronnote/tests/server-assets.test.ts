import { afterEach, describe, expect, test } from "@voidzero-dev/vite-plus-test";
import { mkdtemp, mkdir, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";

// @ts-ignore The server is a Node ESM module outside the TS app graph.
import { assetRefsFromContent, storeAssetFromPath } from "../server/lib/assets.mjs";
// @ts-ignore The server is a Node ESM module outside the TS app graph.
import { configure } from "../server/lib/state.mjs";

const noteRoot = decodeURIComponent(new URL("../../roam", import.meta.url).pathname.replace(/^\/@fs/, "").replace(/\/$/, ""));
const roots: string[] = [];

afterEach(async () => {
  await Promise.all(roots.splice(0).map((root) => rm(root, { recursive: true, force: true })));
});

describe("server asset refs", () => {
  test("extracts markdown image and attachment paths relative to note file", () => {
    const note = `${noteRoot}/project/a.md`;
    expect(
      assetRefsFromContent("![plot](./images/a/plot.png)\n[file](attachments/a/raw%20data.pdf)", note),
    ).toEqual([
      `${noteRoot}/project/images/a/plot.png`,
      `${noteRoot}/project/attachments/a/raw data.pdf`,
    ]);
  });

  test("ignores external asset URLs", () => {
    expect(
      assetRefsFromContent("![remote](https://example.com/a.png)\n<a href=\"mailto:x@y.z\">x</a>", `${noteRoot}/a.md`),
    ).toEqual([]);
  });

  test("copies native asset paths without base64 encoding", async () => {
    const root = await mkdtemp(join(tmpdir(), "aaronnote-assets-"));
    roots.push(root);
    const notes = join(root, "roam");
    const loose = join(root, "loose");
    await mkdir(notes, { recursive: true });
    await mkdir(loose, { recursive: true });
    const note = join(notes, "topic.md");
    const source = join(loose, "plot.png");
    await writeFile(note, "# Topic\n", "utf8");
    await writeFile(source, "PNGDATA", "utf8");
    configure({ root: notes, workspaceRoot: root, pluginRoot: join(root, "plugin") });

    const msg = await storeAssetFromPath({
      file: note,
      path: source,
      name: "plot.png",
      type: "image/png",
    });

    expect(msg.ok).toBe(true);
    expect(msg.isImage).toBe(true);
    expect(msg.markdownPath).toBe("./images/topic/plot.png");
    expect(await readFile(join(notes, "images", "topic", "plot.png"), "utf8")).toBe("PNGDATA");
  });
});
