import { chmod, mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { describe, expect, test } from "@voidzero-dev/vite-plus-test";
// @ts-ignore Desktop helper is plain ESM consumed by Electron.
import { findExternalEditorExecutables, neovideOpenFileCommand } from "../desktop/external-editor.mjs";

describe("external editor (Neovide/Nvim) command", () => {
  test("builds an argv array opening Neovide at a 1-based cursor", () => {
    const { command, args } = neovideOpenFileCommand({
      neovide: "/opt/homebrew/bin/neovide",
      nvim: "/opt/homebrew/bin/nvim",
      file: "/proj/notes/.lean/foo.lean",
      cwd: "/proj/notes/.lean",
      line: 12,
      character: 4,
    });
    expect(command).toBe("/opt/homebrew/bin/neovide");
    expect(args).toEqual([
      "--reuse-instance",
      "--neovim-bin", "/opt/homebrew/bin/nvim",
      "--chdir", "/proj/notes/.lean",
      "--",
      "+call cursor(13, 5)",
      "--",
      "/proj/notes/.lean/foo.lean",
    ]);
  });

  test("clamps and defaults negative/missing positions to line 1 col 1", () => {
    const { args } = neovideOpenFileCommand({
      neovide: "neovide",
      nvim: "nvim",
      file: "/a/b.lean",
      line: -4,
    });
    expect(args).toContain("+call cursor(1, 1)");
    expect(args.every((a: unknown) => typeof a === "string")).toBe(true);
  });

  test("finds explicit overrides before PATH entries", async () => {
    const root = await mkdtemp(join(tmpdir(), "aaronnote-neovide-bin-"));
    const explicitNeovide = join(root, "neovide-explicit");
    const explicitNvim = join(root, "nvim-explicit");
    const pathNeovide = join(root, "neovide");
    const pathNvim = join(root, "nvim");
    for (const file of [explicitNeovide, explicitNvim, pathNeovide, pathNvim]) {
      await writeFile(file, "#!/bin/sh\n", "utf8");
      await chmod(file, 0o755);
    }
    expect(findExternalEditorExecutables({
      env: {
        AARONNOTE_NEOVIDE: explicitNeovide,
        AARONNOTE_NVIM: explicitNvim,
        PATH: root,
      },
      preferredDirs: [],
    })).toEqual({ neovide: explicitNeovide, nvim: explicitNvim });
  });
});
