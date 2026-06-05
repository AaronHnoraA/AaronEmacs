import { describe, expect, test } from "@voidzero-dev/vite-plus-test";
// @ts-ignore Node ESM helper is outside the TS app graph.
import { jupyterLabUrl, jupyterLaunchArgs, jupyterSelectorPath, mergeJupyterEnv, parseNulEnv } from "../desktop/jupyter.mjs";

describe("jupyter desktop helpers", () => {
  test("builds jupyter-lab launch args without shell concatenation", () => {
    const args = jupyterLaunchArgs({
      command: "/opt/homebrew/bin/jupyter-lab",
      root: "/notes",
      port: 8899,
      token: "tok",
    });
    expect(args).toContain("--no-browser");
    expect(args).toContain("--ServerApp.port=8899");
    expect(args).toContain("--ServerApp.allow_origin=*");
    expect(args).toContain("--ServerApp.root_dir=/notes");
    expect(args.every((arg: unknown) => typeof arg === "string")).toBe(true);
  });

  test("disables all server auth so the cross-origin iframe kernel can connect", () => {
    const args = jupyterLaunchArgs({
      command: "/opt/homebrew/bin/jupyter-lab",
      root: "/notes",
      port: 8899,
      token: "tok",
    });
    // No auth token is emitted regardless of the passed token; both token traits and
    // the password are empty, leaving the local 127.0.0.1 server authentication-free.
    expect(args).toContain("--ServerApp.token=");
    expect(args).toContain("--IdentityProvider.token=");
    expect(args).toContain("--ServerApp.password=");
    expect(args).toContain("--ServerApp.disable_check_xsrf=True");
    expect(args).not.toContain("--ServerApp.token=tok");
  });

  test("uses the lab subcommand for the generic jupyter executable", () => {
    expect(jupyterLaunchArgs({ command: "/usr/local/bin/jupyter", root: "/notes", port: 8899, token: "tok" })[0]).toBe("lab");
  });

  test("builds notebook URLs with direct toc selectors", () => {
    expect(jupyterLabUrl({
      baseUrl: "http://127.0.0.1:8899",
      root: "/notes",
      file: "/notes/experiments/demo notebook.ipynb",
      token: "tok",
      selectorKind: "toc",
      selector: "Model head 1",
    })).toBe("http://127.0.0.1:8899/lab/tree/experiments/demo%20notebook.ipynb?token=tok#Model%20head%201");
    expect(jupyterLabUrl({
      baseUrl: "http://127.0.0.1:8899",
      root: "/notes",
      file: "/notes/experiments/demo.ipynb",
      token: "tok",
      selectorKind: "toc",
      selector: "4",
    })).toBe("http://127.0.0.1:8899/lab/tree/experiments/demo.ipynb?token=tok#4");
  });

  test("keeps jupyter toc selectors single-level", () => {
    expect(jupyterSelectorPath(" Parent @ Child ")).toBe("Parent");
    expect(jupyterLabUrl({
      baseUrl: "http://127.0.0.1:8899",
      root: "/notes",
      file: "/notes/experiments/demo.ipynb",
      token: "tok",
      selectorKind: "toc",
      selector: "Parent@Child",
    })).toBe("http://127.0.0.1:8899/lab/tree/experiments/demo.ipynb?token=tok#Parent");
  });

  test("parses zsh env output for launching jupyter", () => {
    expect(parseNulEnv("PATH=/zsh/bin\0PYENV_ROOT=/pyenv\0EMPTY=\0broken\0")).toEqual({
      PATH: "/zsh/bin",
      PYENV_ROOT: "/pyenv",
      EMPTY: "",
    });
  });

  test("merges zsh env while preserving explicit aaronnote overrides", () => {
    expect(mergeJupyterEnv(
      { PATH: "/gui/bin", AARONNOTE_JUPYTER: "/custom/jupyter" },
      { PATH: "/zsh/bin", AARONNOTE_JUPYTER: "/zsh/jupyter", PYENV_ROOT: "/pyenv" },
    )).toEqual({
      PATH: "/zsh/bin",
      AARONNOTE_JUPYTER: "/custom/jupyter",
      PYENV_ROOT: "/pyenv",
    });
  });
});
