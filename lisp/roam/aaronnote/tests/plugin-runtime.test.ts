import { describe, expect, test } from "@voidzero-dev/vite-plus-test";

import { normalizePluginOverrideMap, pluginShouldRun } from "../aaronnote/plugin-runtime.ts";

describe("plugin runtime", () => {
  test("keeps disabled autoload plugins stopped", () => {
    expect(pluginShouldRun({ id: "copilot", autoload: true }, { copilot: "off" })).toBe(false);
  });

  test("autoloads only when no explicit override exists", () => {
    expect(pluginShouldRun({ id: "copilot", autoload: true })).toBe(true);
    expect(pluginShouldRun({ id: "copilot", autoload: true }, { copilot: "on" })).toBe(true);
    expect(pluginShouldRun({ id: "sample", autoload: false })).toBe(false);
  });

  test("normalizes persisted plugin overrides", () => {
    expect(normalizePluginOverrideMap({ copilot: "off", sample: "on", bad: true })).toEqual({
      copilot: "off",
      sample: "on",
    });
    expect(normalizePluginOverrideMap(null)).toEqual({});
  });
});
