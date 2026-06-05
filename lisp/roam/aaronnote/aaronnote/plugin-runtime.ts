export type PluginOverrideState = "on" | "off";
export type PluginOverrideMap = Record<string, PluginOverrideState>;
export type PluginRuntimeSummary = {
  id: string;
  autoload?: boolean;
};

export function normalizePluginOverrideMap(overrides: Record<string, unknown> | null | undefined): PluginOverrideMap {
  const out: PluginOverrideMap = {};
  if (!overrides || typeof overrides !== "object" || Array.isArray(overrides)) return out;
  for (const [id, state] of Object.entries(overrides)) {
    if (state === "on" || state === "off") out[id] = state;
  }
  return out;
}

export function pluginShouldRun(plugin: PluginRuntimeSummary, overrides: PluginOverrideMap = {}): boolean {
  const override = overrides[plugin.id];
  if (override === "on") return true;
  if (override === "off") return false;
  return plugin.autoload === true;
}
