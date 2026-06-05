# Aaronnote Plugin Workspace

This directory contains local Aaronnote plugins.

The current stable parser surface is command syntax:

- inline command: `@@cmd(switch) [context]{arg: value}`
- block command: `#+begin kind title` ... `#+end kind`

Core code currently exposes these parser primitives in `Aaronnote/src/command-syntax.ts`.
Server-side indexing mirrors the inline command scanner for agenda and future APIs.

## Proposed Plugin Shape

```text
plugin/
  my-plugin/
    plugin.json
    index.ts
    README.md
```

`plugin.json` should stay small:

```json
{
  "id": "my-plugin",
  "name": "My Plugin",
  "version": "0.1.0",
  "entry": "index.ts",
  "commands": ["my-command"],
  "blocks": ["my-block"]
}
```

The runtime loader is wired through:

- native plugin scan: `server/lib/plugins.mjs` through the Electron bridge
- app runtime: `Aaronnote/aaronnote/main.ts`
- Vite entry glob: `../plugin/*/*.ts`

`autoload: true` plugins start with Aaronnote. Plugins without autoload can still expose actions in the plugin manager.

Current local plugins:

- `copilot`: GitHub Copilot inline completion
- `roamlookup`: Notes-page Codex lookup over the local Roam knowledge base
