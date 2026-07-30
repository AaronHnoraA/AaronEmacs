# Project Overview

Aaron's Markdown note system and long-term knowledge base. `#+begin meta` blocks in `roam/**/*.md` are the source of truth; cross-note relationships come from relative Markdown links. `agent/` maintains only derived indexes and condensed wikis to help AI retrieve quickly and verify against original Markdown files.

Noema ranger filesystem management is app-only. Local create, rename, move, duplicate, trash, refresh, focus, and template-backed note creation belong under `Noema/aaronnote/` plus `Noema/server/`; do not implement those behaviors in the published web output under `public/` or the publish pipeline unless a human explicitly asks for web support.
