# Growth Log

2026-05-02: AI maintenance layer runtime model changed to “index refresh always allowed; tooling changes require the development gate.” Future agents read `agent/develop.md` first; scripts may only be changed on explicit human request, a severe defect, or sufficient autonomous votes. Day-to-day work defaults to regenerating derived indexes, condensed wikis, and documentation. This makes long-term self-maintenance more stable and easier to audit.

2026-05-18: Note source of truth moved back to `roam/**/*.md`; distribution layer updated to read Markdown meta and relative links. Old Typst index no longer serves as a navigation entry point.
