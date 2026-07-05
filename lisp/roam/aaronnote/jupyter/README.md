# Aaronnote Jupyter

Self-contained Jupyter runtime for Aaronnote.

This directory owns the Jupyter-facing code and dependencies used by Aaronnote.
It intentionally avoids the top-level Emacs `jupyter/` directory and avoids user
site packages.

Aaronnote also owns a local JupyterLab prebuilt extension under
`labextension/`. The first extension feature is keyboard normalization for
JupyterLab inside Emacs xwidget: raw Delete, Backspace, and Escape control
events are converted into normal editor actions before they can insert glyphs
into Markdown files.

The extension replaces Jupyter's Markdown cell factory with
`AaronnoteMarkdownCell`, a first-class subclass of JupyterLab's `MarkdownCell`.
Aaronnote's live-preview features are CM6 extensions installed directly in the
single EditorView owned by Jupyter. There is no overlay editor and no copied
document state. Jupyter continues to own the Yjs model, undo history, notebook
commands, windowing, and save lifecycle.

Ordinary `.md` documents opened with the Jupyter Editor factory receive the
same Aaronnote CM6 extensions through Jupyter's editor extension registry.
Markdown cells remain live and editable in command and edit modes instead of
switching to a separate rendered Markdown DOM.

The Aaronnote icon in JupyterLab's right activity bar opens a diagnostics panel
showing server, keyboard bridge, native cell/view counts, and
extension runtime logs.

## Commands

From `lisp/roam/aaronnote`:

```sh
npm run jupyter:bootstrap
npm run jupyter:build-extension
npm run jupyter:doctor
npm run jupyter:lab
```

Generated state lives under `jupyter/.venv` and `jupyter/.jupyter`; both are
ignored by git. The tracked `kernel-templates/` directory is copied into the
internal Jupyter data directory during bootstrap. The tracked `labextension/`
source is built into the internal JupyterLab extension directory during
bootstrap.

## Emacs

Markdown file opens are routed through `init-aaronnote-jupyter.el`, which starts
`scripts/run-jupyter-lab.sh` and opens the target file in the managed Lab
session. The managed server uses `/` as its stable root, so files from roam,
project directories, the home directory, and mounted volumes can stay open in
one Lab workspace without restarting the server or invalidating existing tabs.
Hidden path components such as `.config` are enabled explicitly. Jupyter LSP
virtual documents are written under `jupyter/.jupyter/tmp/virtual_documents`,
and external language-server autodetection is disabled, keeping all generated
Jupyter state and runtime dependencies inside Aaronnote.
