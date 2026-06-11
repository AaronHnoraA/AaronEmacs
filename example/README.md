# aaron-neopyter example

This is a minimal integration checklist to manually verify the full pipeline.

## Prerequisites

```bash
pip install neopyter
jupyter lab  # Start JupyterLab (usually at http://localhost:8888)
```

## JupyterLab side-panel setup

1. Open JupyterLab in the browser.
2. Click the Neopyter icon in the left sidebar.
3. Set **Mode** → `direct`
4. Set **IP** → `127.0.0.1`
5. Set **Port** → `9001`
6. Click **Connect** (or leave auto-connect enabled).

The extension will wait for Emacs to start its WebSocket server.

## Emacs side

```
M-x aaron-neopyter-connect
```

You should see `Neopyter: server started at 127.0.0.1:9001` in the echo area.
When the extension connects: `[neopyter-rpc] extension connected`.

## Manual integration checklist

- [ ] **Connect**: `M-x aaron-neopyter-connect` → `[N:⚡]` in modeline
- [ ] **Status**: `M-x aaron-neopyter-status` shows `connected`
- [ ] **Health**: `M-x aaron-neopyter-health-check` returns extension version
- [ ] **Open this file** in Emacs: `C-x C-f example/main.ju.py`
  - `aaron-neopyter-mode` auto-enables (check modeline)
  - `main.ipynb` opens in JupyterLab automatically
- [ ] **Sync on edit**: add a line to any `# %%` cell → JupyterLab updates without manual reload
- [ ] **Cell follow**: move point to different cells → active cell in JupyterLab changes
- [ ] **Scroll follow**: moving to a cell that's off-screen scrolls JupyterLab to it
- [ ] **Run cell**: `C-c C-c` → cell executes in JupyterLab, output visible there
- [ ] **Run all above**: `C-c C-a` → all cells above the cursor run
- [ ] **Run all below**: `C-c C-b` → current cell and below run
- [ ] **Restart kernel**: `C-c C-r` (confirms before acting)
- [ ] **Manual sync**: `C-c C-s` → force immediate sync
- [ ] **Save notebook**: `M-x aaron-neopyter-save-notebook` → notebook saved in JupyterLab
- [ ] **Disconnect and reconnect**: `M-x aaron-neopyter-disconnect` → reconnect the extension

## Debugging

```elisp
(setq aaron-neopyter-debug t)
M-x aaron-neopyter-show-log  ; shows *aaron-neopyter-log* buffer
```

Browser side: open browser DevTools → Console → look for `[neopyter]` messages.

## Known limitations (v1)

- **No browser→Emacs text editing**: edits made directly in JupyterLab cells are NOT
  written back to Emacs. Emacs is the authoritative source of text.
- **No pixel-perfect scroll sync**: only semantic cell-level scrolling.
- **No outputs in Emacs**: execution outputs stay in JupyterLab only.
- **Phase 5 (browser→Emacs cell follow)**: upstream Neopyter extension does not
  emit `activeCellChanged` notifications yet. The Emacs server is ready to receive
  them (the notification handler hook exists), but the extension must be patched first.
  Tracked for a future upstream PR to SUSTech-data/neopyter.
- **Proxy mode**: not yet implemented; only direct mode works.
