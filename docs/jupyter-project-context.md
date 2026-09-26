# Kernel project context

Emacs and the Noema UI stay local. The existing Remote target registry owns
connection identity; the existing kernelspec owns execution/project settings.
No new SSH registry, editor daemon, file transfer protocol or local mirror is
introduced.

## Architecture audit

- `lisp/remote/remote-core.el` already defines targets and contexts;
  `remote-config.el` imports SSH aliases. Noema uses those definitions rather
  than parsing SSH configuration again.
- `remote-make-file-name` and `remote-target-file-name` produce the existing
  `/fs:TARGET:/path` identity. Its file-name handler routes normal Emacs file
  operations to the configured backend, normally TRAMP or TRAMP RPC for SSH.
  `remote-project-file-name` exposes the underlying TRAMP spelling when needed.
  These existing routing choices are preserved; new Noema commands do not
  introduce their own filesystem implementation.
- Local Emacs sources inspected: `files.el` (`file-remote-p`, `file-local-name`,
  executable lookup and handler dispatch), `subr.el` (remote process command
  helpers), `shell.el` (remote default directory / comint startup), and TRAMP's
  file/process handlers. Ordinary `find-file`, Dired, `insert-file-contents`,
  `write-region`, `copy-file`, `rename-file`, `delete-file`, `process-file` and
  `start-file-process` use the owning file-name handler. `make-process` needs
  its handler-aware route; the existing Remote/LSP code already supplies it.
  There is no built-in general-purpose `remote.el` here: this configuration's
  `lisp/remote/` library sits above Emacs/TRAMP.
- Existing Jupyter LSP code duplicated remote-profile interpretation. That now
  delegates to `init-aaronnote-jupyter-project.el`. Launch argv takes precedence
  over old descriptive `remote_kernel.config`; optional project metadata adds
  project settings without copying SSH options into each consumer.
- The LSP runtime resolver formerly reported target mismatch and still started
  the ordinary local language server. A required runtime now blocks startup on
  mismatch, failed interpreter probes or failed direnv preparation.
- The existing launcher still owns Jupyter's kernel start and five-port tunnel;
  those protocol duties were not replaced by a second filesystem layer.

## Configuration

Old profiles work unchanged: SSH host, workdir and interpreter are inferred from
their launch arguments. A profile can optionally add this object inside its
existing `metadata.aaron` object:

```json
{
  "project": {
    "target": "aaron-pc-remote",
    "root": "/home/aaron/Desktop/UNSW/COMP9444",
    "python": ".conda/bin/python",
    "direnv": true,
    "lsp": {
      "server": ["pyright-langserver", "--stdio"]
    }
  }
}
```

`target` is optional when the existing SSH alias resolves through Remote. It is
an existing target ID, not another host/user/options definition. It must agree
with the kernel's SSH host. `root` and `python` override launcher defaults;
relative interpreter paths containing `/` resolve inside the configured root.
Bare executable names resolve through the target environment. LSP `server` is
an argv array and runs on the project target using the existing LSP client.

Newly generated profiles reference their own JSON using `--project-file`.
Saving project metadata in the Board's editable Details page also adds that
reference to older `python -m remote_ikernel` profiles. The launcher reads this
file at the next launch. Guided edits of the same named profile preserve
project metadata. Renaming creates a new profile; copy its project metadata
before removing the old profile. Moving an
installed kernelspec directory requires updating its `--project-file` path.
The launcher on the machine that stores the profile must be this updated fork.

Notebook, Board and LSP kernelspec discovery share the target-owned command
resolver in `lisp/init-jupyter-command.el`. It checks project `.venv`/`.conda`
environments, target PATH and Conda's `~/.conda/environments.txt` registry via
ordinary remote file APIs. A Python module invocation is supported when the
modules are installed without a Jupyter console command. This selects the
catalog tool only: the chosen kernelspec's `argv` still determines the kernel
interpreter. An explicit configured command is never silently replaced.

When enabled, direnv prepares the project environment before LSP and shell
startup; the kernel launcher uses `direnv exec ROOT COMMAND`. Scripts retain
direnv's own authorization requirements. Product code never runs `direnv allow`.
Explicit interpreter selection takes precedence over direnv's PATH. Environment
variables exported by `.envrc` apply to the kernel, LSP and project shell.
For complex shell launch expressions, retain the existing launcher command;
`project.python` rewriting deliberately accepts only a simple argv command.
With direnv enabled, the entire kernel command runs under `/bin/sh -c` inside
`direnv exec`; variable expansion and commands after a semicolon see the loaded
environment. For Bash-specific syntax, use an explicit `bash script.sh` command.

For example, create `.envrc` in the **remote project directory**:

```sh
strict_env
PATH_add .conda/bin
export DATA_ROOT="$PWD/data"
export PYTHONPATH="$PWD/src${PYTHONPATH:+:$PYTHONPATH}"
```

Environment setup scripts may also run here; prefer repeatable setup because
direnv can evaluate the file separately for different processes. Add `watch_file`
for setup files whose changes should trigger direnv reloads. Use `strict_env` if
a failed setup command must abort environment loading. Review the file and run
`M-x direnv-allow` from its remote project buffer (or `direnv allow` in its remote
shell). An unapproved or failed environment blocks tool startup. Changes affect
new processes: restart the kernel/shell and refresh project LSP after editing.
JSON config is not evaluated as code and is not rewritten from runtime variables.
The launcher quotes project directories literally and aborts if changing into
the configured directory fails; it cannot silently start in the login directory.

## Commands

The notebook header has **Files**, **Shell**, **Project**, and **LSP** controls.
The ordinary notebook command menu also includes these actions.

| Shortcut | Action |
| --- | --- |
| `C-c i p d` | Dired at the configured project root |
| `C-c i p f` | Open a real project file, with normal Emacs completion |
| `C-c i p s` | Standard Emacs shell on the project target at that root |
| `C-c i p l` | Open project source if needed, then refresh/start its LSP |
| `C-c i p ?` | Inspect configured target/root/Python and a separate Python probe |

Opening Files or Shell associates that root with the original kernel profile
for this Emacs session. Python files subsequently opened from it use that
profile's interpreter and LSP settings. Association does not upload or relocate
the local notebook. Reopen Files after an Emacs restart to establish the
association again. Existing non-Jupyter remote projects keep their usual LSP
toolchain selection.

A local notebook connected to a remote kernel keeps kernel completion and
execution, but cannot feed its local URI to the remote LSP. Its LSP action asks
for an actual remote source file. There is no implicit synchronization. Jupyter
Contents-only servers still have no general-purpose SSH/LSP process route.

The configured root stays stable when a kernel changes cwd. Environment
inspection labels its new Python process separately from the running kernel;
it does not claim to read `sys.executable` from an already-running kernel.
JSON changes take effect for a newly started kernel, and LSP can be refreshed
through its project action. Reload the changed Emacs modules to expose the new
commands in an already-running Emacs.

## Acceptance evidence

`make jupyter-project-live-smoke` uses Aaron-PC by default, with isolated `/tmp`
files and only test-owned shell/LSP/kernel processes. Override target, Python,
or LSP through `JUPYTER_PROJECT_TARGET`, `JUPYTER_PROJECT_PYTHON`, and
`JUPYTER_PROJECT_LSP`. It checks:

- Dired, real remote file identity, normal read/edit/save;
- shell cwd and shared `.envrc` environment;
- remote Pyright initialization, project Python and real numpy hover;
- a real remote_ikernel kernel using project Python/cwd, direnv and numpy;
- cleanup of test files/processes and revocation of the disposable `.envrc`.

Only the test's freshly written `.envrc` is explicitly allowed by the test.
ERT additionally checks local-notebook mismatch without local LSP fallback,
directory failure, direnv failure, project-root association boundaries, and
normal Dired/file API dispatch. The launcher suite checks old profiles,
metadata preservation, path quoting, and rejection of unsupported expressions.
The real-direnv launcher regression also checks multi-command environment
propagation and blocked/failed `.envrc` files using an isolated allow store.
