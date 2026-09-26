# Remote performance and compatibility audit

Initial measurements were made on 2026-09-25 with Emacs on macOS and
`aaron-pc` over SSH; later sections include 2026-09-26 local and CSE checks.
These numbers are a reproducible baseline, not a claim of VS Code parity.

## What changed

- Repeated `/fs:local:` metadata calls use a bounded context cache and a
  validated native route.  Already canonical absolute names skip redundant
  lexical expansion.  The common local spelling avoids repeated regexp
  parsing, including ordinary hidden-file names.  Route and operation
  contracts are checked on cache hits.  Workspace-free remote contexts are
  cached too; adding a workspace or replacing a target invalidates that
  context immediately, while configured workspaces are rebuilt to observe
  in-place edits.
- The native target's probed PATH, HOME, and SHELL now come from Emacs' client
  process environment.  A login-shell PATH probe had discarded a virtual
  environment already active when Emacs started, causing `/fs:local:` to pick
  another Python even though the native source buffer found the intended one.
  SSH targets still use their own login-shell probe; explicit target and
  workspace environment layers still apply after these base facts.
- Explicit absolute `/fs:` names expand through the existing logical-path
  implementation without entering TRAMP's generic vector parser.  Relative
  names and native absolute names under a logical default directory still use
  TRAMP's context-sensitive expansion.  Other lexical and process operations
  retain their existing dispatch.
- A stable two-backend remote route is reused only while its preference,
  registration, availability, capability, and cooldown guards still hold.
  The preferred-route validator avoids per-query list construction and a
  generic multi-list traversal; it still checks both backends and notices a
  registered backend's availability changing at runtime.
  Active no-hop TRAMP endpoint prefixes are cached with guards for the
  runtime, endpoint, and mutable pipeline config; multi-hop routes use the
  ordinary projection path.  The high-level operation provider registry is
  indexed by operation, avoiding provider scans for ordinary file queries.
- Workspace selection now scans configured roots once and keeps the deepest
  match, avoiding a fresh filtered and sorted list on every file query.  A
  direct edit to a target's workspace paths is still visible on the next call.
- Repeated read-only metadata and directory queries can reuse a session's
  complete liveness check for 100 ms.  Every call still checks the session and
  pipeline state, and the underlying backend operation runs normally.  Writes
  and other operations retain full checks; an operation that reports a backend
  or transport failure invalidates the session through the existing route
  failure path.  Set `remote-connection-read-liveness-interval` to zero to
  disable this short lease.
- Successful retry-safe metadata and directory reads no longer append a route
  and pooled-session reuse event on every call.  This keeps the bounded
  diagnostic log focused on connection changes and failures during large
  directory scans; `remote-log-read-query-successes` restores per-call traces
  when needed.  Route health checks and failure reporting still run on every
  query.  In separate warm 1000-call Aaron-PC samples, logical `/fs:` times
  moved from 146.4 to 139.3 µs for `file-exists-p`, 115.2 to 108.0 µs for
  `file-attributes`, and 164.9 to 158.6 µs for `directory-files`.  Direct
  `/rpc:` timings remained about 41–82 µs in those runs; the small deltas are
  directional rather than a controlled VS Code comparison.  An eight-round
  local C visit rerun measured 78.94 ms native versus 81.07 ms `/fs:local:`
  median; the Aaron-PC logical C clangd smoke passed diagnostics, completion,
  snippets, and watch checks after the logging change.
- Managed OpenSSH connections now use `ServerAliveInterval=15` and
  `ServerAliveCountMax=3` for TRAMP, tramp-rpc, direct processes, and dedicated
  forwards.  This lets OpenSSH close an idle, unresponsive transport so the
  existing process-exit and workspace-recovery paths can run without an Emacs
  timer issuing extra RPCs.  A pipeline's explicit `:ssh-options` overrides
  either value; set the corresponding
  `remote-backend-tramp-ssh-server-alive-interval` or
  `remote-backend-tramp-ssh-server-alive-count-max` customization to nil to
  inherit SSH config.  According to the
  [OpenSSH client manual](https://man.openbsd.org/ssh_config), the defaults
  allow roughly 45 seconds without a reply before SSH disconnects.  A real
  Aaron-PC tramp-rpc transport was inspected after connecting and carried
  both options.  The full Remote check, real SSH E2E suite, and a logical C
  clangd smoke passed with these defaults.  An isolated loopback TCP relay
  now exercises the server-alive and recovery path with shorter test-only
  values; see the fault-injection result below.
- The first automatic workspace reconnect now caps backend connection opening
  at three seconds when the route uses the default connection timeout.  Later
  retries retain the ordinary eight-second framework deadline; an explicit
  pipeline `:connect-timeout` skips the shorter first attempt.  This avoids
  spending the full connect timeout on a first retry that starts while the
  network is still unavailable.  Set
  `remote-workspace-reconnect-first-open-timeout` to nil to disable this
  policy.
- The first `lsp-mode` client-module load runs with the client machine's
  directory, environment, and executable path.  Loading hundreds of local
  Lisp modules in a TRAMP buffer had triggered target filesystem probes.
  When an explicit `lsp-enabled-clients' whitelist names only already
  registered clients, startup defers the unrelated client-module load until
  another command actually needs it.
- The first core `lsp-mode` load and the UI modules used by its configure
  hooks now run in the client environment.  The server availability check
  loads the core; after it finds a client, it loads the UI modules before
  target-buffer hooks fire.  Company and the four configured completion
  backends also load there before the first `didOpen`; their autoloads had
  stalled that phase for about 1.2 seconds.  The `lsp-modeline` module is
  included in this client-side preload; its first autoload previously held
  the initial `lsp-configure-hook` for about 0.33 seconds.  Session-folder
  lookup compares
  canonical paths before checking existence, avoiding remote `stat` calls
  for unrelated roots from a persisted LSP session.
- The first Yasnippet and bundled snippet-directory load now runs in the
  client machine's directory and environment before its minor mode enables
  in a logical or TRAMP source buffer.  A cold buffer enables it after 0.15
  seconds of idle time, or immediately when a snippet command is used; warm
  buffers enable it during the mode hook.  The local snippet scan no longer
  expands paths through the target buffer's file-name handler thousands of
  times.
- Project search chooses the target's ripgrep when available.  A trusted
  Linux x86_64/glibc or aarch64 target without it receives a pinned,
  SHA-256-verified ripgrep 15.2.0 binary in its user cache.  The first search
  opens Consult's target-side grep immediately while a separate client-local
  batch Emacs process checks the target platform and existing cache, then
  prepares rg if needed.  Subsequent searches use the verified tool.  This
  keeps target-side validation off the first search's interactive path, even
  when a binary is already cached.  A successful lookup is refreshed in the
  background after 30 minutes while the existing path remains available;
  a failed refresh switches later searches back to grep.  Provisioning
  uses Remote's existing bulk transfer and atomic versioned
  publish path.  Both the downloaded archive and target binary are checked
  by SHA-256; a damaged ready cache is rebuilt from a validated staging
  directory.  Unsupported or untrusted targets keep Consult's grep fallback.
  Aaron-PC initially had GNU grep but no ripgrep.  Its managed binary and
  Consult's asynchronous ripgrep candidate stream were verified over SSH on
  an untracked file.  The project, Telescope, Evil, and `C-c s` entry points
  share this route; `C-c s` still opens the rg menu where rg is in target PATH.
  When rg is absent, `C-c s` passes its probe result to project search instead
  of repeating the target lookup (about 49 ms per missing-rg lookup in one
  Aaron-PC sample).
  Shared local filesystems pass a native path to Consult.
  Set `remote-search-auto-provision-ripgrep` to nil to use only tools already
  installed on the target.  The archive cache stays on the client; the
  extracted binary lives below the target user's `~/.cache/emacs-remote/`.
  Magit status may use a physical TRAMP path internally, but visiting a
  worktree file returns its existing `/fs:' source buffer even when the user
  opened the logical file directly before opening a workspace.  A local
  workspace returns its native source buffer.
- Repeated ensure hooks for one buffer now coalesce an already deferred LSP
  start when its runtime and environment capsule are unchanged.  Manual starts,
  a new environment capsule, detach, an immediate startup error, and expiry
  of the three-second window all allow another attempt.  This avoids applying
  the same remote toolchain and searching for the same server multiple times
  before the first workspace has finished opening.
- During one synchronous LSP start, project-local settings reuse the source
  buffer's resolved project root.  The binding checks buffer identity so a
  callback for another buffer cannot inherit that root while Emacs waits on
  target I/O.  The next start resolves the root again.
- A disconnected, reconnecting, or failed workspace remains the same owner
  when lsp-mode resolves its
  project again.  An LSP process-exit callback keeps its recoverable resource
  while that owner is disconnected or reconnecting.  Recovery uses lsp-mode's
  public shutdown/start commands, then reattaches every surviving source
  buffer from the old workspace to the new one.  The existing restart advice
  defers lsp-mode's own crash restart while Remote owns transport recovery,
  avoiding a competing transient server and a false crash-loop count.
- On the verified tramp-rpc 0.13.1 client, a generation-checked transport
  exit callback reports a failed route before tramp-rpc closes LSP relays and
  watches.  LSP's process-exit callback independently checks the existing
  pooled session, so an unrecognized future tramp-rpc release can still
  trigger recovery after its child process exits.  Neither check opens a new
  connection inside the process sentinel.  A dead LSP workspace is shut
  down through the existing bounded shutdown helper before restart.
- The verified tramp-rpc adapter bounds retry-safe `/fs:` `file.stat` and
  `dir.list` RPCs to 5 and 10 seconds, respectively.  These settings are
  configurable through `remote-backend-tramp-rpc-read-query-timeouts`.
  A timed-out request kills only its captured RPC transport generation,
  classifies the failure as transport-wide, and lets the workspace reconnect
  on a fresh session.  Writes, direct `/rpc:` operations, and unknown package
  versions retain their existing upstream deadline and behavior.
- Automatic reconnect reuses target PATH facts within one recovery attempt
  while still withholding them from the global cache.  Session invalidation
  clears the attempt-local facts; the background job checks its target epoch
  before storing a new probe.  A changed target retries with a fresh cache.
- Copilot's client-side server startup waits for editor idle time in remote
  buffers, using the configured deferred delay.  A local buffer whose Copilot
  library is still cold waits 0.75 seconds of idle time; later warm local
  buffers enable it immediately.  First package loading uses the client
  directory and environment in either case.  Explicit `copilot-mode' still
  starts immediately.
- Repeated `.envrc' root searches are coalesced for one second.  Explicit
  refreshes, `.envrc' saves, and connection closure invalidate that cache.
  `lsp-mode' no longer auto-starts the separate `dap-mode' UI on every source
  buffer; Dape remains this configuration's debugger.
- Citre's remote helper lookup now uses one target process with Citre's own
  remote search path.  It keeps the original search for unusual path shapes,
  executable suffixes, unavailable shells, or a changed Citre call signature.
  Cached results include the search path, so an environment update cannot
  reuse a result from the previous PATH.
- Remote LSP directory watches use one recursive target process per root.
  `inotifywait` is preferred.  On Linux hosts with Python 3 but no
  `inotifywait`, the standard-library `watch-agent.py` supplies the same
  NUL-framed event stream.  A failed Python startup falls back to the
  ordinary lsp-mode watcher path.  Recovery now waits for the Python agent's
  `READY` handshake before marking its stable watch descriptor open; a
  timeout removes the replacement process and marks the resource failed.
  A real Aaron-PC reconnect kept the public descriptor valid and delivered
  another nested-file event after recovery.
- The tramp-rpc timeout advice now preserves the optional captured connection
  generation.  Private function arities are checked before installing advice.
  The relay cwd, adapter timeout, architecture, and PATH batch workarounds are
  installed only for the exact verified 0.13.1 checkout; reinstalling the
  adapter removes them on an unverified client version.  Doctor reports the
  active state of the versioned shims.  As checked on 2026-09-26, the
  [upstream 0.13.1 release](https://github.com/ArthurHeymans/emacs-tramp-rpc/releases/tag/v0.13.1)
  is still marked Latest; the package lock and installed checkout both point
  to its `e1d4632` commit.  Unreleased master changes are not used by these
  verified private shims.
- The Remote board displays cached session/workspace state and has a quick
  status refresh that does not reload target configuration.  A visible board
  coalesces connection and workspace changes into one idle refresh; hidden
  boards do no refresh work.  Configured,
  active, and explicitly reopened folders have direct actions; recent folders
  are persisted with savehist.  A cheap mode-line item identifies the current
  logical target and opens the board.  Manual reconnect uses the coalesced
  background recovery job.  Its own session invalidation no longer makes a
  successful reconnect appear stale; external invalidation during the new
  connection causes a retry.  A closed workspace cannot be reopened by a
  late result.  The reconnect path has a real SSH E2E check on Aaron-PC.
- The board now classifies cached SSH connection failures and offers an
  asynchronous, bounded OpenSSH diagnostic with a detailed output buffer.
  Its separate login-terminal action uses the pipeline's config file, user,
  port, jump hosts, and SSH options.  Neither board drawing nor status
  refresh performs a network probe; actual Aaron-PC and custom-config-only
  alias diagnostics returned `SSH ready`, and an asynchronous fake-auth-failure
  test verified the error state and captured stderr.  A real localhost port-1
  refusal was classified as a network error without opening a workspace.
- New connection attempts publish transport, backend login, and protocol
  phases to the Remote board and a bounded per-target `L` log.  Warm session
  reuse publishes no progress events.  Session generations keep a late phase
  from an older attempt from replacing the current one.  `C-g` during a
  yielding transport now closes already opened stages and clears both session
  and pipeline placeholders; a fault-injection test checks the cleanup.
- Opening a folder from the board now establishes an owning workspace before
  visiting Dired, so the board and later reconnect/resource actions see the
  same active folder.  A failed visit closes a newly created workspace.
  The visible board shows an in-progress folder and target state while the
  synchronous directory/connection operation runs, then clears it even on an
  error or cancellation; drawing this state performs no target I/O.
  Board actions can close one workspace or disconnect all of a target's
  workspaces and pooled sessions while leaving visited buffers available.
  A real Aaron-PC folder-open/disconnect/reopen check passed.
- Dirvish now skips its optional child-Emacs metadata helper for every
  logical `/fs:` Dired buffer, including `/fs:local:`.  Dirvish can pass a
  native directory while its buffer remains logical, so checking the directory
  alone missed that case.  The missed local path produced a helper sentinel
  error during the board-to-LSP smoke; a focused no-child-process test and
  two complete local clangd checks pass after the buffer-aware guard.
- The board's folder prompt completes directories on the selected target.
  Forward rows can be named and moved to a requested or dynamic client port.
  A move verifies the replacement's local port before closing the old listener;
  an occupied client port leaves the old forward in service.  The name and
  selected local port survive workspace reconnection.  Real Aaron-PC SSH
  checks read the target SSH banner through the changed port and through the
  restored port after reconnect.
- The board can add a host to any imported SSH config and reload its target
  without a network connection.  The new file is private, duplicate aliases,
  excluded hosts, and directive injection are rejected before writing, and
  relative import paths are resolved beside `etc/remote.json` even from a
  logical Remote buffer.  A custom config is recorded on the pipeline and
  passed as `-F FILE` through TRAMP, tramp-rpc, direct processes, SCP,
  forwarding, and SSH control operations.  Client SSH processes retain the
  local environment so target `HOME` cannot change local `Include` expansion.
  TRAMP's connection cache receives the scoped login arguments even when its
  cache entry predates the route; the arguments remain ephemeral.
- The board also accepts a pasted OpenSSH connection command with identity,
  port, user, jump, config-file, and `-o` options.  It parses the command as
  data and writes an imported Host entry without executing the pasted text.
  New entries precede wildcard/Include rules, because OpenSSH usually keeps
  the first value; updates use an atomic replacement that preserves an
  existing config symlink and permission bits.  The parser handles quoted
  values and POSIX backslash escapes without running a shell, including
  multiple identity files.  A focused test checks the effective result with
  `ssh -G`, preserves prior global options for other hosts, and rejects
  remote commands and invalid options before touching the file.  A temporary
  command-created alias then authenticated to Aaron-PC, read `/fs:` `/tmp`,
  and ran a target process through the imported pipeline; its temporary
  config was removed afterward.
- Workspace-owned SSH forwards close before an intentional session reset, then
  recover on the same local port.  Dedicated SSH forward connections avoid
  ControlMaster teardown races.  Local listener readiness is read from SSH
  diagnostics, so normal startup does not repeatedly connect to the target
  service.  A real Aaron-PC reconnect test verifies the original local port
  still reaches the target SSH service.

## Measured behavior

| Probe | Before | After |
| --- | ---: | ---: |
| `/fs:local:` `file-exists-p`, 3000 calls | about 230 µs/call | about 20 µs/call |
| Native `file-exists-p`, same file | about 7 µs/call | about 7.5 µs/call |
| Aaron-PC clangd initialization wait, cold Emacs | about 67 s | about 0.45–0.51 s |
| Aaron-PC clangd initialization wait, second workspace in same Emacs | — | about 0.15–0.24 s |

In a warm 30-call Aaron-PC run using the interactive configuration, the
selected file route was tramp-rpc.  `file-attributes` on `/fs:aaron-pc:` fell
from about 0.37 to 0.23 ms/call; `file-exists-p` fell from 0.41 to 0.26
ms/call; `directory-files` fell from 0.43 to 0.27 ms/call.  The corresponding
direct `/rpc:` calls were about 0.04–0.09 ms/call.  These are hot-cache
microbenchmarks, so they do not imply the same percentage change for cold
SSH connections or LSP startup.

Using the same smoke harness on the local target, a native file path and an
explicit `/fs:local:` path both completed clangd startup and all parity
checks.  After the latest startup changes, first initialization waits were
about 0.131 and 0.129 seconds, and second-workspace waits were about 0.119
and 0.125 seconds respectively.  First startup-request phases were about
0.633 and 0.635 seconds.  This is evidence of practical local LSP
parity, not microbenchmark parity.

The complete second remote workspace smoke took about 3.5 seconds from
target-side temporary directory creation through diagnostics and watch checks.
A later two-workspace smoke that opened both C buffers through logical
`/fs:aaron-pc:` names passed the same checks.  Its clangd initialization waits
were about 2.87 and 1.64 seconds; the first and second visit phases were about
2.00 and 0.19 seconds.  This covers the Remote board's logical file path as
well as direct TRAMP visits.
A custom SSH config containing `Include ~/.ssh/config` passed the real
Aaron-PC file, async process, recursive watch, forward, reconnect, and PTY
checks.  A separate two-workspace `/fs:aaron-pc:` clangd smoke passed
diagnostics, completion, and watch checks through that config.  Its first and
second initialization waits were 0.56 and 0.31 seconds, respectively.  The
custom-config and ordinary-config runs were not paired under controlled
conditions, so these values are validation results rather than a speed ratio.
A separate `Aaron-Emacs-Only` alias present only in a temporary SSH config
passed all five SSH E2E checks.  That run also exercised automatic target
selection and service-port discovery through `-F`.  Before the TRAMP cache
fix, each reconnect emitted a failed-connection warning even though the suite
passed; the same five checks passed without those warnings after the fix.
A separate direct SSH clangd `initialize` exchange took about 0.28 seconds;
this shows that cold Emacs/LSP setup still dominates the first launch.  The
local C smoke took about 8 seconds for the complete Emacs batch process,
while the two-workspace remote batch took about 17 seconds.  These runs include batch
startup and checks, so they should not be read as interactive editor timings.

Normal local file buffers keep native paths and native file operations.
Explicit `/fs:local:` paths still pay file-name-handler overhead, roughly
2.7–3 times native for tiny metadata calls after direct dispatch of canonical
logical-name expansion and registered metadata operations.  The cached local
route still passes its configuration checks.  This remains a performance gap.

A subsequent warm Aaron-PC 30-call sample measured `/fs:` at 205–245 µs
per metadata/directory query versus 42–88 µs for the selected direct
`/rpc:` path.  The 30-call sample varies with cache and scheduler state; it
shows that the logical route still adds significant fixed cost.

A guarded two-backend route cache then reduced one Aaron-PC sample to about
167 µs for `file-attributes` and 201 µs for `file-exists-p`; the direct
`/rpc:` calls in that run were about 44 and 79 µs respectively.  The cache
only applies when one pipeline has a stable first-choice backend.  It checks
live preferences, plugin registration and availability, capabilities, and
cooldown before reuse, so failover still takes the normal path.

With the workspace-free context cache, active endpoint prefix cache, and
operation-provider index, a subsequent 30-call Aaron-PC sample measured
`file-attributes` at 137 µs on `/fs:` versus 43 µs on `/rpc:`,
`file-exists-p` at 171 versus 78 µs, and `directory-files` at 186 versus
85 µs.  The no-provider lookup alone fell from about 14 to 1 µs per call in
a separate 100,000-call local probe.  Explicit `/fs:local:` metadata still
measured about 32 µs versus 8 µs on the native path.

A later 1000-call Aaron-PC sample measured the route validator alone at about
21.7 µs per call before the simpler checks and 16.4 µs afterward.  In the
same warm SSH benchmark, logical `file-attributes` changed from about 121.5
to 118.2 µs per call; direct `/rpc:` was about 42.0 and 41.1 µs in the two
runs.  `file-exists-p` changed from about 152.8 to 148.4 µs, and
`directory-files` from 172.1 to 166.7 µs.  Separate runs can vary with cache
and scheduler state, so these small end-to-end differences are directional.
The local adapter fast path measured about 19.2 µs for explicit
`/fs:local:` `file-exists-p`, versus about 7.4 µs for the native path.
The preferred-route validator now compares snapshots of the target, adapter,
and context preference inputs, including in-place edits, instead of building
a merged preference list on every file query.  In a later 3000-call warm SSH
sample, `/fs:` `file-attributes` moved from 115.8 to 113.7 µs per call;
`file-exists-p` moved from 148.0 to 145.8 µs, and `directory-files` from
182.5 to 181.7 µs.  Direct `/rpc:` timings also moved slightly, so these
small differences are only directional.  A separate instrumented run measured
the validator at 16.3 µs per call.  Contract tests mutate each preference
owner in place and verify that the selected route changes.
After this change, two real logical `/fs:aaron-pc:` C workspaces completed the
clangd smoke checks, including diagnostics, completion, and target file-watch
events.  Their initialization waits were 0.575 and 0.304 seconds.  These are
functional and latency samples, not a controlled VS Code comparison.
The board-to-LSP flow also passed on Aaron-PC after opening its folder first:
folder-open phases were about 0.377 and 0.073 seconds, followed by clangd
initialization waits of about 0.562 and 0.293 seconds.  The same flow on the
local logical target passed after the Dirvish guard, with initialization waits
of about 0.133 and 0.128 seconds.  These are separate smoke runs; they do not
establish a speed ratio against VS Code or native local file visits.

The cold-run profiler found that 39 identical `.envrc' ancestor searches and
eager loading of unrelated LSP clients caused avoidable target probes.  After
the changes, ancestor-search RPCs dropped from 43 to about 10, file-stat RPCs
from about 163 to about 79, and target executable lookups from 28 to 19 in this
C workspace.  The next bottleneck was `lsp-managed-mode': its default DAP
auto-configuration alone took about 10–11 seconds in a remote `didOpen'.
The same profiler then found about 36 wasted `file.stat' calls during LSP
session-folder search, and several seconds of local UI package autoloading
under a target buffer.  The latest cold SSH smoke measured a 1.4-second
startup-request phase and 3.1-second initialization wait; this run includes
the ordinary LSP client UI setup.
Instrumented timings change absolute latency, so use the uninstrumented smoke
test for end-to-end figures.

The later logical-path Aaron-PC LSP profile counted 79 `file.stat` RPCs before
the Citre helper lookup change and 61 after it.  Citre's missing `readtags`
lookup used about 70 ms when the target search path was already warm, while
the one-process lookup used about 12 ms on the same host.  A present `clangd`
helper returned the same `/usr/bin/clangd` path with the new lookup.  A live
custom-PATH probe found an executable placed in a target `/tmp` directory,
then removed that test directory.  End-to-end cold visit time moved from about
2.00 to 1.86 seconds in one pair of runs; that small difference is within
run-to-run variation and is not an established speedup.

With deferred-start coalescing, the same cold Aaron-PC C profile showed one
runtime preparation and one process-environment application rather than three.
Two uninstrumented two-workspace runs measured clangd initialization waits of
about 1.63–1.66 seconds for the first logical `/fs:` workspace and
0.150–0.153 seconds for the second.  The second-workspace start-request phase
still took about 0.58–0.63 seconds, so the whole start is longer than the
initialization wait alone.  These are small C workspaces on one host, not a
general language-server or VS Code comparison.

A later two-workspace profile found four separate project-root searches in
each start request, each costing about 0.10–0.14 seconds on Aaron-PC.  With
the per-call root binding, three uninstrumented two-workspace runs measured
the second logical workspace's start-request phase at 0.048–0.058 seconds,
down from about 0.58–0.65 seconds in the prior runs.  First-workspace
start-request phases measured about 0.72–0.77 seconds, down from about
1.27–1.30 seconds; the remaining cold cost includes local package loading
and target setup.

After clearing profiler totals at the beginning of `find-file-noselect`, the
first remote C visit spent about 0.09 seconds in target RPC calls but about
0.74 seconds in Copilot's eager mode hook.  Deferring that hook moved three
uninstrumented Aaron-PC logical `/fs:` visit phases to 1.07–1.08 seconds,
from about 1.86–2.02 seconds before the change.  The complete LSP smoke still
passed; Copilot's later idle startup is outside the visit timing.

The subsequent cold initialization profile showed that `company-mode'
autoloaded `company-capf', `company-files', `company-tempo', and
`company-yasnippet' inside the remote buffer, taking about 1.2 seconds.
Loading those Lisp features with the client machine's file and process
context before LSP starts cut three fresh Aaron-PC C initialization waits
to 0.45–0.51 seconds, from about 1.55–1.69 seconds before the change.
The second workspace in the two-workspace run waited 0.24 seconds.  These
figures include Emacs' `didOpen' and completion setup, not only clangd's
wire-level `initialize' reply.

After adding direct expansion for explicit absolute `/fs:` names, a warm
1000-call Aaron-PC sample measured `/fs:` versus `/rpc:` at 153.5 versus
71.2 µs for `file-exists-p`, 122.9 versus 41.2 µs for `file-attributes`,
and 173.6 versus 81.2 µs for `directory-files`.  A separate 3000-call
`/tmp/` sample measured `/fs:local:` versus native at 15.5 versus 5.2 µs
for `file-exists-p` and 15.9 versus 5.6 µs for `file-attributes`.  An earlier
same-file local sample measured about 20 versus 7.5 µs.  These paths have
different native metadata costs; compare each logical call with its direct
call in the same run.  A temporary increase of the read-liveness lease from
100 ms to one second did not improve the 1000-call SSH sample, so the shorter
default remains in place.  The logical two-workspace clangd smoke and the
real SSH E2E suite passed after the dispatch change.

Two fresh local C smoke processes after that change measured native versus
explicit `/fs:local:` clangd initialization at 0.133 versus 0.130 seconds
for the first workspace and 0.123 versus 0.129 seconds for the second.  Their
first file-visit phases were 1.54 versus 1.72 seconds, and their second visits
were 0.089 versus 0.110 seconds.  The initialization waits are close; these
single visits are not enough to establish a stable end-to-end speed ratio.

The board-to-clangd Aaron-PC profile next found 13 executable lookups and 67
`file.stat` RPCs in one cold logical workspace.  LSP now shares executable
results only during its preflight and connection, with the target workspace,
environment capsule, and directory in each cache key.  The cache is cleared
after connection or a failed start.  For tramp-rpc routes, an executable
probe asks the target POSIX shell in one process request and validates the
absolute result; unsupported output falls back to Emacs' existing lookup.
The same instrumented workflow then made 4 target executable lookups, 35
`file.stat` RPCs, and spent about 0.06 rather than 0.40 seconds in
`remote-executable-find`.  These are profiler timings, not a controlled
end-to-end speed comparison.  The next profile traced 16 initial `file.stat`
RPCs to tramp-rpc validating each configured PATH directory on first process
use.  A version-guarded adapter for the exact 0.13.1 release now assembles
the same ordered PATH and checks all directories through one target process
request.  Unexpected output, a missing POSIX shell, a changed private
signature, or a different package release uses tramp-rpc's original function.
The same instrumented board-to-clangd workflow then used 19 `file.stat` RPCs
instead of 35 and 80 total tramp-rpc calls instead of 95; the batch added
one `process.run`.  A real target check preserved existing directory order
and removed a missing directory.  Doctor reports whether the batch is active,
and `make remote-e2e` runs its RPC-specific checks under the real init in
addition to the isolated SSH suite.
The installed tramp-rpc 0.13.1 matches the
[latest published release](https://github.com/ArthurHeymans/emacs-tramp-rpc/releases/tag/v0.13.1)
as of 2026-09-25, so no client/server version change was needed for this pass.
The target-side probe is limited to tramp-rpc process routes.  A missing
POSIX shell, unexpected output, or a relative command result takes the
ordinary file-handler path instead.

After this change, the same two-workspace C smoke on the local target used
the native backend for both physical and `/fs:local:` visits.  First visits
were 1.56 seconds physical and 1.73 seconds logical; second visits were
0.097 and 0.103 seconds.  LSP start-request phases were 0.452 and 0.452
seconds on the first workspace, then 0.016 and 0.021 seconds on the second.
These are separate fresh processes with substantial package cold-start work,
so they show functional local parity and the size of this sample's overhead,
not a guaranteed native-speed match on every workload.

A balanced same-process local visit benchmark then measured warm C-file
medians of 78.4 ms native and 81.0 ms `/fs:local:` over 24 visits each.
Reversing the first-visit order exposed a separate cold-load problem:
Yasnippet scanned its client-local snippet directories while the buffer's
default directory was logical, causing about 55,168 `expand-file-name`
handler calls.  Loading that library with the client context reduced the
same first logical visit to about 714 path expansions and from about 1.75 to
1.53 seconds in the benchmark.  In separate complete board-to-clangd C
smokes after the change, local native and logical first visit phases were
1.557 and 1.559 seconds, with Yasnippet enabled in both.  A real Aaron-PC
logical first visit measured about 0.82 seconds, versus about 1.06 seconds
in the earlier run.  These are small samples, so the strongest conclusion is
the eliminated path-expansion work; the timings are workload indicators.
The final 24-visit warm sample measured 78.3 ms native and 81.1 ms logical.
The direct `/rpc:` Aaron-PC C smoke also passed after this change, including
Yasnippet, diagnostics, completion, and a file-watch event.

The Aaron-PC search/SCM check found no `rg` binary in target PATH.  Before
provisioning, project search selected Consult grep, whose actual async
candidate stream returned the new untracked file.  After provisioning, the
verified managed ripgrep 15.2.0 binary ran Consult's async search against the
same kind of untracked file.  Magit status opened the Git repo and
its worktree-file action reused the already-open `/fs:' source buffer both
before and after a workspace was opened; before the identity fix it opened a
duplicate `/rpc:' buffer.  The matching local workspace check reused its
native buffer.  Magit staged and unstaged the target file, with Git status
confirming both transitions.  `make remote-e2e` runs this SSH integration
check under the full configuration.
With an already warm SSH connection in isolated batch Emacs, the first
missing-rg route took 0.597 seconds while platform and cache validation ran
on the caller.  Moving those checks to the worker reduced the measured
interactive call to 0.163 seconds, including worker launch; the worker then
returned the verified target binary.  These figures exclude Consult display
and represent one host and one run, not a general latency guarantee.
In a separate target-only synthetic benchmark with 3,000 plain files of about
4.5 KB each, 15 alternating warm runs had median search-process times of
8.23 ms for the managed rg and 10.35 ms for GNU grep.  This excludes SSH,
Consult, candidate display, and file opening; it is a tool comparison rather
than a VS Code or end-to-end editor benchmark.
The SSH service-validation E2E also changed a deployed executable's contents
while leaving its executable bit set.  The next provision repaired that
versioned directory and restored the expected contents.

A 2026-09-25 warm 1000-call Aaron-PC sample measured `/fs:` versus direct
`/rpc:` at 144.5 versus 73.2 µs for `file-exists-p`, 114.0 versus 41.1 µs
for `file-attributes`, and 163.2 versus 80.5 µs for `directory-files`.  A
proposed extra metadata fast path saved only about 2 µs in an alternating
same-process comparison and was removed because it added a second failover
path for negligible gain.  In an eight-visit-per-path same-process local C
sample, native visits had a 79.5 ms median and `/fs:local:` 82.2 ms.

A later eight-round local C rerun measured 79.0 ms native versus 81.6 ms
`/fs:local:` median over 16 visits per spelling.  A same-process 1000-call
Aaron-PC rerun measured direct `/rpc:` versus `/fs:` at 75.7 versus 144.8
µs for `file-exists-p`, 40.9 versus 113.9 µs for `file-attributes`, and
80.6 versus 163.7 µs for `directory-files`.  Instrumenting the route path
in a separate 300-call run attributed about 40 µs per logical operation to
pooled-session acquisition, 21 µs to route selection, and 16 µs to argument
translation; profiler instrumentation also adds overhead.  This locates the
remaining fixed framework cost without treating the instrumented values as
user-visible latency.

A balanced SSH source-visit benchmark now opens distinct C files in ABBA
order through physical `/rpc:` and logical `/fs:` names in one Emacs process.
It checks that each visited buffer retains the requested spelling and
disables automatic LSP startup; cold package loading and server initialization
therefore remain separate measurements.  On Aaron-PC, two eight-round runs
measured physical medians of 135.0 and 129.1 ms versus logical medians of
136.1 and 136.0 ms, respectively.  Reversing warm-up order changed the first
visit from 810.7 ms physical to 945.0 ms logical, showing that first package
loads dominate that single cold sample.  The warm results suggest the
framework's microsecond-scale metadata overhead contributes only a few
milliseconds to an ordinary file visit on this host; they do not establish
VS Code parity or cover LSP startup.

A fresh 2026-09-26 CSE run with four ABBA rounds measured eight physical
`/rpc:` visits at 133.1 ms median and eight `/fs:` visits at 136.0 ms median.
The same local benchmark with eight rounds measured 16 native visits at
76.2 ms and 16 `/fs:local:` visits at 79.1 ms.  Both benchmarks now accept
`REMOTE_VISIT_MAX_RATIO` as an optional upgrade gate: a paired median above
that logical-to-baseline ratio fails the command.  Repeating both at a 1.20×
gate passed, with local 76.6/79.5 ms (1.037×) and CSE 133.9/137.7 ms
(1.028×).  This is a visit-latency guard for the current machine and host;
it does not replace a same-machine VS Code comparison or cover cold startup.

A later cold C profile attributed about 0.38 seconds of the initial source
visit to the first Yasnippet and snippet-library scan.  The mode hook now
schedules that cold load for an idle period, while explicit snippet commands
load immediately and pin the scan to the client environment.  The previous
two-second wall-clock package load was removed so continued typing cannot
trigger an unrelated scan.  In fresh Aaron-PC clangd smokes, the source
visit went from 0.91–0.93 seconds before this change to 0.62 seconds for
both physical `/rpc:` and logical `/fs:` files; initialization stayed near
0.30–0.34 seconds.  Batch Emacs does not run an interactive idle command
loop, so the smoke test explicitly invokes the pending idle callback before
checking Yasnippet.  Both paths then passed snippets, diagnostics,
completion, and watch checks.  A fresh local C smoke measured 1.158 seconds
for a native visit and 1.162 seconds for `/fs:local:`; a separate balanced
warm local visit sample measured 79.0 versus 81.7 ms median.  These small
samples support local path parity for these checks, while the total cold
experience still includes client startup and asynchronous snippet loading.

A follow-up local cold profile found 0.74 seconds in the first synchronous
Copilot hook, mostly server startup.  Deferring only the cold local automatic
start moved the native first C visit from 1.20 to 0.42 seconds in the same
benchmark; warm native and `/fs:local:` medians were 77.2 and 80.2 ms over
16 visits per spelling.  A real callback check verified that Copilot was
disabled during the initial visit and enabled with its server running when
the deferred function executed.  The time saved from file visit is paid
after idle; this improves first render without claiming lower total CPU work.
Fresh local clangd smokes after this change visited native and `/fs:local:`
C files in 0.391 and 0.384 seconds, then passed diagnostics, completion,
snippets, and watch events.  A fresh Aaron-PC logical C smoke visited in
0.618 seconds with the same checks.  Explicitly firing the deferred Copilot
callback in separate local and Aaron-PC buffers started the client-side
server and enabled `copilot-mode` in each.

Opening a managed SSH folder now schedules one client-side LSP and UI package
preload after 0.5 seconds of editor idle.  This uses ordinary public `require'
under the client environment and leaves a quick source visit on its existing
startup path.  It does not run for native or `/fs:local:' Dired folders.  In
fresh Aaron-PC C smokes, opening the folder and immediately visiting its file
left LSP start-request at 0.510 seconds; explicitly running the pending idle
callback first spent 0.379 seconds during the simulated folder pause, then
reduced start-request to 0.112 seconds.  clangd initialization stayed at
0.282 versus 0.295 seconds, and diagnostics, completion, snippets, and watch
checks passed in both runs.  This moves client package work into folder idle
time; it does not reduce total CPU work or the SSH protocol latency.  Reproduce
the batch callback path with:

```sh
REMOTE_LSP_E2E_TARGET=aaron-pc REMOTE_LSP_E2E_LANGUAGES=c \
  REMOTE_LSP_E2E_VISIT=logical REMOTE_LSP_E2E_OPEN_FOLDER=1 \
  REMOTE_LSP_E2E_PREWARM=1 make lsp-remote-live-smoke
```

The repeated-save profile then found six `process.run` calls per SSH write:
two ACL availability checks, two SELinux availability checks, and the actual
ACL read and restore.  The verified tramp-rpc 0.13.1 adapter now remembers a
successful availability check or a proven missing executable for 60 seconds
on the current RPC process.  Other errors and nonzero command exits are
retried, and a replacement process has an empty cache.  On Aaron-PC, two
separate enabled/disabled eight-write samples measured 65.0 versus 120.1 ms
median; a reversed-order 12-write sample measured 70.4 versus 109.6 ms.
The 12-write `process.run` count fell from 72 to 24 at that stage.  The
remaining commands read and restored ACLs on every write, although the pinned
tramp-rpc server's `file.write` truncates the existing inode in place and
therefore retains ACL and SELinux metadata.  The adapter now skips those
redundant generic TRAMP metadata round trips only for the verified client
revision on Emacs 31, with a setting to disable the optimization.  A CSE
12-write repeat measured 71.1 ms median before and 47.8 ms after; the
`getfacl`/`setfacl` calls fell from 12 each to zero.  A real CSE SSH test
wrote a file with a named ACL three times and then used `save-buffer`; its ACL,
inode, owner, group, and mode stayed unchanged.  This is a target-specific
write-path result, not a file-open or LSP speed claim.
An already visited `.txt` buffer measured through `save-buffer` showed the
same effect: 12 CSE saves had 119.2 ms median with generic metadata handling
and 89.6 ms with the shortcut.  Its `getfacl` and `setfacl` calls again fell
from 12 each to zero.  The remaining save path made 70 `file.stat` requests
over those 12 saves; tracing placed them in Emacs save checks, TRAMP write
setup, and repeated writable-file checks after each save.  A direct visit
through the selected physical `/rpc:` name made the same 70 `file.stat`
requests in 12 CSE saves (95.6 ms median).  This isolates the remaining
requests to Emacs/TRAMP's save and permission checks rather than the logical
`/fs:` route.  Some of those checks deliberately bypass the metadata cache
to detect external changes, so the adapter does not memoize them across
saves.  Reproduce the logical and direct visits with
`REMOTE_WRITE_MODE=save-buffer` and, for the direct visit,
`REMOTE_WRITE_VISIT=physical` on `make remote-ssh-write-benchmark`.

A later four-save CSE call-site trace counted 22 `file.stat` requests: 12
from save and lock-file modification checks, four from tramp-rpc's write
setup, and six from writable-file checks.  Removing these requests requires
a change to Emacs/TRAMP's save transaction semantics rather than another
`/fs:` route cache.  The trace also exposed a separate compatibility issue:
third-party advice made an upstream private function appear variadic, so
reinstalling the Remote handler silently disabled some verified tramp-rpc
optimizations.  The adapter now checks the original function's arity beneath
advice and still rejects a genuinely changed signature.  A live advised
reinstall kept the timeout, path-batch, and attribute-probe shims active on
the pinned 0.13.1 client.

The next Aaron-PC cold C profile traced about 0.33 seconds inside the first
`lsp-modeline` autoload on `lsp-configure-hook`.  Adding it to the client-side
preload removed that gap; the matched profile's initialization wait moved
from about 0.54 to 0.29 seconds.  A fresh uninstrumented logical `/fs:` C
smoke measured 0.286 seconds and passed diagnostics, completion, and a watch
event.  These are small same-host samples, not a controlled VS Code speed
comparison.  A separate real SSH recursive-watch test now forces a workspace
reconnect, verifies that the original public descriptor stays valid, and
observes a nested-file event through the replacement target process.

The C LSP recovery smoke now opens two source buffers in one workspace,
resets its session, and requires both to attach to the same replacement
server.  It passed on the local logical target, Aaron-PC `/fs:` files, and a
physical `/rpc:` buffer after opening the folder.  An injected transport
failure also passed the asynchronous recovery path on Aaron-PC, including
diagnostics, completion, and a fresh watch event.  Before the attempt-local
PATH cache, the instrumented automatic run made 20 target PATH probes and
took about 7.0 seconds; afterward it made 3 probes and took about 2.9
seconds.  The latter includes the test's post-recovery watch event.  These
paired host samples demonstrate the removed repeated probes; they are not a
general SSH reconnect guarantee.

A later isolated Aaron-PC test terminated only its own batch Emacs
tramp-rpc SSH process, leaving the target and other editor sessions alone.
Before the transport-exit fix, lsp-mode started a competing clangd while the
Remote owner stayed open, and the two-buffer recovery timed out after 15
seconds.  With the early 0.13.1 callback, both buffers reattached to one
new clangd and diagnostics, completion, and a new watch event passed in
3.77 seconds after the fault.  Disabling that callback in a separate batch
run exercised the version-independent LSP fallback and passed the same
checks in 3.58 seconds.  These are individual real SSH samples; longer uptime
remains untested.

A separate controlled half-open simulation left this batch Emacs's SSH
transport process alive but discarded its RPC replies.  The next logical
`file-attributes' read timed out in 5.07 seconds, retired that transport,
and automatically restored the workspace, both C buffers, clangd diagnostics
and completion, and a new file-watch event.  The complete failure-to-ready
interval was 8.81 seconds.  This tests the client timeout and recovery path.

Another Aaron-PC batch kept its SSH process running but discarded RPC reply
frames only inside that Emacs process.  A new `file.stat` query timed out in
5.04 seconds, retired the old transport, and recovered the two-buffer clangd
workspace, diagnostics, completion, and a watch event in 8.80 seconds from
the start of the fault.  This models a silent RPC peer.
After narrowing the deadline's dynamic scope to the actual file operation,
the same fault probe passed again in 8.61 seconds.  The eight-round local C
visit sample remained 78.7 ms native versus 81.6 ms `/fs:local:` median;
warm Aaron-PC logical metadata remained about 2.0–2.8 times direct `/rpc:`.

A new opt-in Aaron-PC smoke routed only its batch Emacs SSH connection through
a loopback TCP relay.  The relay discarded both directions for five seconds
without closing either socket; this exercised real OpenSSH keepalive and
process-exit handling rather than a tramp-rpc filter.  The test temporarily
overrode server-alive to one second and two unanswered probes so it finished
quickly.  In two initial runs, OpenSSH closed the stalled transport in 2.10
to 2.93 seconds; after the relay resumed traffic, the workspace, both C
buffers, clangd diagnostics and completion, and a new watch event recovered
in 13.83 to 14.60 seconds from fault injection.  One reconnect attempt began
while the relay was still dropping traffic.  After capping only the first
automatic connection attempt at three seconds, three runs of the same check
recovered all resources in 8.79 to 9.16 seconds, with SSH detecting the stall
in 2.18 to 2.60 seconds.  These are individual host samples; the test
validates the transport and
recovery mechanism on a real SSH target with a controlled TCP blackhole.
A physical network outage, default 15-second keepalive timing, and
long-running interactive sessions remain separate validation work.

## Repeat the checks

From the repository root:

```sh
emacs --batch -Q --eval '(setq user-emacs-directory (file-name-as-directory default-directory) load-prefer-newer t)' \
  -L lisp -L lisp/remote -L lisp/remote/backend -l test/remote-benchmark.el
make remote-check
REMOTE_E2E_TARGET=aaron-pc make remote-e2e
REMOTE_LSP_E2E_TARGET=aaron-pc REMOTE_LSP_E2E_VISIT=logical REMOTE_LSP_E2E_LANGUAGES=c REMOTE_LSP_E2E_RECONNECT=transport make lsp-remote-live-smoke
REMOTE_LSP_E2E_TARGET=aaron-pc REMOTE_LSP_E2E_VISIT=logical REMOTE_LSP_E2E_LANGUAGES=c REMOTE_LSP_E2E_RECONNECT=transport-fallback make lsp-remote-live-smoke
REMOTE_LSP_E2E_TARGET=aaron-pc REMOTE_LSP_E2E_VISIT=logical REMOTE_LSP_E2E_LANGUAGES=c REMOTE_LSP_E2E_RECONNECT=stall make lsp-remote-live-smoke
REMOTE_LSP_E2E_TARGET=aaron-pc REMOTE_LSP_E2E_VISIT=logical REMOTE_LSP_E2E_LANGUAGES=c REMOTE_LSP_E2E_RECONNECT=blackhole make lsp-remote-live-smoke
REMOTE_BENCHMARK_TARGET=aaron-pc make remote-route-benchmark
REMOTE_BENCHMARK_TARGET=aaron-pc REMOTE_VISIT_ROUNDS=8 REMOTE_VISIT_MAX_RATIO=1.20 make remote-ssh-visit-benchmark
REMOTE_BENCHMARK_TARGET=aaron-pc REMOTE_WRITE_ROUNDS=12 make remote-ssh-write-benchmark
REMOTE_BENCHMARK_TARGET=aaron-pc REMOTE_WRITE_ROUNDS=12 REMOTE_WRITE_CACHE=off make remote-ssh-write-benchmark
REMOTE_LOCAL_VISIT_ROUNDS=12 REMOTE_VISIT_MAX_RATIO=1.20 make remote-local-visit-benchmark
REMOTE_LSP_E2E_TARGET=aaron-pc REMOTE_LSP_E2E_LANGUAGES=c \
  REMOTE_LSP_E2E_REPEAT=2 make lsp-remote-live-smoke
REMOTE_LSP_E2E_TARGET=aaron-pc REMOTE_LSP_E2E_VISIT=logical \
  REMOTE_LSP_E2E_LANGUAGES=c REMOTE_LSP_E2E_REPEAT=2 make lsp-remote-live-smoke
REMOTE_LSP_E2E_TARGET=local REMOTE_LSP_E2E_LANGUAGES=c \
  REMOTE_LSP_E2E_REPEAT=2 make lsp-remote-live-smoke
REMOTE_LSP_E2E_TARGET=local REMOTE_LSP_E2E_VISIT=logical \
  REMOTE_LSP_E2E_LANGUAGES=c REMOTE_LSP_E2E_REPEAT=2 make lsp-remote-live-smoke
REMOTE_LSP_E2E_TARGET=local REMOTE_LSP_E2E_VISIT=logical \
  REMOTE_LSP_E2E_LANGUAGES=python make lsp-remote-live-smoke
REMOTE_LSP_E2E_TARGET=local REMOTE_LSP_E2E_VISIT=logical \
  REMOTE_LSP_E2E_LANGUAGES=c REMOTE_LSP_E2E_RECONNECT=1 \
  make lsp-remote-live-smoke
REMOTE_LSP_E2E_TARGET=aaron-pc REMOTE_LSP_E2E_VISIT=logical \
  REMOTE_LSP_E2E_LANGUAGES=c REMOTE_LSP_E2E_RECONNECT=auto \
  make lsp-remote-live-smoke
```

`test/remote-latency-profile.el` can be loaded between `init.el` and the live
smoke test to print phase timestamps and ELP call totals.  The live tests
create and remove only their own directories under the target's `/tmp`.

## Directory browsing and relay completion

`REMOTE_BENCHMARK_TARGET=aaron-pc make remote-directory-benchmark` creates
matching 400-file local and target `/tmp` fixtures, opens and refreshes them
through native, `/fs:local:`, direct `/rpc:`, and logical `/fs:` names, counts
RPCs, verifies every Dired listing is complete, and removes the fixtures.
`REMOTE_DIRECTORY_FILES` and `REMOTE_DIRECTORY_ROUNDS` control the workload.
In a 4,000-file, two-round run on 2026-09-25, warm local native and logical
opens were 67.9 and 65.2 ms; refreshes were 70.2 and 67.4 ms.  Warm SSH
direct and logical opens were 153.0 and 152.4 ms; refreshes were 232.2 and
130.4 ms.  Logical SSH open used three synchronous RPCs and refresh used two.
All four paths listed 4,000 of 4,000 files on every operation.  The first
native local open includes Dirvish package startup and is excluded from these
warm comparisons.
The complete managed `remote-open-folder` path opened the warm 400-file SSH
folder in 58.3 ms with three RPCs, compared with 57.2 ms for the bare logical
Dired open in that run.  Its workspace and progress handling did not add a
measurable warm delay.  The cold folder phase in the clangd SSH smoke was
0.36 seconds and includes connection and workspace setup.
A repeat after adding idle LSP preload measured warm 4,000-file opens at
68.2 ms native local versus 65.1 ms `/fs:local:`, and 164.2 ms direct SSH
versus 170.7 ms `/fs:`.  All listings remained complete; the idle Dired hook
did not create an observed local-open regression in that run.

On CSE, a 400-file, one-round check measured `/fs:` Dired open at 44.0 ms,
refresh at 34.9 ms, and managed `remote-open-folder` at 49.0 ms.  Direct
TRAMP open and refresh were 149.0 and 64.4 ms in the same process.  All 400
entries were present.  A separate balanced four-round source-visit check
with LSP auto-start disabled measured warm direct TRAMP and `/fs:` medians
of 138.0 and 138.4 ms respectively (eight visits per route).  The first
native-local Dired open included package startup and is not a warm baseline.

Rapidly opening, refreshing, and closing the large direct `/rpc:` listing
exposed a tramp-rpc 0.13.1 timer race: after an `ls` process exits, one queued
output callback can try to write to its
closed local cat relay.  Dired had already received its complete listing, but
Emacs logged a timer error.  The backend now suppresses only this exact
closed-write error when that same relay has its `:tramp-rpc-exited` flag set.
The advice is installed only for the verified client release and matching
private function arity, and is removed on a package upgrade.  A repeated
4,000-file run completed with no timer errors and complete listings.

## Upgrade boundary

The installed tramp-rpc checkout is the [current tagged release
`v0.13.1`](https://github.com/ArthurHeymans/emacs-tramp-rpc/releases/tag/v0.13.1)
as of 2026-09-25.
`lisp/init-tramp.el` and the generated `package-lock.el` now pin its client to
commit `e1d4632d576ecf2472c321de1e713b776ea2b78f`.  A fresh package
install therefore uses the same client revision as the verified local checkout.
Upstream `master` is ahead but still advertises server version `0.13.1`;
installing only its Lisp client could silently pair changed code with the
published release server.  Keep the matching release client and binary until
the newer pair can be deployed and tested together.  The backend's contract
report, exact release check, and private arity checks are the compatibility
gate for a future upgrade.  The private shims now require the verified Git
revision as well as the version and clean release tag; a same-version build
from different source takes the upstream fallback.  The in-place metadata
shortcut additionally requires Emacs 31 and the expected tramp-rpc write
handler arity; other versions retain upstream ACL handling.  A simulated
newer release test confirms that stale private advice is removed.  The actual
0.13.1 initialization reports the guarded workarounds, including the
attribute-probe cache, in-place metadata shortcut, and closed-relay guard,
active.
When a newer release is published, update the client pin and remote server as
a pair, regenerate the lock with `make lock`, and run `make remote-check`,
`make remote-e2e` against a real SSH target, and the LSP reconnect smoke test
before treating the new client as verified.  Until its private API checks are
updated, Remote keeps using the upstream fallback paths.

The isolated `remote-check` suite now loads its installed msgpack dependency
for the large process-environment compatibility test.  That test runs and
passes instead of being skipped by the `-Q` package load path.

## Target home and symlink paths

The [TRAMP file-name contract](https://www.gnu.org/software/emacs/manual/html_node/tramp/File-name-syntax.html)
expands `~user` on the target.  The installed Emacs 31 build instead expanded
`/ssh:Aaron-PC:~aaron/` and `/rpc:Aaron-PC:~aaron/` to
`/home/aaron/~aaron/`.  Remote now asks the selected TRAMP backend for that
named account's home before converting the result to a logical `/fs:` path.
For the pinned tramp-rpc 0.13.1 release, a named current account reuses the
server's HOME after one `id -un` check; this avoids that client's `getent`
dependency on targets without `getent`.  A forced no-`getent` probe passed on
Aaron-PC; macOS itself has not been exercised.
The ordinary `~/` path still follows the backend's existing expansion.  A
real Aaron-PC test passes through both standard SSH/TRAMP and tramp-rpc for
`~/`, `~user/`, relative, absolute, dangling, and cross-directory symlinks;
the resulting truenames retain their `/fs:` identity.

The warm route microbenchmark now primes the first SSH ControlMaster
liveness check after GC.  With 300 queries on Aaron-PC, routed
`file-attributes` still measured 108.2 µs per call versus 41.4 µs directly
on `/rpc:`.  A separate component probe measured about 18.5 µs in route
validation, 6.9 µs in leased connection acquisition, and 22.7 µs in path
projection.  These are fixed per-call costs, so the result does not imply a
similar multiplier for whole-file visits or LSP operations.

## Python toolchain startup

A paired local Python LSP smoke showed that native paths and `/fs:local:` had
nearly identical editing costs: 40 simulated keypresses measured about 0.76 ms
median in each, with Company `prin` completion around 15–20 ms.  Both routes
still spent about 1.84 seconds in the cold LSP start request.  A timestamped
profile found 0.95 seconds in two Conda JSON commands and 0.23 seconds in a
Sage command while enumerating *all* toolchains, even though automatic startup
selected the ordinary PATH Python.

Automatic Python startup now discovers project `.venv`/`venv` and PATH Python
without running Conda or Sage.  The explicit toolchain picker and a configured
Conda/Sage selection still use full discovery; the two candidate sets have
separate caches, and refresh or provider replacement invalidates both.  On
fresh batch runs, native and `/fs:local:` Python LSP start-request phases fell
from 1.85/1.84 to 0.65/0.66 seconds, while diagnostics, completion, and watch
checks passed.  Full local discovery still listed Conda base, another Conda
environment, Sage, and the two PATH Python installations.  CSE's Python smoke
also passed; its start request stayed around 0.73 seconds, so this local result
does not establish a CSE startup speedup.  These are startup phase
measurements, not GUI redisplay timings.

## CSE login host

`login.cse.unsw.edu.au` now selects tramp-rpc first, with standard TRAMP as
the configured fallback.  The RPC server deployed successfully to that Linux
x86_64 target.  In separate batch runs, warm `/fs:` directory listing fell
from 2.37 ms on SSH/TRAMP to 0.39 ms on RPC; Python LSP start request fell
from 2.18 s to 0.85 s.  The real Python smoke passed diagnostics, completion,
symbols, and watch events over RPC.  The actual `Desktop/a.py` path read
successfully.  The two runs are not a controlled typing-latency measurement.

A later real completion probe reproduced the reported 10 s Company timeout on
both SSH/TRAMP and RPC.  The LSP server answered the same raw request in 18 ms;
Emacs had been re-sending `workspace/didChangeConfiguration` from
`lsp-configure-hook` whenever a server capability registration reconfigured
the buffer.  One 14 s probe recorded 129 configuration notifications and 322
responses ahead of completion.  Configuration is now supplied through the
server's `workspace/configuration` request; a live setting change is announced
at most once per workspace generation.  The CSE Python smoke now forces actual
CAPF candidates after a symbol request and passed in 40 ms.  On the screenshot's
`Desktop/a.py`, an independent read-only batch session measured 1.335 s for
the first completion and 27 ms for the next uncached request.  The GUI's
typing latency has not been measured directly.

The remaining first-request delay came from Python's cold server completion
path.  The production LSP lifecycle now issues one asynchronous completion
request after a Python workspace initializes.  It leaves the source buffer
untouched, deduplicates by server process, and starts again after a new process
replaces an old one.  On the same existing `Desktop/a.py` file, a paired batch
probe measured 959 ms for Company `prin` completion without warmup and 47.6 ms
with warmup; the warmup itself took 1.08 s in the background.  A repeat after
the final compatibility guards measured 1137 ms without warmup and 38.6 ms
with warmup.  A fresh 2026-09-26 check of that same file initialized Python
LSP in 1.01 s, completed warmup in the background, and returned Company's
`print` candidate in 35.0 ms.  The test restored the unsaved buffer and did
not save the file.  Reproduce with:

```sh
REMOTE_LSP_EXISTING_FILE='/fs:login.cse.unsw.edu.au:/import/reed/7/z5586493/Desktop/a.py' make lsp-existing-file-live-probe
REMOTE_LSP_EXISTING_FILE='/fs:login.cse.unsw.edu.au:/import/reed/7/z5586493/Desktop/a.py' REMOTE_LSP_EXISTING_PREWARM=0 make lsp-existing-file-live-probe
```

There was still a short window where typing immediately after initialization
could make Company's automatic completion enter the cold synchronous request
before the warmup finished.  While the Python warmup is pending, the Company
idle check now defers that automatic request and remembers the buffer position.
When warmup finishes, it resumes the popup only if the same visible buffer,
text and point remain.  Explicit completion remains available.  On the actual
`Desktop/a.py`, an immediate simulated typing check took 0.015 ms to defer the
automatic request; after warmup, the popup appeared with `print`, and a later
direct Company request returned in 44 ms.  Reproduce with
`REMOTE_LSP_EXISTING_EARLY=1` on the first command above.

A later screenshot showed Company could still wait for a full synchronous
`textDocument/completion` timeout in an interactive session.  Remote Company
CAPF requests now use a one-second response deadline.  On timeout, that
buffer briefly skips LSP completion and offers local code-word candidates,
then sends one asynchronous completion health request.  A response clears the
fallback immediately; an eight-second health timeout rebuilds the owning
Remote LSP resource at most once per workspace restart window.  Until that
decision, further keystrokes use local candidates without another synchronous
wait.  This is scoped to managed nonlocal buffers; local completion and other LSP requests retain their existing
timeouts.  A separate Emacs timer enforces the same deadline if a future
`lsp-mode` release changes its timeout error text or wait path.  The same
`Desktop/a.py` read-only live probe after the change returned `print` in
41 ms.  A controlled missing-response probe in that real CSE buffer returned
after about 250 ms with a test-only 250 ms deadline, skipped the next
synchronous request in about 0.008 ms, and received a healthy asynchronous
reply in about 98 ms.  A second fault probe suppressed both replies: the
versioned Remote resource path stopped the old Python server, attached a new
one, finished prewarm, and returned `print` in about 2.5 s with a shortened
test-only health deadline.  Removing the LSP resource record in a second
CSE fault probe exercised the fallback path and recovered the same source in
about 2.3 s.  Both paths now use one source-buffer restart routine, which
also reattaches sibling buffers.  Focused tests cover the ordinary timeout,
independent deadline, retry suppression, generation cleanup, rate-limited
restart, recovery, and unchanged local behavior.  The interactive timeout's underlying
cause remains unconfirmed because that Emacs session was unavailable to probe.

An independent logical-route smoke with 40 simulated Python keypresses,
including command hooks, measured 1.20 ms median and 1.37 ms p95 per keypress
on CSE.  Its actual Company `prin` request returned in 35 ms.  These batch
numbers isolate the LSP/Company path; they do not measure GUI redisplay,
physical keyboard latency, or VS Code Remote SSH on the same machine.

A final three-language CSE LSP probe passed for clangd, Python and JDTLS:
each initialized on the RPC route and supplied diagnostics, symbols, a
completion provider and file-watch events.  Python returned four actual CAPF
candidates in 30 ms in that run.  lsp-java's `locate-file` had returned nil
for an executable `/fs:` JDTLS launcher, dropping into its jar startup path;
the Java adapter now checks the logical target executable directly.  The
smoke probe now reads completion state after its asynchronous postchecks so
JDTLS startup cannot race that assertion.  These are batch checks, not GUI
typing or a head-to-head VS Code measurement.

## Remote LSP editing latency

An opt-in profile of 40 simulated Python keypresses showed the remaining
CSE/local editing gap in `lsp-on-change`: CSE spent about 26 ms there versus
6 ms locally.  The nested process path sent 42 individual `didChange`
notifications and spent about 16 ms in tramp-rpc process writes.  A temporary
full-document debounce experiment reached 0.65 ms per keypress, but the
production path keeps each server's advertised incremental sync method.

Remote incremental changes now retain their original ranges and order in a
bounded queue.  Adjacent changes to one document are sent as a single LSP
`contentChanges` array on a 30 ms timer; any outgoing LSP request or other
notification flushes the queue first.  Process replacement discards the old
generation's queue, and unsupported message shapes or sync modes use lsp-mode's
ordinary send path.  If a synchronous send raises, the failed change and all
later changes stay queued, and the next request retries them before proceeding;
a focused fault test covers this ordering.  The same 40-key CSE Python profile
sent 19 process writes instead of 42.  An uninstrumented CSE smoke measured
0.83 ms median and 0.95 ms p95 per keypress, versus about 1.25 and 1.38 ms
before batching.
The native local route remained about 0.77 ms median.  CSE clangd, Python
and JDTLS initialization, diagnostics, completion, and file watches passed;
the Python transport-loss smoke also recovered both source buffers, completion
and the watcher.  These are batch edit-hook timings and do not include GUI
redisplay or physical keyboard latency.

An additional CSE live check inserted an unsaved Python function while its
change was queued.  The immediately following `textDocument/documentSymbol`
request found the function; after deleting it, the queue drained during an
idle period and a second request no longer found it.  This verifies the real
server sees both forced and timer-driven flushes.  A repeat after the send
failure guard measured 0.60 ms median and 0.94 ms p95 for 40 CSE Python
edits, with the same document-symbol and completion checks passing.  Run it with:

```sh
REMOTE_LSP_E2E_TARGET=login.cse.unsw.edu.au \
  REMOTE_LSP_E2E_LANGUAGES=python REMOTE_LSP_E2E_VISIT=logical \
  REMOTE_LSP_E2E_CHANGE_BATCH=1 REMOTE_LSP_E2E_TYPING_ROUNDS=40 \
  make lsp-remote-live-smoke
```

The same queued-edit check also passed while visiting the ordinary `/rpc:`
buffer rather than `/fs:`; its 40-key median was 0.77 ms on CSE.  Both buffer
spellings use the same workspace-owned LSP process and transport generation.

For a per-hook profile, load `test/remote-typing-profile.el` after
`test/lsp-remote-live-smoke.el` and set `REMOTE_LSP_E2E_TYPING_ROUNDS=40`.

An interactive `emacs -nw` probe now measures forced terminal redisplay after
each edit, using the same live Python LSP smoke.  In a 40-edit, 80×24 pseudo
terminal on 2026-09-26, local native, `/fs:local:`, and CSE `/fs:` edit-hook
medians were 0.83, 0.88, and 0.92 ms; redisplay medians were 1.22, 1.39,
and 1.38 ms.  CSE edit-hook and redisplay p95 were 1.21 and 2.01 ms.
At 180×55, local native versus CSE `/fs:` edit-hook medians were 0.83 versus
0.97 ms and redisplay medians 2.15 versus 2.32 ms; CSE p95 values were 1.08
and 3.09 ms.  The larger terminal increased redraw time on both routes.
Completion, diagnostics, file watches, and snippets passed in every run.
This measures Emacs writing to a terminal device, including its redraw work;
it does not include a physical key event or the terminal emulator's rendering.
Run the opt-in probe from a terminal with, for example:

```sh
REMOTE_LSP_E2E_TARGET=login.cse.unsw.edu.au \
  REMOTE_LSP_E2E_LANGUAGES=python REMOTE_LSP_E2E_VISIT=logical \
  REMOTE_LSP_E2E_TYPING_ROUNDS=40 make lsp-remote-tty-smoke
```

A subsequent paired 80×24, 100-edit repeat with the completion guard loaded
measured local native versus CSE `/fs:` edit-hook medians of 0.877 versus
0.957 ms and forced redraw medians of 1.265 versus 1.394 ms.  Both runs
passed diagnostics, completion, snippets, and watch events.  Their Company
`prin` requests took 19 and 47 ms, respectively.  This is terminal redraw
and simulated editing evidence, not a controlled VS Code SSH comparison.

A 220×70 TTY repeat, closer to the reported full-screen terminal size,
measured local native versus CSE `/fs:` median edit hooks at 0.26 versus
0.96 ms and forced redisplay at 1.37 versus 2.97 ms.  Company completion
returned in 18 versus 41 ms.  An instrumented repeat raised both routes'
costs: local versus CSE edit hooks were 1.03 versus 1.17 ms and redisplay
2.82 versus 3.13 ms.  The profiler observed zero `tramp-rpc` calls during
the 40 CSE redraws.  Its inclusive hook totals included 9.1 ms for 20
asynchronous remote process writes; this is the LSP transport cost outside
redisplay.  The two runs show profiler overhead and ordinary timing variance,
so they do not establish a stable 220×70 ratio.  Set
`REMOTE_TYPING_PROFILE_OUTPUT=/tmp/emacs-typing-profile.txt` and load
`test/remote-typing-profile.el` after the live smoke to capture the per-hook
report; the instrument is opt-in and leaves production hooks unchanged.

### Real PTY key-to-output latency and idle breadcrumb stall

A 220×70 interactive Emacs PTY probe exposed a separate stall that the
simulated edit and forced-redisplay timings above could not see.  With the
Python LSP initialized on CSE, the first key sometimes failed to reach the
Emacs command hooks or terminal output within 12 seconds.  A sample of the
blocked Emacs main thread showed `lsp-headerline-check-breadcrumb` running
from `lsp--on-idle`; its project segment called `lsp-workspace-root`, then
`lsp-f-same?`, then TRAMP `file-exists-p`, and waited inside the remote shell
connection.  In target-only LSP buffers the breadcrumb now keeps its file
and symbol segments, while omitting the project segment that performs the
remote file test on each idle refresh.  Native and shared-client files keep
the full breadcrumb; the project remains visible in the existing tab/window
context for target-only files.

With that change, a real PTY driver sent 24 unique UTF-8 keys one at a time
to a fully initialized Python LSP buffer and timed each key until its bytes
appeared in Emacs terminal output.  It waited for two seconds of quiet after
startup to measure active editing.  Local native measured 10.29 ms median,
44.54 ms nearest-rank p95, and 56.36 ms maximum; CSE `/fs:` measured
11.23 ms median, 19.23 ms nearest-rank p95, and 53.31 ms maximum.  The
first few keys and one local key showed cold or background-work spikes; the
median gap was 0.94 ms in these
single 24-key runs.  The probe visits the existing remote source but never
saves it.  Run it with:

```sh
REMOTE_KEY_SCREEN_FILE=/fs:login.cse.unsw.edu.au:/path/to/file.py \
  make lsp-key-to-screen
```

This measures input delivery, Emacs command hooks, redraw, and bytes written
to a PTY.  It excludes Kitty's pixel rendering and physical keyboard event
delivery, and is not a controlled VS Code SSH comparison.  A subsequent
paired local-native versus `/fs:local` run with the same 24-key driver and
Python LSP measured 10.06 versus 10.22 ms median.  Their cold/background
spikes differed, so the paired medians support only a warm typing comparison.
After switching the breadcrumb guard to the framework's client-accessible
path query, CSE repeated at 10.67 ms median and 19.36 ms nearest-rank p95.
With `REMOTE_KEY_SCREEN_LSP=0`, the generic breadcrumb remained enabled and
CSE returned a 10.23 ms median; this did not reproduce the idle LSP stall.
The repository's LSP UI regression tests assert that target-only breadcrumbs
never call `lsp-workspace-root` while building their visible string and that
a client-accessible target keeps the project segment.

The same PTY driver also visited the screenshot's file directly through the
physical `/rpc:` spelling, rather than `/fs:`.  With 400 ms between keys,
its 24-key median was 31.90 ms, p95 50.44 ms, and maximum 57.54 ms; seven
real completion requests ran.  A separate `/fs:` run on the same file
measured 36.83 ms median, 60.67 ms p95, and 64.67 ms maximum with four
completion requests.  These are separate sessions, so the few-millisecond
difference is not a route speed ranking.  A direct `/rpc:` run that dropped
one completion reply still displayed all 24 keys, with 31.50 ms median and
48.19 ms maximum.  The probe now requires both target-only spellings to
activate the remote completion guard and the breadcrumb policy.  This
checks the physical visit path shown by ordinary TRAMP users as well as
Remote's logical path.

A second PTY scenario types the screenshot's `prin` into the real CSE
`a.py` buffer and waits for Company's actual `print` candidate.  A local
run reached the candidate in 401.64 ms, logical `/fs:` in 447.00 ms and
physical `/rpc:` in 703.17 ms.  Each made one real LSP completion request.
The time includes Company's 280 ms idle trigger, terminal delivery, server
response and candidate preparation; these separate runs do not establish
a stable route ranking.  The TTY frontend did not draw a tooltip for its
single candidate, so this probe asserts candidate readiness rather than a
GUI child-frame popup.

Dropping the first reply in this exact `prin` scenario originally exposed a
recovery gap: the asynchronous LSP health check succeeded, but Company did
not retry until the next edit.  A restored health check now schedules one
automatic Company retry for the same visible buffer, window, edit generation
point, with no intervening input event.  An edit, cursor move or new input
invalidates the retry, and a repeated
timeout at that edit generation cannot schedule another automatic retry.
With the first reply dropped on the physical CSE path, `print` appeared in
1456.56 ms after three completion requests: initial, health check, retry.
The added ERT guard verifies that stale point and edit contexts do not retry.

```sh
REMOTE_KEY_SCREEN_SCENARIO=python-completion \
  REMOTE_KEY_SCREEN_FILE='/rpc:login.cse.unsw.edu.au:/path/to/a.py' \
  python3 test/remote-key-to-screen.py remote
REMOTE_KEY_SCREEN_SCENARIO=python-completion \
  REMOTE_KEY_SCREEN_STALL_COMPLETION=1 \
  REMOTE_KEY_SCREEN_FILE='/rpc:login.cse.unsw.edu.au:/path/to/a.py' \
  python3 test/remote-key-to-screen.py remote
```

```sh
REMOTE_KEY_SCREEN_FILE='/rpc:login.cse.unsw.edu.au:/import/reed/7/z5586493/Desktop/a.py' \
  REMOTE_KEY_SCREEN_INTERKEY_MS=400 python3 test/remote-key-to-screen.py remote
REMOTE_KEY_SCREEN_FILE='/rpc:login.cse.unsw.edu.au:/import/reed/7/z5586493/Desktop/a.py' \
  REMOTE_KEY_SCREEN_INTERKEY_MS=400 REMOTE_KEY_SCREEN_STALL_COMPLETION=1 \
  python3 test/remote-key-to-screen.py remote
```

Repeated unpaced CSE runs exposed an intermittent exit failure after every
key had already reached the terminal: Emacs stayed in its command loop after
`kill-emacs`.  The PTY probe recorded a nonlocal unwind inside
`copilot--shutdown-server-at-exit`, accompanied by Copilot's “Agent service
shut down” response to an in-flight inline completion.  For the client-owned
Copilot process that this configuration starts with `noquery`, the exit hook
now skips its synchronous JSON-RPC shutdown request and lets Emacs close the
child on exit.  Other connection shapes keep the upstream hook.  Two unpaced
logical CSE runs and one direct `/rpc:` run then exited normally with Copilot
enabled; medians were 10.63, 11.05 and 10.75 ms for 24 keys.  No Copilot
process remained after the run.  A focused test verifies that a process which
would query on exit still uses the upstream shutdown path.  The change affects
editor exit only; inline completion remains enabled while editing.  A
post-fix direct `/rpc:` run that dropped one completion reply still displayed
all 24 keys and exited normally, with 32.58 ms median and 58.13 ms maximum.

To let Company's 280 ms idle completion timer run between keys, the same
driver also paced input at 400 ms per key.  In single local and CSE runs,
the 24-key medians were 45.02 and 38.97 ms; the probe counted three and six
real `textDocument/completion` requests respectively.  A CSE run that
dropped the first automatic completion reply still displayed every key,
with a 35.55 ms median and 49.10 ms maximum, and issued later completion
requests.  A paced local run with LSP disabled measured 43.14 ms median, so
the approximately 40 ms paced baseline cannot be attributed to LSP or
Company alone.  This exercises automatic popup behavior; the separate existing
file CAPF fault probe verifies the bounded synchronous timeout and health
recovery path.  Reproduce the paced or fault-injected test with:

```sh
REMOTE_KEY_SCREEN_INTERKEY_MS=400 REMOTE_KEY_SCREEN_FILE=/fs:login.cse.unsw.edu.au:/path/to/file.py \
  make lsp-key-to-screen
REMOTE_KEY_SCREEN_INTERKEY_MS=400 REMOTE_KEY_SCREEN_STALL_COMPLETION=1 \
  REMOTE_KEY_SCREEN_FILE=/fs:login.cse.unsw.edu.au:/path/to/file.py \
  python3 test/remote-key-to-screen.py remote
```

### GUI Company popup on the screenshot's file

An isolated graphical Emacs daemon now checks the visible Company-box child
frame, its parent frame, its buffer text and the `company-capf` backend while
visiting the existing CSE `a.py`.  It edits the source only in memory.  The
runner checks native local, `/fs:local:` and CSE; all three produced `print`
in visible child frames.  In a later native/logical-local/CSE run, time from
the explicit idle callback through redisplay was 90.69/46.81/44.54 ms.
The first local child-frame creation affects this ordering; it is a
correctness and rough latency check, not a route ranking.  An earlier paired
local/CSE run measured 91.02/52.69 ms for the same interval; the LSP response
portions were
2.35 and 42.07 ms.  Three subsequent direct completion requests were
0.91–1.77 ms locally and 17.66–28.07 ms on CSE.  An earlier separate CSE
GUI visit took 409 ms to prepare its first popup, so the one paired run is
not a stable claim that remote is faster.

Order reversal clarified both effects.  In a fresh CSE-first GUI daemon,
the first candidate took 256.77 ms from idle trigger through redisplay:
40.60 ms for the LSP response, 45.91 ms inside Company-box drawing and
165.96 ms in the following GUI redisplay.  In a separate native-first run,
native's first popup took 91.80 ms, including 31.79 ms in Company-box and
56.20 ms in redisplay; the later CSE popup took 94.17 ms, including 4.29 ms
in Company-box and 4.91 ms in redisplay.  Thus cold GUI popup creation and
redraw can dominate the first visible completion, while their warm costs
are small.  These runs do not isolate which part of the cold GUI work is
specific to a remote buffer.

The local-versus-`/fs:local:` first completion response followed visit order
rather than path spelling: native-first measured 2.28/43.04 ms, and
logical-first measured 2.63/45.37 ms.  Five later direct completion requests
on each path were about 0.6–2.1 ms.  This rejects the apparent 40 ms
`/fs:local:` penalty from the native-first run as a route-specific finding;
the paired GUI runner preserves the order control with
`REMOTE_GUI_COMPANY_ORDER=logical-first` or `remote-first`.

The same GUI runner dropped the first automatic completion reply on CSE.
The recovered `print` candidate appeared in a visible child frame after
1.17 seconds, with the `company-capf` backend still active.  This directly
checks the recovery path that previously left the screenshot's `prin`
without a candidate.  The GUI probe starts at Company's idle callback, so
its times exclude the configured 280 ms idle delay and physical key input;
the PTY probe above covers actual key delivery.

The runner can also execute `prin` through Emacs' GUI command loop and let
Company's real idle timer start the request.  In one native/`/fs:local:`/CSE
run, candidate readiness took 408.91/421.39/377.63 ms from the first
synthetic command; the first LSP requests started about 325–344 ms in.
Another run took 410.64/434.71/613.01 ms, with the CSE LSP response only
25.05 ms and a later 244.56 ms gap between child-frame drawing and the
test's next observation.  A subsequent run did not repeat that gap, so it
remains a tail-latency sample rather than a stable remote cost.  With the
first completion reply dropped, the CSE GUI automatically recovered a
visible CAPF `print` candidate in 1.50 seconds from synthetic input.
`execute-kbd-macro` exercises Emacs commands, hooks and timers; it is not
physical keyboard-to-pixel instrumentation.

Python's Company idle delay is now capped at 120 ms only after that LSP
workspace has successfully prewarmed completion on its current server
process.  Cold, failed and replaced generations keep the ordinary 280 ms
delay; the setting can be disabled independently.  In fresh same-order CSE
GUI runs, the 280 ms control began its request at 330 ms after synthetic
`prin` and observed the popup at 395 ms.  The 120 ms run began its request
at 169 ms and observed a visible `print` child frame at 232 ms.  These are
separate single runs, not a latency distribution.
Other runs showed 100–250 ms gaps between child-frame drawing and the
test's next observation, so these are examples rather than a stable popup
median.  The request-start change is the controlled part of this tuning.
A dropped first reply still recovered the visible CSE popup in 1.35 s.
Reproduce the control with
`REMOTE_GUI_COMPANY_FAST_IDLE=0`; the PTY control uses
`REMOTE_KEY_SCREEN_FAST_IDLE=0`.

For 24 real PTY keys paced 400 ms apart on CSE, the ordinary versus fast
idle runs had key-output medians of 28.46 versus 31.56 ms and p95 values of
50.45 versus 51.60 ms.  The request counts varied with Company scheduling;
these two runs do not quantify completion server load.

```sh
REMOTE_GUI_COMPANY_FILE='/rpc:login.cse.unsw.edu.au:/path/to/a.py' \
  REMOTE_GUI_COMPANY_REQUEST_ROUNDS=3 REMOTE_GUI_COMPANY_STALL=1 \
  make lsp-gui-company-popup
REMOTE_GUI_COMPANY_FILE='/rpc:login.cse.unsw.edu.au:/path/to/a.py' \
  REMOTE_GUI_COMPANY_AUTOMATIC=1 REMOTE_GUI_COMPANY_STALL=1 \
  make lsp-gui-company-popup
```

### GUI redraw and local-path control

An isolated macOS GUI Emacs process loaded this configuration and ran the
same real Python LSP smoke in a 180×55 frame.  In one process, a
native/CSE/CSE/native sequence measured forced full-frame redraw medians of
6.92/10.87/11.47/6.94 ms over 40 synthetic edits per buffer.  The edit-hook
medians were 1.15/1.28/1.32/1.21 ms.  All four runs passed diagnostics,
completion, and watch checks.  Their source buffers had the same 60-character
length and five overlays; semantic tokens and inlay hints were inactive.
At 120×50, separate local and CSE runs were both about 9 ms per forced
redraw, so the larger-frame gap is not a uniform per-key cost.

A `/fs:local:` Python LSP run at 180×55 measured 7.02 ms redraw and
1.16 ms edit-hook medians over 60 edits, close to the native GUI result.
This isolates the larger-frame difference from the logical `/fs:` spelling.
With the target LSP disabled, an alternating native/CSE sequence measured
3.01/3.76/4.07/3.06 ms redraw medians for the same small source; the larger
LSP-enabled gap is therefore not explained by the file-name handler alone.
An opt-in 40-edit CSE profile observed zero synchronous tramp-rpc requests
during redraw.  File-handler and process operations during redraw totaled
about 19 ms over all 40 edits, roughly 0.5 ms per redraw with instrumentation
overhead.  CPU sampling placed most of the remaining difference in Emacs'
native `redisplay_internal`; disabling tab, mode, and header lines in the
synthetic probe reduced the CSE median only slightly.  This does not identify
a safe package-level change for the remaining GUI gap.

The benchmark calls `redisplay` with a force flag after each synthetic edit;
it does not measure physical keyboard delivery or Cocoa pixels.  The PTY
key-to-output probe above covers actual byte delivery and its warm medians
remain near local.  Run the GUI control or the paired comparison with:

```sh
REMOTE_LSP_E2E_TARGET=local REMOTE_LSP_E2E_VISIT=logical \
  REMOTE_LSP_E2E_LANGUAGES=python REMOTE_LSP_E2E_FRAME_COLUMNS=180 \
  REMOTE_LSP_E2E_FRAME_ROWS=55 make lsp-remote-gui-smoke
REMOTE_LSP_E2E_TARGET=login.cse.unsw.edu.au REMOTE_LSP_E2E_PAIRED=1 \
  REMOTE_LSP_E2E_LANGUAGES=python REMOTE_LSP_E2E_FRAME_COLUMNS=180 \
  REMOTE_LSP_E2E_FRAME_ROWS=55 make lsp-remote-gui-smoke
```

## SSH terminal route

A real CSE PTY probe found that tramp-rpc 0.13.1 expanded its local
ControlMaster socket path while a target buffer was current.  The resulting
path used the target account's home, so SSH exited before the shell started.
The version-checked adapter now resolves both the socket path and its parent
directory in the client environment.  The same probe passed target cwd,
environment, keyboard input, and shell output on both tramp-rpc and standard
TRAMP.  Process requests with a PTY now select the `pty` capability rather
than `process-async`.  A first implementation preferred standard TRAMP but
still spent about 1.09 s on Aaron-PC opening its file session before the
first terminal; its second terminal took 49 ms.  The SSH import now offers a
dedicated `ssh-pty` backend on all imported hosts.  It reuses the pipeline's
existing OpenSSH ControlMaster and direct client PTY process plan without
creating a TRAMP file session.  With the same fixed Aaron-PC host and an open
RPC workspace, the first terminal took 87 ms, the second 77 ms, and 20 warm
round trips had 5.57 ms median and 13.90 ms p95.  CSE measured 48 ms, 57 ms,
6.40 ms, and 14.77 ms respectively in a separate run.  Opening an Aaron-PC
terminal without a prior workspace took 788 ms, including initial workspace
and SSH setup.  Actual VTerm batch probes on both targets confirmed the
`ssh-pty` route, workspace tracking, remote `/tmp` cwd and shell output.
The first batch VTerm call in an already open Aaron-PC workspace still spent
about 2.55 s loading the client VTerm package before its routed launch took
about 40 ms.  Preloading that package reduced the measured launch to 39 ms;
the first shell command then returned after about 705 ms, which includes the
target shell startup.  The interactive configuration now schedules VTerm
package loading on idle after editor startup or workspace open, using the
client environment.  A cold VTerm call that must also open its workspace took
about 1.57 s in one Aaron-PC batch run.  These are batch process timings; an
existing GUI session may have a different package cache and rendering cost.
Workspace close keeps pooled sessions available for a quick reopen, but Emacs
exit now closes those sessions and their owned SSH ControlMaster sockets.  A
real Aaron-PC probe verified its live socket was gone after shutdown instead
of waiting for the 600-second OpenSSH persistence timer.
Standard TRAMP and tramp-rpc remain PTY fallbacks.  Physical GUI keyboard
latency and a controlled VS Code comparison remain unmeasured; the synthetic
GUI redraw and PTY key-to-output probes above cover separate editor paths.
An opt-in CSE fault probe also killed only the batch Emacs client's RPC
transport while the RPC PTY was waiting for input.  The terminal stayed
visible as `disconnected`; after workspace reconnection, explicit terminal
restart restored cwd, environment, and bidirectional input/output.  A PTY
process exit fault on Aaron-PC likewise preserved the buffer and restart path.
Physical disconnect remains to be measured.

## Workspace tasks

Build and test commands now use `remote-task-run` on the same owning workspace
and process route as the source buffer.  Compilation mode keeps errors
clickable; target-absolute file names are projected back to `/fs:` identity.
The real CSE `Desktop/a.py` probe visited that same logical buffer from a
task error, verified the target cwd and environment, checked exit code 7, and
cancelled a 30-second task.  With tramp-rpc, closing its Emacs process also
ended the target process.  With standard TRAMP, that alone left the target
process running, so task cancellation now records the target PID and sends a
target-side TERM before closing the local relay.  The same CSE probe then
confirmed the target process ended on both routes.  No build throughput or
VS Code task latency has been measured here.
Immediate cancellation before the PID frame arrives also passed on both CSE
routes.  Compilation is loaded only when a task actually starts, and the task
footer no longer depends on Compilation mode's private start-time variable.
The task wrapper checks `setsid -w` exit-code behavior before using a process
group; the CSE probe then verified cancellation of a spawned child on both
tramp-rpc and standard TRAMP.  A selected process route is recorded on its
owning workspace.  Transport failure matching uses the shared transport key
(target plus pipeline), so sibling backends recover together while an
unrelated target with the same pipeline name remains open.
Killing only the batch Emacs client's RPC transport while a CSE task was
running exposed a synthetic relay exit zero, even though the target result
was unknown.  The owning workspace now informs task resources of transport
failure before their sentinels run.  The real fault probe reports
`interrupted`, keeps the target exit code unknown, and verifies automatic
workspace recovery.  Tasks are not replayed after reconnection.  The task
output buffer's `g` key runs the same command, directory, and invocation
environment again only after the workspace is live.  The CSE fault probe
started and cancelled that manual rerun on the recovered RPC route.
Cancellation now waits asynchronously for target-side TERM confirmation before
closing its relay, with a five-second bound.  The confirmation probe reads the
target process state directly; a lost transport or failed confirmation is
shown as `cancel-unconfirmed` because closing the local relay alone cannot
prove that the target process exited.  The CSE fault probe verified the
recovered task's PID was gone after cancellation.  A late local relay exit
now preserves `cancel-unconfirmed` and leaves the target exit code unknown;
the isolated lifecycle test covers that callback order.

## Python debugging

The Dape `python-file` and `python-module` aliases now run the target's
`python3 -m debugpy.adapter` over process stdio.  Dape's socket default
chooses a local port, while its automatic host inference only recognizes the
`ssh` TRAMP method; a logical `/fs:` source could therefore start a target
adapter and then try to connect to the client machine.  The stdio route
avoids that port and uses Remote's ordinary target process handler.  Dape's
existing TRAMP path projection maps the adapter's source paths back to `/fs:`.
The target Python must have `debugpy` installed.  Local-target and CSE live
probes for both file and module launch verified that the adapter and Dape's integrated
terminal processes retained the same target route, stopped at the Python
entry point, resolved the stack frame to the original logical source,
continued to normal exit, and displayed the program's `42` output.  Run the
opt-in tests with:

```sh
REMOTE_DEBUG_E2E_TARGET=login.cse.unsw.edu.au make remote-debug-live-smoke
REMOTE_DEBUG_E2E_TARGET=login.cse.unsw.edu.au REMOTE_DEBUG_E2E_MODE=module make remote-debug-live-smoke
REMOTE_DEBUG_E2E_TARGET=login.cse.unsw.edu.au REMOTE_DEBUG_E2E_MODE=attach make remote-debug-live-smoke
```

`my/debug-python-attach` (debug menu key `y`) attaches to a debugpy listener
bound to `127.0.0.1` on the current source buffer's target.  Dape starts a
short target-side Python byte bridge over its ordinary Remote stdio route;
it does not mistake the client's loopback for the target's listener or
require a separate SSH forward.  The CSE attach probe verified a stopped
breakpoint, `/fs:` stack path, target ownership of the bridge and debuggee,
continue, and normal output.  Dape owns and closes the bridge with the
session.

The local run used an isolated temporary Python environment with `debugpy`
1.8.12, and verified that `/fs:local:` selected the same interpreter as a
native buffer.  This proves Python launch on those targets.  Other languages and
debug-session recovery after a transport failure have not had equivalent
live tests.

## Remaining limits

- Warm `/fs:` queries still take roughly 1.9–2.8 times the direct `/rpc:` call in
  the Aaron-PC microbenchmark.  Every cached route still validates live
  configuration and health; this validation and logical-to-physical path
  translation retain a fixed cost.  Explicit `/fs:local:` metadata remains
  about 2.5–2.7 times a native path, while ordinary local buffers use native paths.
- No VS Code Remote SSH process was available for a controlled side-by-side
  measurement in the earlier runs.  On 2026-09-26 an isolated official VS Code
  1.139.1 client with Remote-SSH 0.128.0 authenticated to the same CSE host.
  Its default exec-server mode installed and started the remote server; the
  extension reported 13.43 s to resolve the cold authority, including 2.30 s
  server download and 3.12 s installation, but the client window never
  established a usable remote file session.  Legacy mode downloaded the
  server locally and stalled while transferring its archive.  Neither run
  produced an edit or completion measurement, so no VS Code speed ratio can
  be claimed.  Both isolated client instances were stopped and their new
  server revision was removed from the CSE account afterward.  The repeatable
  Emacs probes above remain the available baseline.
- Managed ripgrep currently supports Linux x86_64 with glibc and Linux
  aarch64.  Other targets use their existing rg or GNU grep; the fallback
  depends on grep accepting Consult's command-line options.  The x86_64
  deployment uses the official GNU/Linux `.deb` binary because the upstream
  15.2.0 x86_64 musl build has a reported
  [large-tree crash](https://github.com/BurntSushi/ripgrep/issues/3494).
- First search on a trusted target without rg uses target grep while a
  separate batch Emacs prepares the versioned rg cache.  Subsequent searches
  switch to rg after that worker succeeds; a failed worker keeps grep.
- The first use of a language whose LSP client is not already registered
  still loads the complete stock client set.  A newly created or removed
  `.envrc' outside Emacs may take up to one second to be rediscovered.
- Recursive inotify still uses one kernel watch per directory on Linux, even
  though Emacs owns only one process and one logical descriptor per root.
- A newline in a filename can be represented by the logical path helpers and
  watch stream, but upstream TRAMP/tramp-rpc file writes to such paths were
  not reliable in the live test.  The watch test creates that file through a
  target-side Python command and verifies its event separately.
- `remote-source` is an optional Node-based source API.  Aaron-PC lacks Node,
  so that API is not available there; ordinary file, process, LSP, and watch
  workflows passed their live tests.
- The Python smoke passed on the logical `local` target with the installed
  Pyright server, including diagnostics, symbols, completion, routing, and
  watch events.  CSE also passed real C, Python and Java LSP smokes, including
  JDTLS initialization and completion registration.  Aaron-PC has no Node or
  Python language server, so Python LSP on that SSH target remains unverified.
  A missing target Java runtime fails the external-client preflight before
  JDTLS command construction, and the real SSH negative check verifies a clear
  diagnostic without starting lsp-java.  External client packages load with
  the client machine's directory and environment.
