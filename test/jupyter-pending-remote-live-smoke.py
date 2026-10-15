"""Opt-in Linux Remote smoke test for cross-session Jupyter broker cleanup.

Run with JUPYTER_PENDING_LIVE_TARGET=<configured SSH target> via the Make target.
The middle session simulates an unavailable target; it does not cut the network.
"""

import os
import pathlib
import re
import shlex
import shutil
import subprocess
import tempfile
import uuid


TARGET = os.environ.get("JUPYTER_PENDING_LIVE_TARGET")
PYTHON = os.environ.get("JUPYTER_PENDING_LIVE_PYTHON", "/usr/bin/python3")
ROOT = pathlib.Path(__file__).resolve().parents[1]
ID = "noema-" + uuid.uuid4().hex[:24]
LOCAL = None

if not TARGET:
    raise SystemExit("Set JUPYTER_PENDING_LIVE_TARGET to a configured Linux SSH target")


def ssh(command, timeout=12):
    return subprocess.run(
        ["ssh", "-o", "BatchMode=yes", "-o", "ConnectTimeout=4",
         "-o", "ConnectionAttempts=1", TARGET, command],
        capture_output=True, text=True, timeout=timeout,
    )


def emacs(form, expected):
    env = os.environ.copy()
    env.update(NOEMA_PROBE_REMOTE_DIR=REMOTE, NOEMA_PROBE_LOCAL_DIR=str(LOCAL),
               NOEMA_PROBE_ID=ID, NOEMA_PROBE_PYTHON=PYTHON,
               JUPYTER_PENDING_LIVE_TARGET=TARGET)
    try:
        result = subprocess.run(
            ["emacs", "--batch", "--no-site-file", "--no-site-lisp",
             "--no-splash", f"--init-directory={ROOT}",
             "-q", "-l", "./early-init.el", "-l", "./init.el", "--eval", form],
            cwd=ROOT, env=env,
            text=True, capture_output=True, timeout=90,
        )
    except subprocess.TimeoutExpired as error:
        stdout = (error.stdout or b"").decode(errors="replace") if isinstance(error.stdout, bytes) else (error.stdout or "")
        stderr = (error.stderr or b"").decode(errors="replace") if isinstance(error.stderr, bytes) else (error.stderr or "")
        raise RuntimeError(f"Emacs timed out; stdout={stdout[-900:]} stderr={stderr[-1800:]}") from error
    if result.returncode or expected not in result.stdout:
        raise RuntimeError(
            f"Emacs phase failed ({result.returncode}), expected {expected}\n"
            f"stdout: {result.stdout[-1200:]}\nstderr: {result.stderr[-2400:]}"
        )
    return result.stdout


phase1 = r'''(progn
 (remote-config-load)
 (remote-fs-install)
 (let* ((root (getenv "NOEMA_PROBE_REMOTE_DIR"))
        (id (getenv "NOEMA_PROBE_ID"))
        (my/noema-jupyter-pending-directory
         (expand-file-name "pending/" (getenv "NOEMA_PROBE_LOCAL_DIR")))
        (placement (remote-make-file-name (getenv "JUPYTER_PENDING_LIVE_TARGET") (expand-file-name "note.ipynb" root)))
        (connection (remote-make-file-name (getenv "JUPYTER_PENDING_LIVE_TARGET") (expand-file-name (concat id ".json") root)))
        (log (remote-make-file-name (getenv "JUPYTER_PENDING_LIVE_TARGET") (expand-file-name (concat id ".log") root)))
        (context (remote-context placement)))
   (with-file-modes #o600 (write-region "{}\n" nil connection nil 'silent))
   (princ "REMOTE_FILE_WRITTEN\n")
   (let* ((python (getenv "NOEMA_PROBE_PYTHON"))
          (argv (vector python "-c" "import time; time.sleep(120)" (remote-file-local-name connection)))
          (pid (string-to-number
                (my/noema-jupyter--output context python "-c"
                 my/noema-jupyter--launch-helper
                 (json-serialize argv) "{}" root (remote-file-local-name log))))
          (runtime (my/noema-jupyter-runtime-create
                    :id id :context context :placement-file placement :pid pid
                    :connection-file connection :log-file log :state 'shutdown-pending)))
     (unless (> pid 0) (error "invalid target pid"))
     (my/noema-jupyter--record-pending-shutdown runtime)
     (princ (format "REMOTE_PID=%s\n" pid)))))'''

phase2 = r'''(progn
 (remote-config-load)
 (let* ((my/noema-jupyter-pending-directory
          (expand-file-name "pending/" (getenv "NOEMA_PROBE_LOCAL_DIR")))
        (id (getenv "NOEMA_PROBE_ID"))
        (entry (my/noema-jupyter--pending-shutdown-for id)))
   (unless entry (error "missing journal"))
   (let ((failed nil))
     (cl-letf (((symbol-function 'my/noema-jupyter--alive-p)
                (lambda (_runtime) 'unknown)))
       (condition-case nil
           (my/noema-jupyter--retry-pending-shutdown entry)
         (error (setq failed t))))
     (unless failed (error "offline retry unexpectedly succeeded")))
   (unless (file-exists-p (my/noema-jupyter--pending-file id))
     (error "offline attempt lost journal"))
   (princ "OFFLINE_RETAINED\n")))'''

phase3 = r'''(progn
 (remote-config-load)
 (let* ((my/noema-jupyter-pending-directory
          (expand-file-name "pending/" (getenv "NOEMA_PROBE_LOCAL_DIR")))
        (id (getenv "NOEMA_PROBE_ID"))
        (entry (my/noema-jupyter--pending-shutdown-for id)))
   (unless entry (error "missing journal"))
   (my/noema-jupyter--retry-pending-shutdown entry)
   (when (file-exists-p (my/noema-jupyter--pending-file id))
     (error "journal remains"))
   (princ "REMOTE_RECOVERED\n")))'''


checked = ssh("uname -s")
if checked.returncode or checked.stdout.strip() != "Linux":
    raise SystemExit("Target must be a reachable Linux SSH host: " + checked.stderr[-500:])

created = ssh("mktemp -d /tmp/noema-pending-probe.XXXXXXXX")
if created.returncode:
    raise SystemExit(created.stderr[-1000:])
REMOTE = created.stdout.strip()
if not re.fullmatch(r"/tmp/noema-pending-probe\.[A-Za-z0-9]+", REMOTE):
    raise RuntimeError(f"unexpected remote directory: {REMOTE!r}")
LOCAL = pathlib.Path(tempfile.mkdtemp(prefix="noema-pending-client-"))

try:
    print("REMOTE_TARGET_READY", flush=True)
    output = emacs(phase1, "REMOTE_PID=")
    pid = int(output.split("REMOTE_PID=")[1].splitlines()[0])
    print("REMOTE_PROCESS_LAUNCHED", flush=True)
    emacs(phase2, "OFFLINE_RETAINED")
    print("PENDING_RECORD_RETAINED", flush=True)
    emacs(phase3, "REMOTE_RECOVERED")
    token = str(pathlib.PurePosixPath(REMOTE) / (ID + ".json"))
    verify_code = (
        "import glob,os,sys\n"
        "token=sys.argv[1]\n"
        "assert not os.path.exists(token), 'connection file remains'\n"
        "for file in glob.glob('/proc/[0-9]*/cmdline'):\n"
        " try:\n"
        "  args=open(file,'rb').read().split(b'\\0')\n"
        "  assert not (len(args)>3 and args[1]==b'-c' and "
        "args[2]==b'import time; time.sleep(120)' and token.encode() in args), 'process remains'\n"
        " except (OSError,IndexError): pass\n"
    )
    verified = ssh("python3 -c " + shlex.quote(verify_code) + " " + shlex.quote(token))
    if verified.returncode:
        raise RuntimeError("Remote cleanup did not finish: " + verified.stderr[-900:])
    print("REMOTE_CROSS_SESSION_RECOVERY_PASS", flush=True)
finally:
    # The marker is unique to this temporary directory.  A failed probe must
    # still terminate only the process it launched before removing its files.
    token = str(pathlib.PurePosixPath(REMOTE) / (ID + ".json"))
    cleanup_code = (
        "import glob,os,signal,sys,shutil\n"
        "token,root=sys.argv[1:]\n"
        "for file in glob.glob('/proc/[0-9]*/cmdline'):\n"
        " try:\n"
        "  args=open(file,'rb').read().split(b'\\0')\n"
        "  if len(args)>3 and args[1]==b'-c' and "
        "args[2]==b'import time; time.sleep(120)' and token.encode() in args:\n"
        "   os.killpg(int(file.split('/')[2]),signal.SIGTERM)\n"
        " except (OSError,ValueError,IndexError): pass\n"
        "shutil.rmtree(root)\n"
    )
    cleanup = (
        "python3 -c " + shlex.quote(cleanup_code) + " "
        + shlex.quote(token) + " " + shlex.quote(REMOTE)
    )
    result = ssh(cleanup, timeout=15)
    shutil.rmtree(LOCAL, ignore_errors=True)
    if result.returncode:
        raise RuntimeError("REMOTE_CLEANUP_FAILED: " + result.stderr[-900:])
