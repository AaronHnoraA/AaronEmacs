"""Opt-in broker recovery probe after a real SIGKILL of the client Emacs.

JUPYTER_PENDING_LIVE_TARGET=<configured Linux SSH target> \
    make jupyter-abrupt-exit-live-smoke
The remote process is a sleeping Python probe, not an ipykernel.
"""

import os
import pathlib
import re
import shlex
import shutil
import signal
import subprocess
import tempfile
import time


ROOT = pathlib.Path(__file__).resolve().parents[1]
TARGET = os.environ.get("JUPYTER_PENDING_LIVE_TARGET")
PYTHON = os.environ.get("JUPYTER_PENDING_LIVE_PYTHON", "/usr/bin/python3")
if not TARGET:
    raise SystemExit("Set JUPYTER_PENDING_LIVE_TARGET to a configured Linux SSH target")


def ssh(command, timeout=12):
    return subprocess.run(
        ["ssh", "-o", "BatchMode=yes", "-o", "ConnectTimeout=4",
         "-o", "ConnectionAttempts=1", TARGET, command],
        capture_output=True, text=True, timeout=timeout,
    )


def emacs_args(form):
    return ["emacs", "--batch", "--no-site-file", "--no-site-lisp",
            "--no-splash", f"--init-directory={ROOT}", "-q",
            "-l", "./early-init.el", "-l", "./init.el", "--eval", form]


phase1 = r'''(progn
 (remote-config-load)
 (remote-fs-install)
 (let* ((root (getenv "NOEMA_PROBE_REMOTE_DIR"))
        (target (getenv "JUPYTER_PENDING_LIVE_TARGET"))
        (python (getenv "NOEMA_PROBE_PYTHON"))
        (placement (remote-make-file-name target (expand-file-name "note.ipynb" root)))
        (workspace (remote-workspace-create
                    :root (remote-make-file-name target root)))
        (my/noema-jupyter-pending-directory
         (expand-file-name "pending/" (getenv "NOEMA_PROBE_LOCAL_DIR")))
        (my/noema-jupyter-runtimes (make-hash-table :test 'equal)))
   (cl-letf (((symbol-function 'my/noema-jupyter--context)
              (lambda (_file) (remote-context placement)))
             ((symbol-function 'remote-workspace-open)
              (lambda (&rest _) workspace))
             ((symbol-function 'my/noema-jupyter--kernelspecs)
              (lambda (_file)
                `(((name . "probe")
                   (spec . ((argv . ,(vector python "-c"
                                             "import time; time.sleep(120)"
                                             "{connection_file}"))))))))
             ((symbol-function 'my/noema-jupyter--runtime-directory)
              (lambda (_workspace) (remote-make-file-name target root)))
             ((symbol-function 'my/noema-jupyter--json-output)
              (lambda (&rest _) '(41001 41002 41003 41004 41005)))
             ((symbol-function 'my/noema-jupyter--signing-key)
              (lambda () "probe-only-key"))
             ((symbol-function 'remote-channel-group-open)
              (lambda (&rest _) 'probe-group))
             ((symbol-function 'my/noema-jupyter--client-connection)
              (lambda (connection _group) connection))
             ((symbol-function 'remote-workspace-ensure-recoverable-resource)
              (lambda (&rest _) t)))
     (let ((runtime (my/noema-jupyter--launch-runtime
                     `((kernelId . "abrupt-exit-probe")
                       (sourceFile . ,placement)
                       (kernelName . "probe")))))
       (with-temp-file (expand-file-name "launched" (getenv "NOEMA_PROBE_LOCAL_DIR"))
         (insert (format "%s %s\n" (my/noema-jupyter-runtime-id runtime)
                         (my/noema-jupyter-runtime-pid runtime))))
       (sleep-for 120)))))'''

phase2 = r'''(progn
 (remote-config-load)
 (let* ((my/noema-jupyter-pending-directory
         (expand-file-name "pending/" (getenv "NOEMA_PROBE_LOCAL_DIR")))
        (id (getenv "NOEMA_PROBE_ID"))
        (entry (my/noema-jupyter--pending-shutdown-for id)))
   (unless entry (error "abrupt exit left no broker record"))
   (unless (plist-get (car (my/noema-jupyter-runtime-snapshot
                           (getenv "JUPYTER_PENDING_LIVE_TARGET")))
                      :journal-pending)
     (error "Board snapshot did not recover broker"))
   (my/noema-jupyter--retry-pending-shutdown entry)
   (when (file-exists-p (my/noema-jupyter--pending-file id))
     (error "broker record remains after recovery"))
   (princ "ABRUPT_EXIT_RECOVERED\n")))'''


checked = ssh("uname -s")
if checked.returncode or checked.stdout.strip() != "Linux":
    raise SystemExit("Target must be a reachable Linux SSH host: " + checked.stderr[-500:])
created = ssh("mktemp -d /tmp/noema-abrupt-probe.XXXXXXXX")
if created.returncode:
    raise SystemExit(created.stderr[-500:])
remote = created.stdout.strip()
if not re.fullmatch(r"/tmp/noema-abrupt-probe\.[A-Za-z0-9]+", remote):
    raise RuntimeError(f"unexpected remote directory: {remote!r}")
local = pathlib.Path(tempfile.mkdtemp(prefix="noema-abrupt-client-"))
env = os.environ.copy()
env.update(NOEMA_PROBE_REMOTE_DIR=remote, NOEMA_PROBE_LOCAL_DIR=str(local),
           NOEMA_PROBE_PYTHON=PYTHON, JUPYTER_PENDING_LIVE_TARGET=TARGET)
client = None
try:
    with (local / "phase1.log").open("w") as log:
        client = subprocess.Popen(emacs_args(phase1), cwd=ROOT, env=env,
                                  stdout=log, stderr=subprocess.STDOUT)
        deadline = time.monotonic() + 80
        marker = local / "launched"
        while not marker.exists() and time.monotonic() < deadline:
            if client.poll() is not None:
                break
            time.sleep(0.1)
        if not marker.exists():
            raise RuntimeError("Broker did not launch: " + (local / "phase1.log").read_text()[-2400:])
        runtime_id, pid = marker.read_text().split()
        if not re.fullmatch(r"noema-[0-9a-f]{24}", runtime_id) or not pid.isdigit():
            raise RuntimeError("Invalid broker launch marker")
        print("BROKER_LAUNCHED", flush=True)
        client.send_signal(signal.SIGKILL)
        client.wait(timeout=5)
        if client.returncode != -signal.SIGKILL:
            raise RuntimeError(f"Expected SIGKILL, got {client.returncode}")
        print("CLIENT_SIGKILL_CONFIRMED", flush=True)
    token = f"{remote}/{runtime_id}.json"
    alive_code = (
        "import os,sys\n"
        "pid,token=sys.argv[1:]\n"
        "args=open('/proc/'+pid+'/cmdline','rb').read().split(b'\\0')\n"
        "if not (len(args)>3 and args[1]==b'-c' and "
        "args[2]==b'import time; time.sleep(120)' and token.encode() in args):\n"
        " raise SystemExit('detached broker did not survive client SIGKILL')\n"
        "if not os.path.exists(token): raise SystemExit('connection file missing')\n"
    )
    alive = ssh("python3 -c " + shlex.quote(alive_code) + " "
                + shlex.quote(pid) + " " + shlex.quote(token))
    if alive.returncode:
        raise RuntimeError("Remote broker did not survive client SIGKILL: "
                           + alive.stderr[-900:])
    print("DETACHED_BROKER_STILL_RUNNING", flush=True)
    env["NOEMA_PROBE_ID"] = runtime_id
    result = subprocess.run(emacs_args(phase2), cwd=ROOT, env=env,
                            capture_output=True, text=True, timeout=90)
    if result.returncode or "ABRUPT_EXIT_RECOVERED" not in result.stdout:
        diagnose = ssh("ps -p " + shlex.quote(pid) + " -o pid,pgid,stat,args=; ls -la "
                       + shlex.quote(remote))
        raise RuntimeError("New Emacs session failed: "
                           + result.stdout[-1600:] + result.stderr[-1600:]
                           + "\nSSH: " + diagnose.stdout[-1000:] + diagnose.stderr[-500:])
    verify_code = (
        "import os,sys,time\n"
        "pid,token=sys.argv[1:]\n"
        "for _ in range(30):\n"
        " try: args=open('/proc/'+pid+'/cmdline','rb').read().split(b'\\0')\n"
        " except FileNotFoundError: args=[]\n"
        " if not (len(args)>3 and args[1]==b'-c' and "
        "args[2]==b'import time; time.sleep(120)' and token.encode() in args): break\n"
        " time.sleep(0.1)\n"
        "else: raise SystemExit('broker process remains after TERM')\n"
        "if os.path.exists(token): raise SystemExit('connection file remains')\n"
    )
    verified = ssh("python3 -c " + shlex.quote(verify_code) + " "
                   + shlex.quote(pid) + " " + shlex.quote(token), timeout=15)
    if verified.returncode:
        raise RuntimeError("Remote broker cleanup unconfirmed: " + verified.stderr[-900:])
    print("ABRUPT_EXIT_REMOTE_RECOVERY_PASS", flush=True)
finally:
    if client and client.poll() is None:
        client.kill()
        client.wait(timeout=5)
    cleanup_code = (
        "import glob,os,signal,shutil,sys\n"
        "root=sys.argv[1]\n"
        "for file in glob.glob('/proc/[0-9]*/cmdline'):\n"
        " try:\n"
        "  args=open(file,'rb').read().split(b'\\0')\n"
        "  if len(args)>3 and args[1]==b'-c' and "
        "args[2]==b'import time; time.sleep(120)' and "
        "any(a.startswith((root+'/noema-').encode()) and a.endswith(b'.json') for a in args):\n"
        "   os.killpg(int(file.split('/')[2]),signal.SIGTERM)\n"
        " except (OSError,ValueError,IndexError): pass\n"
        "shutil.rmtree(root,ignore_errors=True)\n"
    )
    cleanup = ssh("python3 -c " + shlex.quote(cleanup_code) + " " + shlex.quote(remote),
                  timeout=15)
    shutil.rmtree(local, ignore_errors=True)
    if cleanup.returncode:
        raise RuntimeError("REMOTE_CLEANUP_FAILED: " + cleanup.stderr[-900:])
