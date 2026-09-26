#!/usr/bin/env python3
"""Check the real Company child frame in an isolated graphical Emacs daemon.

REMOTE_GUI_COMPANY_FILE=/rpc:host:/path/a.py \
  python3 test/remote-gui-company-popup.py
The visited source is edited only in memory and never saved.
"""

import json
import os
import pathlib
import signal
import subprocess
import tempfile
import time
import uuid


ROOT = pathlib.Path(__file__).resolve().parents[1]
EMACS = os.environ.get("EMACS", "emacs")
EMACSCLIENT = os.environ.get("EMACSCLIENT", "emacsclient")


def client(server, expression, timeout=120):
    result = subprocess.run(
        [EMACSCLIENT, "-s", server, "-e", expression],
        cwd=ROOT, text=True, capture_output=True, timeout=timeout, check=False,
    )
    if result.returncode:
        raise RuntimeError(
            f"emacsclient failed ({result.returncode}): "
            + result.stdout[-2000:] + result.stderr[-2000:]
        )
    return result.stdout.strip()


def gui_result(server, file, rounds=0, stall=False, automatic=False, idle_delay=None):
    expression = (
        "(json-encode (my/remote-gui-company-popup-run "
        + json.dumps(str(file))
        + f" {rounds} {'t' if stall else 'nil'} {'t' if automatic else 'nil'}"
        + f" {idle_delay if idle_delay is not None else 'nil'}))"
    )
    # emacsclient prints the JSON value as an escaped Lisp string.  For this
    # ASCII probe payload, its string escaping is also valid JSON escaping.
    return json.loads(json.loads(client(server, expression)))


def main():
    remote_file = os.environ.get("REMOTE_GUI_COMPANY_FILE")
    rounds = max(0, int(os.environ.get("REMOTE_GUI_COMPANY_REQUEST_ROUNDS", "0")))
    stall = os.environ.get("REMOTE_GUI_COMPANY_STALL") == "1"
    automatic = os.environ.get("REMOTE_GUI_COMPANY_AUTOMATIC") == "1"
    idle_delay_raw = os.environ.get("REMOTE_GUI_COMPANY_IDLE_DELAY")
    idle_delay = float(idle_delay_raw) if idle_delay_raw else None
    if idle_delay is not None and not (0 < idle_delay <= 1):
        raise ValueError("REMOTE_GUI_COMPANY_IDLE_DELAY must be in (0, 1]")
    server = "remote-gui-company-" + uuid.uuid4().hex[:12]
    daemon_pid = None
    daemon_started = False
    with tempfile.TemporaryDirectory(prefix="emacs-gui-company-") as directory:
        local_root = pathlib.Path(directory)
        (local_root / ".git").mkdir()
        local_file = local_root / "source.py"
        local_file.write_text("name = 42\n", encoding="utf-8")
        try:
            subprocess.run(
                [EMACS, "-Q", "--daemon=" + server], cwd=ROOT,
                text=True, capture_output=True, timeout=20, check=True,
            )
            daemon_started = True
            daemon_pid = int(client(server, "(emacs-pid)", timeout=5))
            subprocess.run(
                [EMACSCLIENT, "-s", server, "-c", "-n"], cwd=ROOT,
                text=True, capture_output=True, timeout=15, check=True,
            )
            if client(server, "(display-graphic-p)", timeout=5) != "t":
                raise RuntimeError("Emacs did not select its graphical frame")
            init = (
                "(progn (setq user-emacs-directory " + json.dumps(str(ROOT) + "/")
                + " load-prefer-newer t)"
                + " (load-file " + json.dumps(str(ROOT / "early-init.el")) + ")"
                + " (load-file " + json.dumps(str(ROOT / "init.el")) + ")"
                + " (load-file "
                + json.dumps(str(ROOT / "test/remote-gui-company-popup.el"))
                + "))"
            )
            client(server, init, timeout=45)
            paths = [
                ("local", local_file),
                ("logical-local", "/fs:local:" + str(local_file)),
            ]
            order = os.environ.get("REMOTE_GUI_COMPANY_ORDER", "native-first")
            if order == "logical-first":
                paths.reverse()
            if remote_file:
                remote_entry = ("remote", remote_file)
                if order == "remote-first":
                    paths.insert(0, remote_entry)
                else:
                    paths.append(remote_entry)
            if order not in ("native-first", "logical-first", "remote-first"):
                raise ValueError("REMOTE_GUI_COMPANY_ORDER is invalid")
            for route, file in paths:
                result = gui_result(
                    server, file, rounds, automatic=automatic,
                    idle_delay=idle_delay,
                )
                result["route"] = route
                print(json.dumps(result, ensure_ascii=False), flush=True)
            if stall:
                if not remote_file:
                    raise ValueError("REMOTE_GUI_COMPANY_STALL needs REMOTE_GUI_COMPANY_FILE")
                result = gui_result(
                    server, remote_file, 0, True, automatic, idle_delay,
                )
                result["route"] = "remote-fault"
                print(json.dumps(result, ensure_ascii=False), flush=True)
        finally:
            if daemon_started:
                try:
                    client(server, "(kill-emacs 0)", timeout=3)
                except (RuntimeError, subprocess.TimeoutExpired):
                    pass
            if daemon_pid is not None:
                time.sleep(0.2)
                try:
                    os.kill(daemon_pid, 0)
                except ProcessLookupError:
                    pass
                else:
                    os.kill(daemon_pid, signal.SIGTERM)


if __name__ == "__main__":
    main()
