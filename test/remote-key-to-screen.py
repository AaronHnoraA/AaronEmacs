#!/usr/bin/env python3
"""Measure real PTY key delivery to Emacs terminal output with Python LSP.

REMOTE_KEY_SCREEN_FILE=/fs:host:/path/file.py python3 test/remote-key-to-screen.py
The remote source is visited and edited only in memory; it is never saved.
"""

import errno
import fcntl
import json
import math
import os
import pathlib
import pty
import select
import signal
import statistics
import struct
import subprocess
import sys
import tempfile
import termios
import time


ROOT = pathlib.Path(__file__).resolve().parents[1]
KEYS = "αβγδεζηθικλμνξοπρστυφχψω"
EMACS = os.environ.get("EMACS", "emacs")


def read_available(fd, timeout):
    if not select.select([fd], [], [], timeout)[0]:
        return b""
    try:
        return os.read(fd, 65536)
    except OSError as exc:
        if exc.errno == errno.EIO:
            return b""
        raise


def one_run(target):
    popup_scenario = os.environ.get("REMOTE_KEY_SCREEN_SCENARIO") == "python-completion"
    interkey_ms = max(0.0, float(os.environ.get("REMOTE_KEY_SCREEN_INTERKEY_MS", "0")))
    stall_completion = os.environ.get("REMOTE_KEY_SCREEN_STALL_COMPLETION") == "1"
    if stall_completion and not (interkey_ms or popup_scenario):
        raise ValueError("Completion fault injection needs REMOTE_KEY_SCREEN_INTERKEY_MS")
    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 70, 220, 0, 0))
    ready_fd, ready_path = tempfile.mkstemp(prefix="emacs-key-screen-")
    os.close(ready_fd)
    os.unlink(ready_path)
    env = os.environ.copy()
    env.update(
        TERM="xterm-256color",
        REMOTE_KEY_SCREEN_TARGET=target,
        REMOTE_KEY_SCREEN_READY=ready_path,
    )
    args = [
        EMACS, "-nw", "--no-site-file", "--no-site-lisp", "--no-splash",
        "--init-directory=" + str(ROOT), "-q", "-l", "early-init.el",
        "-l", "init.el", "-l", "test/remote-key-to-screen.el",
    ]
    proc = subprocess.Popen(
        args, cwd=ROOT, env=env, stdin=slave, stdout=slave, stderr=slave,
        start_new_session=True,
    )
    os.close(slave)
    tail = bytearray()
    try:
        deadline = time.monotonic() + 90
        while not os.path.exists(ready_path):
            chunk = read_available(master, 0.05)
            tail.extend(chunk)
            del tail[:-8000]
            if proc.poll() is not None:
                raise RuntimeError("Emacs exited before ready: " + repr(bytes(tail[-2000:])))
            if time.monotonic() > deadline:
                raise TimeoutError("Emacs startup timed out: " + repr(bytes(tail[-2000:])))
        status = pathlib.Path(ready_path).read_text().strip()
        if status != "READY":
            raise RuntimeError(status + "; terminal: " + repr(bytes(tail[-2000:])))

        # Drain startup redraws; each key below has a unique UTF-8 sequence.
        quiet_until = time.monotonic() + 2.0
        while time.monotonic() < quiet_until:
            if read_available(master, 0.02):
                quiet_until = time.monotonic() + 2.0

        latencies = []
        popup_ms = None
        popup_status = ""
        if popup_scenario:
            # Match the screenshot: type `prin`, then wait for Company's
            # automatic completion to contain the actual `print` candidate.
            for index, key in enumerate("prin", 1):
                started = time.monotonic()
                os.write(master, key.encode())
                deadline = started + 12
                while True:
                    read_available(master, 0.005)
                    status = pathlib.Path(ready_path).read_text().strip()
                    if status.startswith("POPUP-TIMEOUT") or status.startswith("ERROR"):
                        raise RuntimeError(status)
                    if index < 4 and status.startswith("KEY count="):
                        count = status.split("=", 1)[1]
                        if count.isdigit() and int(count) >= index:
                            break
                    if index == 4 and status.startswith("POPUP candidate=print"):
                        popup_ms = (time.monotonic() - started) * 1000
                        popup_status = status
                        break
                    if proc.poll() is not None:
                        raise RuntimeError("Emacs exited before Company returned `print`: " + status)
                    if time.monotonic() > deadline:
                        raise TimeoutError("Company candidate wait timed out: " + status)
                if index < 4:
                    time.sleep(0.05)
        else:
            for index, key in enumerate(KEYS):
                if index and interkey_ms:
                    idle_until = time.monotonic() + interkey_ms / 1000
                    while time.monotonic() < idle_until:
                        read_available(master, max(0, min(0.02, idle_until - time.monotonic())))
                encoded = key.encode()
                started = time.monotonic()
                os.write(master, encoded)
                observed = bytearray()
                deadline = started + 12
                while encoded not in observed:
                    observed.extend(read_available(master, 0.01))
                    if len(observed) > 200000:
                        del observed[:-1000]
                    if proc.poll() is not None:
                        raise RuntimeError("Emacs exited before displaying " + key)
                    if time.monotonic() > deadline:
                        raise TimeoutError("Key did not reach terminal: " + key + "; output " + repr(bytes(observed[-1000:])))
                latencies.append((time.monotonic() - started) * 1000)
        exit_deadline = time.monotonic() + 15
        exit_output = bytearray()
        while proc.poll() is None and time.monotonic() < exit_deadline:
            exit_output.extend(read_available(master, 0.05))
            del exit_output[:-4000]
        if proc.poll() is None:
            status = pathlib.Path(ready_path).read_text().strip()
            raise RuntimeError(
                "Emacs did not finish after all key events: " + status
                + "; terminal: " + repr(bytes(exit_output[-1500:]))
            )
        if proc.returncode != 0:
            raise RuntimeError("Emacs exited with status " + str(proc.returncode))
        completion_status = pathlib.Path(ready_path).read_text().strip()
        if not completion_status.startswith("DONE completion_requests="):
            raise RuntimeError("Probe did not count all key events: " + completion_status)
        fields = dict(field.split("=", 1) for field in completion_status.split()[1:])
        completion_requests = int(fields["completion_requests"])
        dropped_completions = int(fields["dropped"])
        if (interkey_ms or popup_scenario) and os.environ.get("REMOTE_KEY_SCREEN_LSP") != "0" and completion_requests == 0:
            raise RuntimeError("Company did not issue a completion request")
        if stall_completion and dropped_completions != 1:
            raise RuntimeError("Completion fault injection did not run")
        if popup_scenario:
            if fields.get("popup") != "1":
                raise RuntimeError("Company did not retain the `print` candidate")
            return {
                "target": target,
                "path_kind": (
                    "logical" if target == "remote" and env.get("REMOTE_KEY_SCREEN_FILE", "").startswith("/fs:")
                    else "physical" if target == "remote" else target
                ),
                "scenario": "python-completion",
                "candidate": "print",
                "candidate_ready_ms": round(popup_ms, 2),
                "tooltip_visible": fields.get("tooltip") == "1",
                "completion_requests": completion_requests,
                "dropped_completions": dropped_completions,
                "popup_status": popup_status,
            }
        values = sorted(latencies)
        return {
            "target": target,
            "path_kind": (
                "logical" if target == "remote" and env.get("REMOTE_KEY_SCREEN_FILE", "").startswith("/fs:")
                else "physical" if target == "remote" else target
            ),
            "keys": len(values),
            "median_ms": round(statistics.median(values), 2),
            "p95_ms": round(values[math.ceil(0.95 * len(values)) - 1], 2),
            "max_ms": round(values[-1], 2),
            "interkey_ms": interkey_ms,
            "completion_requests": completion_requests,
            "dropped_completions": dropped_completions,
            "all_ms": [round(x, 2) for x in latencies],
        }
    finally:
        if proc.poll() is None:
            os.killpg(proc.pid, signal.SIGTERM)
            try:
                proc.wait(timeout=3)
            except subprocess.TimeoutExpired:
                os.killpg(proc.pid, signal.SIGKILL)
                try:
                    proc.wait(timeout=3)
                except subprocess.TimeoutExpired:
                    pass
        os.close(master)
        try:
            os.unlink(ready_path)
        except FileNotFoundError:
            pass


def main():
    targets = sys.argv[1:] or ["local", "logical-local", "remote"]
    for target in targets:
        if target not in ("local", "logical-local", "remote"):
            raise SystemExit("Targets must be local, logical-local, or remote")
        if target == "remote" and not os.environ.get("REMOTE_KEY_SCREEN_FILE"):
            raise SystemExit("Set REMOTE_KEY_SCREEN_FILE to an existing /fs Python file")
        print(json.dumps(one_run(target), ensure_ascii=False), flush=True)


if __name__ == "__main__":
    main()
