#!/usr/bin/env python3
"""Disposable HTTP server that survives the tested SSH forward's failure."""

import functools
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
import json
import os
from pathlib import Path
import re
import signal
import subprocess
import sys
import time


def process_record(pid):
    result = subprocess.run(["/bin/ps", "-p", str(pid), "-o", "uid=,stat=,command="],
                            capture_output=True, text=True)
    if result.returncode:
        return None
    fields = result.stdout.strip().split(None, 2)
    return fields if len(fields) == 3 else None


def serve(directory):
    handler = functools.partial(SimpleHTTPRequestHandler, directory=str(directory))
    server = ThreadingHTTPServer(("127.0.0.1", 0), handler)
    print(f"URL-SERVER:{server.server_address[1]}", flush=True)
    server.serve_forever()


def start(directory, owner=None):
    log_path = directory / "http-server.log"
    with log_path.open("wb") as output:
        command = [sys.executable, str(Path(__file__).resolve()), "serve", str(directory)]
        if owner:
            command.append(owner)
        child = subprocess.Popen(command,
                                 stdin=subprocess.DEVNULL, stdout=output, stderr=output,
                                 start_new_session=True)
    try:
        (directory / "http-server.pid").write_text(str(child.pid))
        deadline = time.monotonic() + 8
        while time.monotonic() < deadline:
            match = re.search(r"URL-SERVER:(\d+)", log_path.read_text())
            if match:
                print(json.dumps({"pid": child.pid, "port": int(match[1])}), flush=True)
                return
            if child.poll() is not None:
                raise RuntimeError("HTTP fixture exited before listening")
            time.sleep(0.02)
        raise TimeoutError("HTTP fixture did not listen")
    except BaseException:
        child.terminate()
        try:
            child.wait(timeout=2)
        except subprocess.TimeoutExpired:
            child.kill()
            child.wait(timeout=2)
        raise


def stop(directory):
    pid_path = directory / "http-server.pid"
    if not pid_path.exists():
        return
    pid = int(pid_path.read_text())
    if pid <= 0:
        raise ValueError("Invalid fixture PID")
    record = process_record(pid)
    if not record or record[1].startswith("Z"):
        return
    owner, _, command = record
    if int(owner) != os.getuid() or str(directory) not in command or str(Path(__file__).resolve()) not in command:
        raise RuntimeError("PID no longer belongs to this HTTP fixture; left it untouched")
    os.kill(pid, signal.SIGTERM)
    deadline = time.monotonic() + 5
    while time.monotonic() < deadline:
        record = process_record(pid)
        if not record or record[1].startswith("Z"):
            return
        time.sleep(0.02)
    raise TimeoutError("HTTP fixture did not stop")


if __name__ == "__main__":
    action, path = sys.argv[1:3]
    directory = Path(path).resolve()
    if action == "start":
        start(directory, sys.argv[3] if len(sys.argv) > 3 else None)
    else:
        {"serve": serve, "stop": stop}[action](directory)
