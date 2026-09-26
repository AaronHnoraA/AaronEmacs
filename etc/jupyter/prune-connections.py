"""Remove abandoned local TCP connection files; never terminate a kernel.

Runs on the filesystem's owning host, including through Remote. File age alone
is insufficient: all five listeners must refuse connections and no process may
reference the file. Unknown liveness, external addresses and symlinks stay put.
"""
import argparse
import errno
import ipaddress
import json
import os
from pathlib import Path
import socket
import stat
import subprocess
import time

PORTS = ("shell_port", "iopub_port", "stdin_port", "control_port", "hb_port")


def process_commands():
    try:
        return subprocess.check_output(
            ["ps", "-ax", "-o", "command="], text=True, timeout=5,
            stderr=subprocess.DEVNULL)
    except (OSError, subprocess.SubprocessError):
        return None


def refused(host, port):
    try:
        with socket.create_connection((host, port), timeout=0.2):
            return False
    except OSError as error:
        return error.errno == errno.ECONNREFUSED


def fingerprint(file):
    info = file.lstat()
    return info.st_dev, info.st_ino, info.st_mtime_ns, info.st_size


def prune(directory, *, apply=False, min_age=600):
    root = Path(directory)
    commands = process_commands()
    result = {"removed": [], "eligible": [], "kept": 0}
    if commands is None or not root.is_dir():
        return result
    for file in root.glob("*kernel-*.json"):
        try:
            attributes = file.lstat()
            if (not stat.S_ISREG(attributes.st_mode)
                    or time.time() - attributes.st_mtime < min_age
                    or attributes.st_uid != os.getuid() or file.name in commands):
                result["kept"] += 1
                continue
            before = fingerprint(file)
            info = json.loads(file.read_text())
            address = ipaddress.ip_address(info.get("ip", ""))
            ports = [info.get(key) for key in PORTS]
            if (info.get("transport") != "tcp"
                    or not (address.is_loopback or address.is_unspecified)
                    or not all(type(port) is int and 0 < port < 65536 for port in ports)):
                result["kept"] += 1
                continue
            host = ("::1" if address.version == 6 else "127.0.0.1") if address.is_unspecified else str(address)
            if not all(refused(host, port) for port in ports):
                result["kept"] += 1
                continue
            # Recheck process ownership, listeners and file identity immediately
            # before unlinking; a restarting owner may have rewritten the file.
            current = process_commands()
            if (current is None or file.name in current or fingerprint(file) != before
                    or not all(refused(host, port) for port in ports)):
                result["kept"] += 1
                continue
            result["eligible"].append(file.name)
            if apply:
                file.unlink()
                result["removed"].append(file.name)
        except (OSError, ValueError, TypeError):
            result["kept"] += 1
    return result


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--runtime-dir", required=True)
    parser.add_argument("--apply", action="store_true")
    options = parser.parse_args()
    print(json.dumps(prune(options.runtime_dir, apply=options.apply)))
