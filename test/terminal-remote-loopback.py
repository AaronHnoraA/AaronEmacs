#!/usr/bin/env python3
"""Run real SSH terminal/optional ACP checks against a disposable local sshd.

Creates its own host/client keys and config; uses no existing private keys or
authorized_keys.  GUI tests use Portal's runner through the Makefile target.
"""

import getpass
import os
from pathlib import Path
import re
import shutil
import signal
import shlex
import socket
import subprocess
import tempfile
import time


def stop_owned_helpers(root):
    """Stop client masters/RPC helpers still naming this unique fixture root."""
    listing = subprocess.check_output(["/bin/ps", "-u", str(os.getuid()), "-o", "pid=,command="], text=True)
    for line in listing.splitlines():
        fields = line.strip().split(None, 1)
        if len(fields) != 2 or not fields[0].isdigit() or str(root) not in fields[1]:
            continue
        pid = int(fields[0])
        if pid != os.getpid():
            try:
                os.kill(pid, signal.SIGTERM)
            except ProcessLookupError:
                pass


def main():
    repository = Path(__file__).resolve().parents[1]
    with tempfile.TemporaryDirectory(prefix="terminal-ssh-") as directory:
        root = Path(directory)
        for name in ("host", "client"):
            subprocess.run(["/usr/bin/ssh-keygen", "-q", "-t", "ed25519", "-N", "", "-f", str(root / name)], check=True)
        with socket.socket() as listener:
            listener.bind(("127.0.0.1", 0))
            port = listener.getsockname()[1]
        server_config = root / "sshd_config"
        server_config.write_text(f"""Port {port}
ListenAddress 127.0.0.1
HostKey {root / 'host'}
PidFile {root / 'sshd.pid'}
AuthorizedKeysFile {root / 'client.pub'}
StrictModes no
PasswordAuthentication no
KbdInteractiveAuthentication no
UsePAM no
UseDNS no
AllowUsers {getpass.getuser()}
LogLevel ERROR
""")
        key = (root / "host.pub").read_text().split()
        (root / "known_hosts").write_text(f"[127.0.0.1]:{port} {key[0]} {key[1]}\n")
        client_config = root / "ssh_config"
        client_config.write_text(f"""Host terminal-loopback
  HostName 127.0.0.1
  Port {port}
  User {getpass.getuser()}
  IdentityFile {root / 'client'}
  IdentitiesOnly yes
  UserKnownHostsFile {root / 'known_hosts'}
  StrictHostKeyChecking yes
  BatchMode yes
""")
        client_bin = root / "bin"
        client_bin.mkdir()
        ssh = client_bin / "ssh"
        ssh.write_text("#!/bin/sh\nexec /usr/bin/ssh -F " + shlex.quote(str(client_config)) + ' "$@"\n')
        ssh.chmod(0o700)
        with open(root / "server.log", "w+") as log:
            server = subprocess.Popen(["/usr/sbin/sshd", "-D", "-e", "-f", str(server_config)], stdout=log, stderr=log)
            try:
                time.sleep(0.3)
                probe = subprocess.run(["/usr/bin/ssh", "-F", str(client_config), "terminal-loopback", "true"],
                                       capture_output=True, text=True, timeout=10)
                if probe.returncode:
                    raise RuntimeError(f"Disposable SSH fixture failed: {probe.stderr.strip()}")
                environment = dict(os.environ,
                                   TERMINAL_REMOTE_TARGET="terminal-loopback",
                                   TERMINAL_REMOTE_SSH_CONFIG=str(client_config),
                                   TERMINAL_REMOTE_SSH_PROGRAM=str(ssh),
                                   TERMINAL_REMOTE_RPC_DIRECTORY=str(root / "rpc"),
                                   TERMINAL_REMOTE_TMP_ROOT=os.path.realpath("/tmp"),
                                   PATH=str(client_bin) + os.pathsep + os.environ.get("PATH", ""))
                gui_logs = root / "gui-logs"
                gui_logs.mkdir()
                environment["PORTAL_TEST_LOGS"] = str(gui_logs)
                # Use exactly the installed RPC release.  This fixture tests
                # terminal placement and transport, separately from deploy.
                source = (repository / "elpa/tramp-rpc/lisp/tramp-rpc-deploy.el").read_text()
                version = re.search(r'\(defconst tramp-rpc-deploy-version "([^"]+)"', source).group(1)
                binary = Path(os.environ.get("TERMINAL_REMOTE_RPC_BINARY",
                                             repository / f"tramp-rpc/{version}/aarch64-darwin/tramp-rpc-server"))
                if not binary.is_file():
                    raise RuntimeError("The pinned aarch64-darwin RPC server is not cached; set TERMINAL_REMOTE_RPC_BINARY")
                deployed = root / "rpc" / "tramp-rpc-server"
                deployed.parent.mkdir()
                shutil.copy2(binary, deployed)
                deployed.chmod(0o700)
                environment["TERMINAL_REMOTE_RPC_BINARY"] = str(deployed)
                backends = os.environ.get("TERMINAL_REMOTE_LOOPBACK_BACKENDS", "tramp,tramp-rpc").split(",")
                targets = [(backend, "terminal-remote-live") for backend in backends]
                if os.environ.get("TERMINAL_REMOTE_GUI") == "1":
                    targets.append(("tramp-rpc", "terminal-remote-gui-live"))
                if os.environ.get("TERMINAL_REMOTE_AGENT") == "1":
                    targets.extend((backend, "agent-remote-live") for backend in backends)
                failures = []
                for backend, target in targets:
                    environment["TERMINAL_REMOTE_BACKEND"] = backend
                    environment["AGENT_REMOTE_TARGET"] = "terminal-loopback"
                    environment["AGENT_REMOTE_BACKEND"] = backend
                    print(f"Disposable SSH: backend={backend} make {target}", flush=True)
                    process = subprocess.Popen(["make", target], cwd=repository, env=environment, start_new_session=True)
                    try:
                        status = process.wait(timeout=75)
                        if status:
                            failures.append(f"{backend}/{target}: exit {status}")
                    except subprocess.TimeoutExpired:
                        failures.append(f"{backend}/{target}: timeout")
                    finally:
                        if process.poll() is None:
                            os.killpg(process.pid, signal.SIGTERM)
                            try:
                                process.wait(timeout=5)
                            except subprocess.TimeoutExpired:
                                os.killpg(process.pid, signal.SIGKILL)
                                process.wait(timeout=5)
                if failures:
                    raise RuntimeError("; ".join(failures))
            finally:
                stop_owned_helpers(root)
                server.terminate()
                server.wait(timeout=5)


if __name__ == "__main__":
    main()
