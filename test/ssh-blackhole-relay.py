#!/usr/bin/env python3
"""Loopback SSH relay for the opt-in Remote LSP blackhole smoke.

The marker affects only connections accepted by this process.  While its
timestamp is in the future, bytes in both directions are discarded without
closing either TCP socket, so OpenSSH's encrypted server-alive probes must
detect the stall.  The expiry lets recovery proceed even if Emacs is busy.
"""

import select
import socket
import sys
import threading
import time


def dropping(marker):
    try:
        with open(marker, encoding="ascii") as stream:
            expiry = stream.read().strip()
    except FileNotFoundError:
        return False
    try:
        return not expiry or time.time() < float(expiry)
    except ValueError:
        # A reader can race the marker's short write; keep dropping until
        # the complete deadline is visible.
        return True


def relay(client, host, port, marker):
    try:
        upstream = socket.create_connection((host, port), timeout=5)
    except OSError:
        client.close()
        return
    with client, upstream:
        while True:
            try:
                ready, _, _ = select.select((client, upstream), (), (), 0.2)
                for source in ready:
                    chunk = source.recv(65536)
                    if not chunk:
                        return
                    if not dropping(marker):
                        destination = upstream if source is client else client
                        destination.sendall(chunk)
            except OSError:
                return


def main():
    host, port_text, marker = sys.argv[1:]
    port = int(port_text)
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as listener:
        listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        listener.bind(("127.0.0.1", 0))
        listener.listen(16)
        print(listener.getsockname()[1], flush=True)
        while True:
            client, _ = listener.accept()
            threading.Thread(
                target=relay, args=(client, host, port, marker), daemon=True
            ).start()


if __name__ == "__main__":
    main()
