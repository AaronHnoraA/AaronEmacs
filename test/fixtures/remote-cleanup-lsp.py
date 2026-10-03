"""Small stdio LSP peer for explicit-disconnect lifecycle tests."""

import json
import sys


def send(message):
    payload = json.dumps(message).encode("utf-8")
    sys.stdout.buffer.write(
        f"Content-Length: {len(payload)}\r\n\r\n".encode("ascii") + payload
    )
    sys.stdout.buffer.flush()


with open(sys.argv[1], "a", encoding="utf-8") as events:
    while True:
        headers = {}
        while True:
            line = sys.stdin.buffer.readline()
            if not line:
                sys.exit(0)
            if line in (b"\r\n", b"\n"):
                break
            key, value = line.decode("ascii").split(":", 1)
            headers[key.lower()] = value.strip()
        message = json.loads(
            sys.stdin.buffer.read(int(headers["content-length"]))
        )
        method = message.get("method", "")
        events.write(method + "\n")
        events.flush()
        if method == "exit":
            break
        if "id" in message:
            result = (
                {"capabilities": {"textDocumentSync": 1}}
                if method == "initialize"
                else None
            )
            send({"jsonrpc": "2.0", "id": message["id"], "result": result})
        if method == "initialized":
            print("cleanup LSP initialized", file=sys.stderr, flush=True)
