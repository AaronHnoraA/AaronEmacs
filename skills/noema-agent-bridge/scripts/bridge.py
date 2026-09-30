#!/usr/bin/env python3
"""Send a small, explicit request to this Mac's live Noema ACP sessions."""

import argparse
import base64
import json
import subprocess
import sys


def request(action, session_id=None, agent=None, workspace=None, message=None,
            request_id=None, socket=None):
    payload = {"action": action}
    if action != "list":
        payload.update(sessionId=session_id, agent=agent, workspace=workspace)
    if action == "send":
        payload["message"] = message
    if action == "read":
        payload["requestId"] = request_id
    encoded = base64.b64encode(json.dumps(payload, ensure_ascii=False).encode()).decode()
    command = ["emacsclient"]
    if socket:
        command += ["--socket-name", socket]
    command += ["--eval", f'(my/noema-agent-bridge-request "{encoded}")']
    result = subprocess.run(command, capture_output=True, text=True, timeout=15, check=False)
    if result.returncode:
        raise RuntimeError(result.stderr.strip() or "emacsclient failed")
    return json.loads(json.loads(result.stdout.strip()))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--socket", help="Emacs local socket name/path (default: emacsclient default)")
    commands = parser.add_subparsers(dest="action", required=True)
    commands.add_parser("list", help="List live Noema/agent-shell ACP sessions")
    for action in ("send", "read", "interrupt"):
        item = commands.add_parser(action)
        item.add_argument("--session-id", required=True)
        item.add_argument("--agent", required=True)
        item.add_argument("--workspace", required=True)
        if action == "send":
            item.add_argument("--message", help="Text to send (defaults to stdin)")
        if action == "read":
            item.add_argument("--request-id", required=True)
    args = parser.parse_args()
    message = getattr(args, "message", None)
    if args.action == "send" and message is None:
        if sys.stdin.isatty():
            parser.error("send needs --message or text on stdin")
        message = sys.stdin.read()
    try:
        response = request(
            args.action,
            session_id=getattr(args, "session_id", None),
            agent=getattr(args, "agent", None),
            workspace=getattr(args, "workspace", None),
            message=message,
            request_id=getattr(args, "request_id", None),
            socket=args.socket,
        )
    except (OSError, subprocess.TimeoutExpired, ValueError, RuntimeError) as error:
        response = {"ok": False, "error": str(error)}
    print(json.dumps(response, ensure_ascii=False, indent=2))
    return 0 if response.get("ok") else 1


if __name__ == "__main__":
    raise SystemExit(main())
