"""Checks for the Codex Remote to local Emacs request boundary."""

import base64
import importlib.util
import json
from pathlib import Path
import subprocess
import unittest
from unittest.mock import patch


SCRIPT = Path(__file__).resolve().parents[1] / ".agents/skills/noema-agent-bridge/scripts/bridge.py"
SPEC = importlib.util.spec_from_file_location("noema_agent_bridge_cli", SCRIPT)
BRIDGE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(BRIDGE)


class BridgeCliTests(unittest.TestCase):
    def test_unicode_and_lisp_syntax_stay_inside_encoded_data(self):
        message = '手机消息 ") (delete-process x)'
        response = {"ok": True, "state": "queued"}
        completed = subprocess.CompletedProcess(
            [], 0, stdout=json.dumps(json.dumps(response)), stderr=""
        )
        with patch.object(BRIDGE.subprocess, "run", return_value=completed) as run:
            self.assertEqual(
                BRIDGE.request("send", "task", "codex", "/fs:local:/work", message),
                response,
            )
        command = run.call_args.args[0]
        expression = command[-1]
        self.assertEqual(expression.count('"'), 2)
        encoded = expression.split('"')[1]
        payload = json.loads(base64.b64decode(encoded))
        self.assertEqual(payload["message"], message)
        self.assertEqual(payload["workspace"], "/fs:local:/work")

    def test_emacsclient_failure_is_reported(self):
        completed = subprocess.CompletedProcess([], 1, stdout="", stderr="no socket")
        with patch.object(BRIDGE.subprocess, "run", return_value=completed):
            with self.assertRaisesRegex(RuntimeError, "no socket"):
                BRIDGE.request("list")

    def test_read_uses_the_exact_bridge_request_id(self):
        completed = subprocess.CompletedProcess(
            [], 0,
            stdout=json.dumps(json.dumps({"ok": True, "state": "completed", "text": "done"})),
            stderr="",
        )
        with patch.object(BRIDGE.subprocess, "run", return_value=completed) as run:
            response = BRIDGE.request(
                "read", "task", "codex", "/fs:local:/work", request_id="bridge-7"
            )
        encoded = run.call_args.args[0][-1].split('"')[1]
        self.assertEqual(json.loads(base64.b64decode(encoded))["requestId"], "bridge-7")
        self.assertEqual(response["text"], "done")

    def test_generic_api_body_is_data_and_preserves_notification_identity(self):
        body = {"action": "spawn", "prompt": '读公式 $(ignored) "',
                "notify": {"buffer": "*Pi*", "nativeSessionId": "parent"}}
        completed = subprocess.CompletedProcess(
            [], 0, stdout=json.dumps(json.dumps({"ok": True, "requestId": "agent-1"})), stderr=""
        )
        with patch.object(BRIDGE.subprocess, "run", return_value=completed) as run:
            result = BRIDGE.request("api", body=body)
        encoded = run.call_args.args[0][-1].split('"')[1]
        self.assertEqual(json.loads(base64.b64decode(encoded)), {"action": "api", "body": body})
        self.assertEqual(result["requestId"], "agent-1")


if __name__ == "__main__":
    unittest.main()
