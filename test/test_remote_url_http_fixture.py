"""Verify ownership and cleanup of the detached URL integration fixture."""

import http.client
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch


fixture_path = Path(__file__).parent / "fixtures" / "remote-url-http-server.py"
spec = importlib.util.spec_from_file_location("url_http_fixture", fixture_path)
fixture = importlib.util.module_from_spec(spec)
spec.loader.exec_module(fixture)


class HTTPFixtureTests(unittest.TestCase):
    def test_serves_content_and_stops_only_its_child(self):
        with tempfile.TemporaryDirectory(prefix="url-http-fixture-") as path:
            directory = Path(path).resolve()
            (directory / "index.html").write_text("owned HTTP response")
            try:
                owner = str(directory / "disposable-ssh-owner")
                started = subprocess.run([sys.executable, str(fixture_path), "start", str(directory), owner],
                                         check=True, capture_output=True, text=True)
                server = json.loads(started.stdout)
                self.assertIn(owner, fixture.process_record(server["pid"])[2])
                connection = http.client.HTTPConnection("127.0.0.1", server["port"], timeout=3)
                try:
                    connection.request("GET", "/index.html")
                    response = connection.getresponse()
                    self.assertEqual(response.status, 200)
                    self.assertEqual(response.read(), b"owned HTTP response")
                finally:
                    connection.close()
            finally:
                fixture.stop(directory)
            record = fixture.process_record(server["pid"])
            self.assertTrue(record is None or record[1].startswith("Z"))

    def test_refuses_a_pid_that_has_been_reused(self):
        with tempfile.TemporaryDirectory(prefix="url-http-owner-") as path:
            directory = Path(path).resolve()
            (directory / "http-server.pid").write_text(str(os.getpid()))
            with patch.object(fixture.os, "kill") as kill:
                with self.assertRaisesRegex(RuntimeError, "no longer belongs"):
                    fixture.stop(directory)
                kill.assert_not_called()

    def test_failed_pid_file_write_reaps_the_started_child(self):
        children = []
        original = subprocess.Popen

        def spawn(*args, **kwargs):
            child = original(*args, **kwargs)
            children.append(child)
            return child

        with tempfile.TemporaryDirectory(prefix="url-http-failed-start-") as path:
            directory = Path(path).resolve()
            (directory / "http-server.pid").mkdir()
            try:
                with patch.object(fixture.subprocess, "Popen", side_effect=spawn):
                    with self.assertRaises(IsADirectoryError):
                        fixture.start(directory)
                self.assertEqual(len(children), 1)
                self.assertIsNotNone(children[0].poll())
            finally:
                for child in children:
                    if child.poll() is None:
                        child.kill()
                        child.wait(timeout=3)


if __name__ == "__main__":
    unittest.main()
