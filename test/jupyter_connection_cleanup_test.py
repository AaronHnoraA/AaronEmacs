import importlib.util
import json
import os
from pathlib import Path
import tempfile
import time
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location(
    "prune_connections", Path(__file__).resolve().parents[1] / "etc/jupyter/prune-connections.py")
cleanup = importlib.util.module_from_spec(spec)
spec.loader.exec_module(cleanup)


class ConnectionCleanup(unittest.TestCase):
    def test_removes_only_unowned_refused_old_local_files(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for name in ("dead", "live", "owned", "external", "recent", "unknown"):
                info = dict(transport="tcp", ip="10.0.0.1" if name == "external" else "127.0.0.1")
                info.update({key: 41000 + index for index, key in enumerate(cleanup.PORTS)})
                if name in ("live", "unknown"):
                    info["hb_port"] = 42000
                file = root / f"kernel-{name}.json"
                file.write_text(json.dumps(info))
                if name != "recent":
                    os.utime(file, (time.time() - 3600,) * 2)
            (root / "kernel-link.json").symlink_to(root / "kernel-dead.json")
            with patch.object(cleanup, "process_commands", return_value="python -f kernel-owned.json"), \
                    patch.object(cleanup, "refused", side_effect=lambda host, port: port != 42000):
                result = cleanup.prune(root, apply=True)
            self.assertEqual(result["removed"], ["kernel-dead.json"])
            self.assertTrue((root / "kernel-live.json").exists())
            self.assertTrue((root / "kernel-link.json").is_symlink())

    def test_preserves_file_if_owner_appears_during_probe(self):
        with tempfile.TemporaryDirectory() as directory:
            file = Path(directory) / "kernel-restarting.json"
            file.write_text(json.dumps(dict(transport="tcp", ip="127.0.0.1",
                                           **{key: 41000 + index for index, key in enumerate(cleanup.PORTS)})))
            os.utime(file, (time.time() - 3600,) * 2)
            with patch.object(cleanup, "process_commands", side_effect=["", "python -f kernel-restarting.json"]), \
                    patch.object(cleanup, "refused", return_value=True):
                self.assertEqual(cleanup.prune(directory, apply=True)["removed"], [])
            self.assertTrue(file.exists())


if __name__ == "__main__":
    unittest.main()
