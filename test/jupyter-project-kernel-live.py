"""Launch only our disposable test kernel through the real remote_ikernel."""
import json
import os
from pathlib import Path
import sys
import tempfile

from jupyter_client import KernelManager
from jupyter_client.kernelspec import KernelSpecManager

host, root, python = sys.argv[1:]
with tempfile.TemporaryDirectory(prefix="jupyter-project-kernel-") as directory:
    specdir = Path(directory) / "project-audit"
    specdir.mkdir()
    specfile = specdir / "kernel.json"
    specfile.write_text(json.dumps({
        "display_name": "Disposable project audit", "language": "python",
        "argv": [sys.executable, "-m", "remote_ikernel", "--interface", "ssh",
                 "--host", host, "--kernel_cmd", "python -m ipykernel_launcher -f {host_connection_file}",
                 "--project-file", str(specfile), "{connection_file}"],
        "metadata": {"aaron": {"project": {"root": root, "python": python, "direnv": True}}}}))
    manager = KernelManager(kernel_name="project-audit",
                            kernel_spec_manager=KernelSpecManager(kernel_dirs=[directory]))
    client = None
    try:
        manager.start_kernel(cwd=directory)
        client = manager.blocking_client()
        client.start_channels()
        client.wait_for_ready(timeout=45)
        msgid = client.execute(
            "import os,sys,numpy\n"
            "assert os.environ.get('NOEMA_PROJECT_AUDIT') == 'remote-envrc'\n"
            f"assert os.getcwd() == {root.rstrip('/')!r}\n"
            f"assert sys.executable == {python!r}\n"
            "print('PASS real remote kernel, project Python/cwd, direnv, numpy')")
        while True:
            message = client.get_shell_msg(timeout=30)
            if message["parent_header"].get("msg_id") == msgid:
                if message["content"]["status"] != "ok":
                    raise RuntimeError(message["content"])
                break
        print("PASS real remote kernel execution, project Python/cwd, direnv, numpy")
    finally:
        if client:
            client.stop_channels()
        if manager.has_kernel:
            manager.shutdown_kernel(now=True)
