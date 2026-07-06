"""CLI coverage using an isolated Jupyter data directory."""

import json
import os
from pathlib import Path
import subprocess
import sys


ROOT = Path(__file__).resolve().parents[1]


def run_cli(tmp_path, *args, check=True):
    env = os.environ.copy()
    env.update({
        "IPYTHONDIR": str(tmp_path / "ipython"),
        "JUPYTER_CONFIG_DIR": str(tmp_path / "config"),
        "JUPYTER_DATA_DIR": str(tmp_path / "data"),
        "PYTHONPATH": str(ROOT),
    })
    result = subprocess.run(
        [sys.executable, "-m", "remote_ikernel", *args],
        cwd=ROOT,
        env=env,
        text=True,
        capture_output=True,
    )
    if check and result.returncode != 0:
        raise AssertionError(result.stdout + result.stderr)
    return result


def kernel_json(tmp_path, name):
    return json.loads(
        (tmp_path / "data" / "kernels" / name / "kernel.json").read_text()
    )


def test_launcher_help_and_version(tmp_path):
    result = run_cli(tmp_path, check=False)
    assert result.returncode != 0
    assert "usage: __main__.py" in run_cli(tmp_path, "--help").stdout
    version = run_cli(tmp_path, "-V")
    assert "0.4.6+aaron.1" in version.stdout + version.stderr


def test_manage_minimum_arguments(tmp_path):
    result = run_cli(
        tmp_path,
        "manage", "--add", "--kernel_cmd=command", "--name=name",
        check=False,
    )
    assert result.returncode != 0
    assert "interface must be specified" in result.stderr

    result = run_cli(
        tmp_path,
        "manage", "--add", "--interface=local", "--name=name",
        check=False,
    )
    assert result.returncode != 0
    assert "kernel_cmd is required" in result.stderr


def test_add_group_show_change_and_delete(tmp_path):
    added = run_cli(
        tmp_path,
        "manage", "--add",
        "--interface=ssh",
        "--host=example",
        "--kernel_cmd=python -m ipykernel -f {connection_file}",
        "--name=Python",
        "--language=python",
    )
    assert "Added kernel ['rik_ssh_example_python']" in added.stdout

    spec = kernel_json(tmp_path, "rik_ssh_example_python")
    remote = spec["metadata"]["aaron"]["remote_kernel"]
    assert remote["group"] == "temporary"
    assert remote["config"]["host"] == "example"
    assert remote["config"]["interface"] == "ssh"

    changed = run_cli(
        tmp_path, "manage", "--set-group",
        "rik_ssh_example_python", "core",
    )
    assert "group to core" in changed.stdout
    assert kernel_json(tmp_path, "rik_ssh_example_python")["metadata"]["aaron"]["remote_kernel"]["group"] == "core"

    shown = run_cli(tmp_path, "manage", "--show", "rik_ssh_example_python")
    assert "Kernel found in" in shown.stdout
    removed = run_cli(tmp_path, "manage", "--delete", "rik_ssh_example_python")
    assert "Removed kernel" in removed.stdout
    assert not (tmp_path / "data" / "kernels" / "rik_ssh_example_python").exists()


def test_explicit_core_group(tmp_path):
    run_cli(
        tmp_path,
        "manage", "--add",
        "--interface=local",
        "--kernel_cmd=command {connection_file}",
        "--name=Core",
        "--group=core",
    )
    spec = kernel_json(tmp_path, "rik_local_core")
    assert spec["metadata"]["aaron"]["remote_kernel"]["group"] == "core"
