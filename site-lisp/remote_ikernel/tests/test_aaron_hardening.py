"""Tests for the locally maintained SSH policy."""

from remote_ikernel.kernel import RemoteIKernel


class FakeConnection:
    def read_nonblocking(self, _size):
        from pexpect import TIMEOUT
        raise TIMEOUT("done")


def test_launch_ssh_accepts_new_but_not_changed_host_keys():
    kernel = RemoteIKernel.__new__(RemoteIKernel)
    kernel.host = "example"
    kernel.launch_args = None
    kernel.log = type("Log", (), {"info": lambda *_args: None})()
    kernel.connection = FakeConnection()
    commands = []

    def spawn(command):
        commands.append(command)
        return kernel.connection

    kernel._spawn = spawn
    kernel.launch_ssh()
    assert commands == [
        "ssh -o StrictHostKeyChecking=accept-new  example"
    ]


def test_tunnel_host_chain_uses_hardened_policy():
    kernel = RemoteIKernel.__new__(RemoteIKernel)
    kernel.tunnel_hosts = ["gateway"]
    assert "StrictHostKeyChecking=accept-new" in kernel.tunnel_hosts_cmd
    assert "StrictHostKeyChecking=no" not in kernel.tunnel_hosts_cmd
