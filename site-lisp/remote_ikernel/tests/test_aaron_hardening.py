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

    kernel.tunnel_hosts = ["gateway:2200"]
    assert "ssh -o StrictHostKeyChecking=accept-new -p 2200 gateway" == kernel.tunnel_hosts_cmd


def test_kernel_tunnel_fails_closed_and_has_no_artificial_expiry():
    kernel = RemoteIKernel.__new__(RemoteIKernel)
    kernel.host = "compute.example:2222"
    kernel.tunnel_hosts = ["login.example:2200"]
    kernel.log = type("Log", (), {"debug": lambda *_args: None})()
    command = kernel.tunnel_cmd

    assert "-N -T" in command
    assert "ExitOnForwardFailure=yes" in command
    assert "ServerAliveInterval=30" in command
    assert "ServerAliveCountMax=3" in command
    assert "ConnectTimeout=15" in command
    assert "-J login.example:2200" in command
    assert "-p 2222 compute.example" in command
    assert "sleep 600" not in command
    for port_name in (
        "hb_port", "shell_port", "iopub_port", "stdin_port", "control_port"
    ):
        assert "{{{0}}}".format(port_name) in command
