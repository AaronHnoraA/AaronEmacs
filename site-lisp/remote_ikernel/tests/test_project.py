"""The same kernelspec project controls kernel cwd, Python and direnv."""
import argparse
import json
import os
import shlex
import shutil
import subprocess

import pytest
from remote_ikernel.kernel import RemoteIKernel, apply_project_config


def test_project_overrides_and_quotes_launch_without_changing_ssh(tmp_path):
    file = tmp_path / 'kernel.json'
    file.write_text(json.dumps({'metadata': {'aaron': {'project': {
        'root': '/work/course space', 'python': '.conda/bin/python', 'direnv': True,
        'lsp': {'server': ['pyright-langserver', '--stdio']}}}}}))
    args = argparse.Namespace(project_file=str(file), workdir='/old', host='SSH-Alias',
                              kernel_cmd='python -m ipykernel -f {host_connection_file}')
    apply_project_config(args)
    assert args.host == 'SSH-Alias'
    assert args.workdir == '/work/course space'
    outer = shlex.split(args.kernel_cmd)
    assert outer[:5] == ['direnv', 'exec', '/work/course space', '/bin/sh', '-c']
    assert shlex.split(outer[5]) == [
        '/work/course space/.conda/bin/python',
        '-m', 'ipykernel', '-f', '{host_connection_file}']


def test_project_rejects_relative_root_and_opaque_command(tmp_path):
    file = tmp_path / 'kernel.json'
    for project, command in [({'root': 'relative'}, 'python -m ipykernel'),
                             ({'root': '/work', 'python': '.venv/bin/python'}, 'module load x; python')]:
        file.write_text(json.dumps({'metadata': {'aaron': {'project': project}}}))
        args = argparse.Namespace(project_file=str(file), workdir=None, kernel_cmd=command)
        with pytest.raises(ValueError):
            apply_project_config(args)


def test_legacy_launcher_remains_unchanged():
    args = argparse.Namespace(project_file=None, kernel_cmd='custom launcher', workdir=None)
    assert apply_project_config(args) is args
    assert args.kernel_cmd == 'custom launcher'


@pytest.mark.skipif(not shutil.which('direnv'), reason='direnv is not installed')
def test_direnv_covers_entire_script_and_blocks_unapproved_or_failed_envrc(tmp_path):
    # Only authorize our disposable script, with an isolated direnv allow store.
    root = tmp_path.resolve() / 'project with spaces'
    root.mkdir()
    env = {key: value for key, value in os.environ.items()
           if not key.startswith('DIRENV_') and key != 'NOEMA_SCRIPT_TEST'}
    for name in ('CONFIG', 'DATA', 'CACHE'):
        env['XDG_{}_HOME'.format(name)] = str(tmp_path.resolve() / name.lower())
    file = root / 'kernel.json'
    file.write_text(json.dumps({'metadata': {'aaron': {'project': {
        'root': str(root), 'direnv': True}}}}))
    marker = root / 'launched'
    args = argparse.Namespace(project_file=str(file), workdir=None,
                              kernel_cmd='printf "%s" "$NOEMA_SCRIPT_TEST"; '
                              'printf ":%s" "$NOEMA_SCRIPT_TEST"; touch launched')
    apply_project_config(args)

    def launch():
        return subprocess.run(args.kernel_cmd, shell=True, cwd=root, env=env,
                              text=True, capture_output=True, timeout=10)

    envrc = root / '.envrc'
    envrc.write_text('strict_env\nexport NOEMA_SCRIPT_TEST="value with spaces"\n')
    assert launch().returncode != 0
    assert not marker.exists()
    subprocess.run(['direnv', 'allow', '.'], cwd=root, env=env, check=True,
                   capture_output=True, timeout=10)
    result = launch()
    assert result.returncode == 0, result.stderr
    assert result.stdout == 'value with spaces:value with spaces'
    assert marker.exists()
    marker.unlink()
    envrc.write_text('strict_env\nfalse\nexport NOEMA_SCRIPT_TEST=incorrect\n')
    subprocess.run(['direnv', 'allow', '.'], cwd=root, env=env, check=True,
                   capture_output=True, timeout=10)
    assert launch().returncode != 0
    assert not marker.exists()


@pytest.mark.parametrize('exists', [False, True])
def test_kernel_directory_is_literal_and_missing_root_never_launches(tmp_path, exists):
    root = tmp_path / 'project $HOME "quoted"'
    if exists:
        root.mkdir()
    commands = []
    kernel = RemoteIKernel.__new__(RemoteIKernel)
    kernel.workdir = str(root)
    kernel.uuid = 'disposable-test'
    kernel.connection_info = {}
    kernel.precmd = None
    kernel.kernel_cmd = 'pwd > launched'
    kernel.log = type('Log', (), {'info': lambda *_: None})()
    kernel.connection = type('Connection', (), {
        'sendline': lambda _, command: commands.append(command),
        'expect': lambda *_: None})()
    kernel.start_kernel()
    result = subprocess.run(['/bin/sh', '-c', '\n'.join(commands)],
                            cwd=tmp_path, text=True, capture_output=True, timeout=10)
    assert not (tmp_path / 'launched').exists()
    if exists:
        assert result.returncode == 0, result.stderr
        assert (root / 'launched').read_text().strip() == str(root.resolve())
        assert not list(root.glob('rik_kernel-*.json'))
    else:
        assert result.returncode != 0
