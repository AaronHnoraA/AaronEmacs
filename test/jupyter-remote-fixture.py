"""Prepare/clean an isolated live Jupyter fixture over ssh python3 stdin.
Usage: ssh HOST python3 - < test/jupyter-remote-fixture.py
Cleanup: ssh HOST python3 - --stop /tmp/noema-jupyter-audit.XXX < this-file
"""
import json
import os
from pathlib import Path
import secrets
import shutil
import signal
import subprocess
import sys
import tempfile
import time
import urllib.request

if len(sys.argv) == 3 and sys.argv[1] == '--stop':
    root = Path(sys.argv[2])
    if root.parent != Path('/tmp') or not root.name.startswith('noema-jupyter-audit.'):
        raise ValueError('Not an audit fixture')
    pid = int((root / 'server.pid').read_text())
    proc = Path(f'/proc/{pid}/cmdline')
    if proc.exists() and str(root).encode() in proc.read_bytes():
        os.kill(pid, signal.SIGTERM)
        for _ in range(50):
            if not proc.exists() or not proc.read_bytes():
                break
            time.sleep(.1)
        else:
            raise RuntimeError('Fixture server has not exited; preserving directory')
    shutil.rmtree(root)
    print('AUDIT_CLEANED')
else:
    root = Path(tempfile.mkdtemp(prefix='noema-jupyter-audit.', dir='/tmp'))
    try:
        subprocess.run([sys.executable, '-m', 'venv', '--without-pip', str(root / 'venv')], check=True)
        urllib.request.urlretrieve('https://bootstrap.pypa.io/pip/pip.pyz', root / 'pip.pyz')
        subprocess.run([str(root / 'venv/bin/python'), str(root / 'pip.pyz'), 'install', '-q',
                        'ipykernel', 'jupyter_server', 'ipywidgets'], check=True)
        (root / 'runtime').mkdir()
        (root / 'work').mkdir()
        env = dict(os.environ, JUPYTER_RUNTIME_DIR=str(root / 'runtime'), JUPYTER_TOKEN=secrets.token_hex(24))
        with (root / 'server.log').open('ab') as log:
            proc = subprocess.Popen([str(root / 'venv/bin/jupyter'), 'server', '--no-browser', '--ip=127.0.0.1',
                                     '--port=0', '--ServerApp.base_url=/audit/', '--ServerApp.root_dir='+str(root / 'work')],
                                    env=env, stdout=log, stderr=log, stdin=subprocess.DEVNULL, start_new_session=True)
        (root / 'server.pid').write_text(str(proc.pid))
        for _ in range(100):
            if list((root / 'runtime').glob('jpserver-*.json')):
                print(str(root))
                break
            if proc.poll() is not None:
                raise RuntimeError('Fixture server failed; see '+str(root / 'server.log'))
            time.sleep(.1)
        else:
            raise RuntimeError('Fixture server did not become ready')
    except Exception:
        print('Fixture failure; cleanup directory: '+str(root), file=sys.stderr)
        raise
