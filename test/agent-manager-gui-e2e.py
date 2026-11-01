#!/usr/bin/env python3
"""Exercise actual Agent Manager child frames in an owned Emacs daemon.
No user daemon is touched. ACP peers are deterministic local subprocesses.
"""
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import uuid

ROOT = Path(__file__).resolve().parents[1]
SERVER = 'agent-manager-e2e-' + uuid.uuid4().hex[:12]
SCENARIOS = {
    'manager': ('test/agent-manager-gui-e2e.el', 'agent-manager-gui-e2e-run'),
    'display': ('test/agent-display-audit-gui-e2e.el', 'agent-display-audit-gui-e2e-run'),
    'queue': ('test/agent-queue-gui-e2e.el', 'agent-queue-gui-e2e-run'),
}


def client(expression, timeout=45):
    result = subprocess.run(
        [os.getenv('EMACSCLIENT', 'emacsclient'), '-s', SERVER, '-e', expression],
        cwd=ROOT, capture_output=True, text=True, timeout=timeout,
    )
    if result.returncode:
        raise RuntimeError((result.stdout + result.stderr)[-4000:])
    return result.stdout.strip()


def main():
    scenario = sys.argv[1] if len(sys.argv) > 1 else 'manager'
    test_file, test_function = SCENARIOS[scenario]
    pid = None
    try:
        subprocess.run([os.getenv('EMACS', 'emacs'), '-Q', '--daemon=' + SERVER],
                       cwd=ROOT, capture_output=True, text=True, timeout=25, check=True)
        pid = int(client('(emacs-pid)', 5))
        subprocess.run([os.getenv('EMACSCLIENT', 'emacsclient'), '-s', SERVER, '-c', '-n'],
                       cwd=ROOT, capture_output=True, text=True, timeout=20, check=True)
        init = '(progn (setq user-emacs-directory ' + json.dumps(str(ROOT) + '/')
        init += ' load-prefer-newer t)'
        for file in ['early-init.el', 'init.el', 'test/agent-flow-e2e.el', test_file]:
            init += ' (load-file ' + json.dumps(str(ROOT / file)) + ')'
        client(init + ')', 60)
        report = json.loads(json.loads(client('(json-encode (' + test_function + '))', 55)))
        print(json.dumps(report, ensure_ascii=False))
    finally:
        if pid is not None:
            try:
                client('(kill-emacs 0)', 5)
            except (RuntimeError, subprocess.TimeoutExpired):
                try:
                    os.kill(pid, signal.SIGTERM)
                except ProcessLookupError:
                    pass


if __name__ == '__main__':
    main()
