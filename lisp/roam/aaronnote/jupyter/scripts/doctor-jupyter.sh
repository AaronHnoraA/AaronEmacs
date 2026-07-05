#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JUPYTER_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
VENV="${JUPYTER_ROOT}/.venv"

export JUPYTER_CONFIG_DIR="${JUPYTER_ROOT}/.jupyter/config"
export JUPYTER_DATA_DIR="${JUPYTER_ROOT}/.jupyter/data"
export JUPYTER_RUNTIME_DIR="${JUPYTER_ROOT}/.jupyter/runtime"
export JUPYTER_PATH="${JUPYTER_ROOT}/.jupyter/data"
export IPYTHONDIR="${JUPYTER_ROOT}/.jupyter/ipython"
export PYTHONNOUSERSITE=1
export PATH="${VENV}/bin:${PATH}"
ALLOWED_KERNELS="${AARONNOTE_JUPYTER_ALLOWED_KERNELS:-[\"python3\", \"sagemath-10.9\"]}"

printf 'Aaronnote Jupyter doctor\n'
printf 'root=%s\n' "$JUPYTER_ROOT"
printf 'venv=%s\n' "$VENV"
printf 'JUPYTER_CONFIG_DIR=%s\n' "$JUPYTER_CONFIG_DIR"
printf 'JUPYTER_DATA_DIR=%s\n' "$JUPYTER_DATA_DIR"
printf 'JUPYTER_RUNTIME_DIR=%s\n' "$JUPYTER_RUNTIME_DIR"
printf 'JUPYTER_PATH=%s\n' "$JUPYTER_PATH"
printf 'IPYTHONDIR=%s\n' "$IPYTHONDIR"
printf 'PYTHONNOUSERSITE=%s\n' "$PYTHONNOUSERSITE"
printf 'allowed_kernels=%s\n' "$ALLOWED_KERNELS"

if [ ! -x "${VENV}/bin/python" ]; then
  printf 'missing venv: run npm run jupyter:bootstrap from lisp/roam/aaronnote\n' >&2
  exit 2
fi

"${VENV}/bin/python" - <<'PY'
from importlib.metadata import version
import sys

print(sys.version.split()[0])
for dist in ("jupyter-server", "ipykernel"):
    print(f"{dist} {version(dist)}")
PY
"${VENV}/bin/jupyter" kernelspec list "--KernelSpecManager.allowed_kernelspecs=${ALLOWED_KERNELS}"
