#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JUPYTER_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
VENV="${JUPYTER_ROOT}/.venv"
PYTHON_BIN="${PYTHON:-python3}"

mkdir -p \
  "${JUPYTER_ROOT}/.jupyter/config" \
  "${JUPYTER_ROOT}/.jupyter/data" \
  "${JUPYTER_ROOT}/.jupyter/runtime" \
  "${JUPYTER_ROOT}/.jupyter/logs" \
  "${JUPYTER_ROOT}/.jupyter/ipython" \
  "${JUPYTER_ROOT}/.jupyter/tmp"

if [ ! -x "${VENV}/bin/python" ]; then
  "$PYTHON_BIN" -m venv "$VENV"
fi

export JUPYTER_CONFIG_DIR="${JUPYTER_ROOT}/.jupyter/config"
export JUPYTER_DATA_DIR="${JUPYTER_ROOT}/.jupyter/data"
export JUPYTER_RUNTIME_DIR="${JUPYTER_ROOT}/.jupyter/runtime"
export JUPYTER_PATH="${JUPYTER_ROOT}/.jupyter/data"
export IPYTHONDIR="${JUPYTER_ROOT}/.jupyter/ipython"
export PYTHONNOUSERSITE=1
export PATH="${VENV}/bin:${PATH}"

"${VENV}/bin/python" -m pip install --upgrade pip setuptools wheel
"${VENV}/bin/python" -m pip install --requirement "${JUPYTER_ROOT}/requirements.txt"

"${SCRIPT_DIR}/install-kernelspecs.sh"
"${SCRIPT_DIR}/build-labextension.sh"

printf 'Aaronnote Jupyter bootstrap complete:\n'
printf '  root: %s\n' "$JUPYTER_ROOT"
printf '  python: %s\n' "${VENV}/bin/python"
printf '  jupyter_data: %s\n' "$JUPYTER_DATA_DIR"
