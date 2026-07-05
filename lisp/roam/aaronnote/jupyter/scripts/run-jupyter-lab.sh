#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JUPYTER_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
AARONNOTE_ROOT="$(cd "${JUPYTER_ROOT}/.." && pwd)"
VENV="${JUPYTER_ROOT}/.venv"

if [ ! -x "${VENV}/bin/jupyter-lab" ]; then
  printf 'Aaronnote Jupyter is not bootstrapped. Run npm run jupyter:bootstrap in %s.\n' "$AARONNOTE_ROOT" >&2
  exit 2
fi

ROOT_DIR="${AARONNOTE_JUPYTER_ROOT:-${1:-${AARONNOTE_ROOT}}}"
if [ $# -gt 0 ] && [ "$1" = "$ROOT_DIR" ]; then
  shift
fi

mkdir -p \
  "${JUPYTER_ROOT}/.jupyter/config" \
  "${JUPYTER_ROOT}/.jupyter/data" \
  "${JUPYTER_ROOT}/.jupyter/runtime" \
  "${JUPYTER_ROOT}/.jupyter/logs" \
  "${JUPYTER_ROOT}/.jupyter/ipython" \
  "${JUPYTER_ROOT}/.jupyter/tmp" \
  "${JUPYTER_ROOT}/.jupyter/tmp/virtual_documents"

"${SCRIPT_DIR}/install-kernelspecs.sh" >/dev/null

export JUPYTER_CONFIG_DIR="${JUPYTER_ROOT}/.jupyter/config"
export JUPYTER_DATA_DIR="${JUPYTER_ROOT}/.jupyter/data"
export JUPYTER_RUNTIME_DIR="${JUPYTER_ROOT}/.jupyter/runtime"
if [ "${AARONNOTE_JUPYTER_USE_HOME_KERNELS:-1}" = "1" ]; then
  JUPYTER_PATH_ENTRIES=("${JUPYTER_ROOT}/.jupyter/data")
  HOME_JUPYTER_PATHS=()
  [ -n "${HOME:-}" ] && HOME_JUPYTER_PATHS+=("${HOME}/Library/Jupyter" "${HOME}/.local/share/jupyter")
  JUPYTER_PATH_ENTRIES+=("${HOME_JUPYTER_PATHS[@]}")
  [ -n "${JUPYTER_PATH:-}" ] && JUPYTER_PATH_ENTRIES+=("${JUPYTER_PATH}")
  export JUPYTER_PATH="$(IFS=:; printf '%s' "${JUPYTER_PATH_ENTRIES[*]}")"
else
  export JUPYTER_PATH="${JUPYTER_ROOT}/.jupyter/data"
fi
export IPYTHONDIR="${JUPYTER_ROOT}/.jupyter/ipython"
export PYTHONNOUSERSITE=1
export PATH="${VENV}/bin:${PATH}"
ALLOWED_KERNELS="${AARONNOTE_JUPYTER_ALLOWED_KERNELS:-}"

HOST="${AARONNOTE_JUPYTER_HOST:-127.0.0.1}"
PORT="${AARONNOTE_JUPYTER_PORT:-8890}"
PORT_RETRIES="${AARONNOTE_JUPYTER_PORT_RETRIES:-0}"
LABEXTENSIONS_USER="${JUPYTER_ROOT}/.jupyter/data/labextensions"
LABEXTENSIONS_VENV="${VENV}/share/jupyter/labextensions"

ARGS=(
  --no-browser \
  "--ServerApp.ip=${HOST}" \
  "--ServerApp.port=${PORT}" \
  "--ServerApp.port_retries=${PORT_RETRIES}" \
  "--IdentityProvider.token=" \
  "--ServerApp.password=" \
  "--ServerApp.root_dir=${ROOT_DIR}" \
  "--ContentsManager.allow_hidden=True" \
  "--LanguageServerManager.autodetect=False" \
  "--LanguageServerManager.virtual_documents_dir=${JUPYTER_ROOT}/.jupyter/tmp/virtual_documents" \
  "--LabApp.extension_manager=readonly" \
  "--LabApp.labextensions_path=${LABEXTENSIONS_USER}" \
  "--LabApp.labextensions_path=${LABEXTENSIONS_VENV}"
)

if [ -n "${ALLOWED_KERNELS}" ]; then
  ARGS+=("--KernelSpecManager.allowed_kernelspecs=${ALLOWED_KERNELS}")
fi

exec "${VENV}/bin/jupyter-lab" "${ARGS[@]}" "$@"
