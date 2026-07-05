#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JUPYTER_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
AARONNOTE_ROOT="$(cd "${JUPYTER_ROOT}/.." && pwd)"
VENV="${JUPYTER_ROOT}/.venv"
LABEXTENSION_ROOT="${JUPYTER_ROOT}/labextension"
JUPYTER_BUILDER="${AARONNOTE_ROOT}/node_modules/@jupyter/builder/lib/build-labextension.js"
JUPYTER_CORE_META="${AARONNOTE_ROOT}/node_modules/@jupyterlab/core-meta/core.package.json"

if [ ! -x "${VENV}/bin/jupyter" ]; then
  printf 'Aaronnote Jupyter is not bootstrapped. Run npm run jupyter:bootstrap in %s.\n' "$AARONNOTE_ROOT" >&2
  exit 2
fi

if [ ! -x "${AARONNOTE_ROOT}/node_modules/.bin/tsc" ] || \
   [ ! -f "$JUPYTER_BUILDER" ] || \
   [ ! -f "$JUPYTER_CORE_META" ]; then
  printf 'Missing Aaronnote npm dependencies for the JupyterLab extension.\n' >&2
  printf 'Run npm install in %s, then retry.\n' "$AARONNOTE_ROOT" >&2
  exit 2
fi

mkdir -p "${JUPYTER_ROOT}/.jupyter/data/labextensions"

export JUPYTER_CONFIG_DIR="${JUPYTER_ROOT}/.jupyter/config"
export JUPYTER_DATA_DIR="${JUPYTER_ROOT}/.jupyter/data"
export JUPYTER_RUNTIME_DIR="${JUPYTER_ROOT}/.jupyter/runtime"
export JUPYTER_PATH="${JUPYTER_ROOT}/.jupyter/data"
export PYTHONNOUSERSITE=1
export PATH="${VENV}/bin:${AARONNOTE_ROOT}/node_modules/.bin:${PATH}"

cd "$AARONNOTE_ROOT"
npm run build:lib
node "${JUPYTER_ROOT}/scripts/generate-embedded-style.mjs"

cd "$LABEXTENSION_ROOT"
npm run build:ts
node "$JUPYTER_BUILDER" --core-package-file "$JUPYTER_CORE_META" .
