#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JUPYTER_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
KERNEL_DATA="${JUPYTER_ROOT}/.jupyter/data/kernels"
TEMPLATE_ROOT="${JUPYTER_ROOT}/kernel-templates"

mkdir -p "${KERNEL_DATA}/sagemath-10.9" "${KERNEL_DATA}/bash"
sed "s|@AARONNOTE_JUPYTER_ROOT@|${JUPYTER_ROOT}|g" \
  "${TEMPLATE_ROOT}/sagemath-10.9/kernel.json" \
  >"${KERNEL_DATA}/sagemath-10.9/kernel.json"
sed "s|@AARONNOTE_JUPYTER_ROOT@|${JUPYTER_ROOT}|g" \
  "${TEMPLATE_ROOT}/bash/kernel.json" \
  >"${KERNEL_DATA}/bash/kernel.json"

printf 'Installed kernelspecs under %s\n' "$KERNEL_DATA"
