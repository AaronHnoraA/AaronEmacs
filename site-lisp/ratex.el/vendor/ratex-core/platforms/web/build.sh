#!/usr/bin/env bash
set -e
cd "$(dirname "$0")"
echo "Building ratex-wasm for web..."
wasm-pack build ../../crates/ratex-wasm --target web --out-dir "$(pwd)/pkg"
echo "Done. Output in platforms/web/pkg/"
