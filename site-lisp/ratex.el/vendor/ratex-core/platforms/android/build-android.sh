#!/usr/bin/env bash
# build-android.sh — Build libratex_ffi.so for Android ABIs
#
# Prerequisites:
#   cargo install cargo-ndk
#   rustup target add aarch64-linux-android armv7-linux-androideabi x86_64-linux-android
#   NDK installed (set ANDROID_NDK_HOME or let cargo-ndk auto-detect)
#
# Output: platforms/android/src/main/jniLibs/{arm64-v8a,armeabi-v7a,x86_64}/libratex_ffi.so

set -eo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
JNILIBS="$REPO_ROOT/platforms/android/src/main/jniLibs"

abi_for() {
    case "$1" in
        aarch64-linux-android)  echo "arm64-v8a" ;;
        armv7-linux-androideabi) echo "armeabi-v7a" ;;
        x86_64-linux-android)   echo "x86_64" ;;
        *) echo "unknown target: $1" >&2; exit 1 ;;
    esac
}

echo "==> Building ratex-ffi for Android targets (parallel)..."
PIDS=()
for RUST_TARGET in aarch64-linux-android armv7-linux-androideabi x86_64-linux-android; do
    ABI="$(abi_for "$RUST_TARGET")"
    echo "    → $RUST_TARGET ($ABI) [starting]"
    (
        cargo ndk \
            --target "$RUST_TARGET" \
            --manifest-path "$REPO_ROOT/Cargo.toml" \
            build --release -p ratex-ffi

        DEST="$JNILIBS/$ABI"
        mkdir -p "$DEST"
        cp "$REPO_ROOT/target/$RUST_TARGET/release/libratex_ffi.so" "$DEST/"
        echo "    ✓ $RUST_TARGET done"
    ) &
    PIDS+=($!)
done

for PID in "${PIDS[@]}"; do
    wait "$PID" || { echo "==> Build failed!"; exit 1; }
done

echo "==> Done. Libraries copied to $JNILIBS"
