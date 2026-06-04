# 发布与统一版本

本仓库通过 **Git 标签 `v*`** 触发多平台发布，并约定**同一 tag 下各包使用同一版本号**。

## 版本落在哪里

| 位置 | 用途 |
|------|------|
| `VERSION` | 单一来源，供脚本和人工对齐用 |
| `Cargo.toml` `[workspace.package].version` | Rust 所有 crate 的版本 |
| `platforms/flutter/pubspec.yaml` `version` | pub.dev 发布 |
| `platforms/web/package.json` `version` | npm 发布 |

Android/iOS 的产物版本在各自 workflow 里由 tag 推导，无需单独改 manifest。

## 发布前：统一版本

1. **改版本（二选一）**
   - 编辑根目录 `VERSION`，写新版本号（如 `0.0.10`），然后执行：
     ```bash
     ./scripts/set-version.sh
     ```
   - 或直接指定版本：
     ```bash
     ./scripts/set-version.sh 0.0.10
     ```
2. **提交**
   ```bash
   git add VERSION Cargo.toml platforms/flutter/pubspec.yaml platforms/web/package.json
   git commit -m "chore: release v0.0.10"
   ```
3. **打 tag 并推送**
   ```bash
   git tag v0.0.10
   git push origin main --tags
   ```

推送 tag 后会触发：

- **CI** (`ci.yml`) — 仅 main/PR 的 build/test，不依赖 tag
- **Release Cargo** (`release-crates.yml`) — 发布 workspace 内 crate 到 crates.io（需配置 `CARGO_REGISTRY_TOKEN`）
- **Release npm** (`release-npm.yml`) — 发布 `ratex-wasm` 到 npm
- **Release Flutter** (`release-flutter.yml`) — 发布到 pub.dev
- **Release Android** (`release-android.yml`) — 构建 AAR 并发布到 Maven Central
- **Release iOS** (`release-ios.yml`) — 构建 XCFramework、创建 GitHub Release

各 workflow 会校验对应 manifest 的版本是否与 tag 一致，不一致则失败。

## Cargo 首次发布到 crates.io

1. 在 [crates.io](https://crates.io) 登录并生成 API Token。
2. 仓库 Settings → Secrets and variables → Actions 中添加 `CARGO_REGISTRY_TOKEN`，值为该 token。
3. 若某次只想发部分 crate，可编辑 `.github/workflows/release-crates.yml` 中的 `for pkg in ...` 列表。

若**不打算**把 crate 发到 crates.io，可删除或禁用 `.github/workflows/release-crates.yml`，其余发布流程不受影响。
