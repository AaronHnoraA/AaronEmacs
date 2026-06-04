# RaTeX iOS Demo

SwiftUI app that renders a list of LaTeX formulas using RaTeX + CoreGraphics (no WebView).

## Prerequisites

| Tool | Version |
|------|---------|
| Xcode | 15+ |
| Rust | 1.75+ (`rustup`) |
| iOS Simulator or device | iOS 14+ |

Install Rust iOS targets once:

```bash
rustup target add aarch64-apple-ios aarch64-apple-ios-sim x86_64-apple-ios
```

## Step 1 — Build the XCFramework

From the **repo root**:

```bash
bash platforms/ios/build-ios.sh
```

This compiles the Rust engine for all iOS targets and produces
`platforms/ios/RaTeX.xcframework`.

## Step 2 — Open in Xcode

```bash
open demo/ios/RaTeXDemo/RaTeXDemo.xcodeproj
```

Select an iPhone simulator and press **Run (⌘R)**. Demo 开箱即用，无需额外配置。

> **Fonts**: KaTeX 字体已通过 Copy Bundle Resources 打入 demo target，首次渲染时由
> `RaTeXFontLoader.ensureLoaded()` / `loadFromBundle()` 自动加载。其他应用集成时：SPM 使用包内字体；手动集成可将 `platforms/ios/Sources/Ratex/Fonts/` 加入 target 的 Copy Bundle Resources。

## Project structure

```
demo/ios/RaTeXDemo/
├── RaTeXDemo.xcodeproj/     # Xcode project
└── RaTeXDemo/
    ├── RaTeXDemoApp.swift   # App entry point
    └── ContentView.swift    # Formula list UI
```

The library sources live in `platforms/ios/Sources/Ratex/` and are
referenced by the Xcode project via relative paths; Fonts are included as a folder reference (Copy Bundle Resources).
