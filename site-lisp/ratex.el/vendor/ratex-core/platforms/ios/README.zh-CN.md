# RaTeX — iOS 集成说明

通过 Swift 与 CoreGraphics 在 iOS 上原生渲染 LaTeX 数学公式。  
无 WebView、无 JavaScript、无 DOM。

---

## 工作原理

```
LaTeX 字符串
    ↓ ratex_parse_and_layout() [C ABI，静态库]
JSON DisplayList
    ↓ RaTeXEngine.parse()       [Swift JSON 解码]
DisplayList
    ↓ RaTeXRenderer.draw()      [CoreGraphics]
UIView / SwiftUI View
```

---

## 开箱即用

1. **添加依赖** — Xcode：**File → Add Package Dependencies**，输入仓库 URL，选择 `RaTeX`。
2. **使用** — 直接使用 `RaTeXView` 或 `RaTeXFormula`，字体会在首次渲染时自动加载，无需手动调用。
   ```swift
   // SwiftUI
   RaTeXFormula(latex: #"\frac{-b \pm \sqrt{b^2-4ac}}{2a}"#, fontSize: 24)
   ```
   **可选**：若希望在启动时提前加载字体（例如避免首屏公式略有延迟），可在 App 启动时调用 `RaTeXFontLoader.loadFromPackageBundle()`。

**本地开发**（修改 RaTeX 源码时）：先在本仓库根目录执行 `bash platforms/ios/build-ios.sh`，再在 Xcode 里 **File → Add Package Dependencies → Add Local…** 选择 RaTeX 仓库根目录即可。

---

## 环境要求

| 工具 | 版本 |
|------|------|
| Xcode | 15+ |
| Rust | 1.75+（rustup） |
| iOS 目标 | 14+ |

安装 Rust iOS 目标（一次性）：

```bash
rustup target add aarch64-apple-ios aarch64-apple-ios-sim x86_64-apple-ios
```

---

## 构建 XCFramework

在仓库根目录执行：

```bash
bash platforms/ios/build-ios.sh
```

将生成 `platforms/ios/RaTeX.xcframework`。

---

## 接入 Xcode 项目

### 方式 A — Swift Package Manager（推荐）

**已发布版本** — 在 Xcode 中：**File → Add Package Dependencies**，输入 GitHub 仓库 URL，选择 `RaTeX` 产品。字体会在首次渲染时自动加载；可选在启动时调用 `RaTeXFontLoader.loadFromPackageBundle()` 提前加载。

**本地开发** — 构建好 XCFramework 后，在 Xcode 中指向仓库根目录（**File → Add Package Dependencies → Add Local…**）。

### 方式 B — 手动集成

1. 将 `platforms/ios/RaTeX.xcframework` 拖入 Xcode 项目。
2. 在 **Build Phases → Link Binary With Libraries** 中确保已链接。
3. 将 `platforms/ios/Sources/RaTeX/*.swift` 复制到你的项目中。
4. 将 `platforms/ios/Sources/Ratex/Fonts/` 下的 `Fonts` 文件夹加入 target 的 **Copy Bundle Resources**；字体会在首次渲染时自动加载，或在启动时调用 `RaTeXFontLoader.loadFromBundle()`。

---

## 使用

### UIKit

```swift
import RaTeX

let mathView = RaTeXView()
mathView.latex    = #"\frac{-b \pm \sqrt{b^2-4ac}}{2a}"#
mathView.fontSize = 28
mathView.onError  = { print("RaTeX error:", $0) }

// 自动布局
mathView.translatesAutoresizingMaskIntoConstraints = false
view.addSubview(mathView)
NSLayoutConstraint.activate([
    mathView.centerXAnchor.constraint(equalTo: view.centerXAnchor),
    mathView.centerYAnchor.constraint(equalTo: view.centerYAnchor),
])
```

### SwiftUI

```swift
import RaTeX

struct ContentView: View {
    var body: some View {
        RaTeXFormula(
            latex: #"\int_0^\infty e^{-x^2}\,dx = \frac{\sqrt{\pi}}{2}"#,
            fontSize: 24
        )
        .padding()
    }
}
```

### 底层自定义绘制

```swift
import RaTeX

let displayList = try RaTeXEngine.shared.parse(#"\sum_{n=1}^\infty \frac{1}{n^2}"#)
let renderer    = RaTeXRenderer(displayList: displayList, fontSize: 20)

// 在你的 UIView.draw(_:) 或 CGContext 块中：
renderer.draw(in: UIGraphicsGetCurrentContext()!)
```

---

## 坐标系

所有 `DisplayList` 坐标均为 **em 单位**。`RaTeXRenderer` 乘以 `fontSize`（pt）得到屏幕坐标。

- X 自左边缘向右递增。
- Y 自顶部向下递增。
- 基线位于 Y = `height × fontSize`。

---

## 文件说明

| 文件 | 说明 |
|------|------|
| `build-ios.sh` | 构建脚本 → 生成 `RaTeX.xcframework` |
| `Package.swift` | Swift Package 清单 |
| `Sources/RaTeX/DisplayList.swift` | Rust 类型的 Codable Swift 镜像 |
| `Sources/RaTeX/RaTeXEngine.swift` | 调用 C ABI、解码 JSON |
| `Sources/RaTeX/RaTeXRenderer.swift` | CoreGraphics 绘制循环 |
| `Sources/RaTeX/RaTeXView.swift` | UIKit `UIView` 与 SwiftUI `View` |
