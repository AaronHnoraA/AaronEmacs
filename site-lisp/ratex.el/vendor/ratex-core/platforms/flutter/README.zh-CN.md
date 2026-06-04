# RaTeX — Flutter 集成说明

通过 Dart FFI 与 CustomPainter 在 Flutter 中原生渲染 LaTeX 数学公式。  
无 WebView、无 JavaScript。

---

## 工作原理

```
LaTeX 字符串
    ↓ RaTeXFfi.parseAndLayout()   [Dart FFI → libratex_ffi]
JSON DisplayList
    ↓ DisplayList.fromJson()       [Dart JSON 解码]
DisplayList
    ↓ RaTeXPainter.paint()         [flutter/canvas]
CustomPaint Widget
```

---

## 开箱即用

1. **添加依赖** — 在 `pubspec.yaml` 中：`ratex_flutter: ^0.0.3`，然后执行 `flutter pub get`。无需自行编译原生库，发布包内已含 Android `.so` 与 iOS XCFramework。
2. **使用** — 直接使用 `RaTeXWidget`：
   ```dart
   RaTeXWidget(
     latex: r'\frac{-b \pm \sqrt{b^2-4ac}}{2a}',
     fontSize: 28,
     onError: (e) => debugPrint('RaTeX: $e'),
   )
   ```

---

## 安装

### 从 pub.dev（推荐）

在 `pubspec.yaml` 中添加：

```yaml
dependencies:
  ratex_flutter: ^0.0.3
```

然后执行 `flutter pub get`。无需本地构建 — 已发布包内含预编译的 Android `.so` 与 iOS `RaTeX.xcframework`。

### 从本地路径（开发）

若从 RaTeX 仓库使用该包：

```yaml
dependencies:
  ratex_flutter:
    path: /path/to/RaTeX/platforms/flutter
```

需先构建原生库：

| 平台 | 命令 |
|------|------|
| iOS | `bash platforms/ios/build-ios.sh`（生成 `RaTeX.xcframework`） |
| Android | `bash platforms/android/build-android.sh`（生成 `.so` 文件） |

**从源码构建的环境要求**：Flutter 3.10+、Dart 3.0+、Rust 1.75+。

---

## 使用

### Widget（推荐）

```dart
import 'package:ratex_flutter/ratex_flutter.dart';

class MathPage extends StatelessWidget {
  @override
  Widget build(BuildContext context) => Scaffold(
    body: Center(
      child: RaTeXWidget(
        latex: r'\frac{-b \pm \sqrt{b^2-4ac}}{2a}',
        fontSize: 28,
        onError: (e) => debugPrint('RaTeX: $e'),
      ),
    ),
  );
}
```

### 底层 CustomPainter

```dart
import 'package:ratex_flutter/ratex_flutter.dart';

final dl      = RaTeXEngine.instance.parseAndLayout(r'\sum_{n=1}^\infty \frac{1}{n^2}');
final painter = RaTeXPainter(displayList: dl, fontSize: 24);

// 在 CustomPaint 中：
CustomPaint(painter: painter, size: Size(painter.widthPx, painter.totalHeightPx))
```

### 异步（大公式）

```dart
import 'package:flutter/foundation.dart';

final dl = await compute(
  (latex) => RaTeXEngine.instance.parseAndLayout(latex),
  r'\prod_{n=1}^\infty \left(1 - \frac{1}{n^2}\right)',
);
```

---

## 坐标系

与 iOS/Android 一致：所有坐标为 **em 单位**，乘以 `fontSize`（逻辑像素）得到屏幕坐标。Y 自边界框顶部向下递增。基线位于 Y = `height × fontSize`。

---

## 文件说明

| 文件 | 说明 |
|------|------|
| `pubspec.yaml` | Flutter 插件清单 |
| `ios/` | iOS 插件（podspec + RaTeXPlugin.swift）；链接 RaTeX.xcframework |
| `android/` | Android 插件（RaTeXPlugin.kt）；使用包内 `jniLibs` 中的 `libratex_ffi.so` |
| `lib/ratex_flutter.dart` | 对外 API：`RaTeXEngine`、`RaTeXWidget` |
| `lib/src/display_list.dart` | Dart JSON 类型（DisplayList、DisplayItem 等） |
| `lib/src/ratex_ffi.dart` | 对 `libratex_ffi` 的 Dart FFI 绑定 |
| `lib/src/ratex_painter.dart` | `CustomPainter` 绘制循环 |

---

## 发布到 pub.dev（维护者）

要发布**开箱即用**、无需用户构建原生代码的包：

1. **Android** — 构建并将 JNI 库复制到包内：
   ```bash
   # 在仓库根目录
   ./platforms/android/build-android.sh
   cp -R platforms/android/src/main/jniLibs/* platforms/flutter/android/src/main/jniLibs/
   ```

2. **iOS** — 确保 `RaTeX.xcframework` 在包内为实体目录（非符号链接）：
   ```bash
   # 在仓库根目录
   ./platforms/ios/build-ios.sh
   # 若 platforms/flutter/ios/RaTeX.xcframework 为符号链接，替换为实体拷贝：
   rm -rf platforms/flutter/ios/RaTeX.xcframework
   cp -R platforms/ios/RaTeX.xcframework platforms/flutter/ios/
   ```

3. **校验并发布**：
   ```bash
   cd platforms/flutter
   dart pub publish --dry-run
   dart pub publish
   ```

   **CI**：推送版本 tag（如 `v0.0.4`）会触发 [release-flutter.yml](https://github.com/erweixin/RaTeX/blob/main/.github/workflows/release-flutter.yml)：构建 Android 与 iOS 原生库、注入本包并执行 `dart pub publish`。请确保 tag 与 `pubspec.yaml` 中的 `version` 一致。仓库需配置 Secret：`PUB_DEV_TOKEN`（在 https://pub.dev/settings/tokens 创建）。
