# RaTeX — Android

Native LaTeX math on Android (Kotlin + Canvas). AAR includes KaTeX fonts.  
minSdk 21, targetSdk 34.

## Out of the box

1. **Add dependency** — In your app's `build.gradle`: `implementation("io.github.erweixin:ratex-android:0.0.3")` (or from Maven Central / local publish).
2. **Use** — Add `RaTeXView` in your layout and set LaTeX in code; fonts load automatically from `assets/fonts/` on first render.
   ```kotlin
   binding.mathView.latex = """\frac{-b \pm \sqrt{b^2-4ac}}{2a}"""
   binding.mathView.fontSize = 24f   // dp — no manual density conversion needed
   ```
   **Optional:** To preload fonts at startup, call `RaTeXFontLoader.loadFromAssets(context, "fonts")` in your Application or first screen.

## Prerequisites

NDK 26+, Rust + `cargo install cargo-ndk`, and targets:  
`rustup target add aarch64-linux-android armv7-linux-androideabi x86_64-linux-android`

## Build native lib

From repo root: `bash platforms/android/build-android.sh`  
→ outputs `src/main/jniLibs/{arm64-v8a,armeabi-v7a,x86_64}/libratex_ffi.so`

## Add to project

- **Maven:** `implementation("io.github.erweixin:ratex-android:0.0.3")` (after publishing; use `mavenLocal()` / `mavenCentral()` as needed).
- **Module:** include this folder as `:ratex-android` in settings.gradle and `implementation(project(":ratex-android"))` in app.

## Fonts

AAR has KaTeX in `assets/fonts/`. **RaTeXView** loads them automatically on first use. Optional: call `RaTeXFontLoader.loadFromAssets(context, "fonts")` at startup to load earlier.

## Usage

### Block formula — `RaTeXView`

```xml
<io.ratex.RaTeXView android:id="@+id/mathView"
    android:layout_width="wrap_content" android:layout_height="wrap_content" />
```

```kotlin
binding.mathView.latex = """\frac{-b \pm \sqrt{b^2-4ac}}{2a}"""
binding.mathView.fontSize = 24f   // dp — no manual density conversion needed
```

### Inline formula — `RaTeXSpan`

`RaTeXSpan` is a `ReplacementSpan` that renders a LaTeX formula inline with surrounding text. The formula baseline is aligned to the text baseline, and the line height expands automatically to accommodate the formula.

Rendering is async (uses `Dispatchers.IO` internally). Call `RaTeXSpan.create` from a coroutine:

```kotlin
private val scope = CoroutineScope(Dispatchers.Main + SupervisorJob())

fun showInlineFormula(textView: TextView) {
    scope.launch {
        val span = RaTeXSpan.create(
            context = this@MainActivity,
            latex   = """\frac{1+\sqrt{5}}{2}""",
            fontSize = 18f   // dp — converted to px internally
        )
        val ssb = SpannableStringBuilder("黄金比例 φ = ")
        val start = ssb.length
        ssb.append("\u200B")   // zero-width placeholder for the span
        ssb.setSpan(span, start, ssb.length, 0)
        ssb.append(" ≈ 1.618")
        textView.text = ssb
    }
}
```

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `context` | `Context` | Used for asset access during font loading. |
| `latex` | `String` | LaTeX math-mode string (no surrounding `$` or `\[…\]`). |
| `fontSize` | `Float` | Font size in **dp** (density-independent pixels). Converted to px internally. |

**Throws** `RaTeXException` if the formula cannot be parsed.

### Low-level (Compose / custom drawing)

```kotlin
val dl       = RaTeXEngine.parse(latex)
val renderer = RaTeXRenderer(dl, fontSize) { RaTeXFontLoader.getTypeface(it) }
// draw into any Canvas:
renderer.draw(canvas)
```

## Publish

- **Local:** `./gradlew :ratex-android:publishReleasePublicationToMavenLocal` (from a build that includes this module, e.g. `demo/android`).
- **Remote (e.g. GitHub Packages):** set `MAVEN_REPO_URL`, `MAVEN_USER`, `MAVEN_PASSWORD` in gradle.properties, then `./gradlew :ratex-android:publishReleasePublicationToRemote`.
- **Maven Central:** configure [central.sonatype.com](https://central.sonatype.com) + GPG + `SONATYPE_NEXUS_USERNAME` / `SONATYPE_NEXUS_PASSWORD` in gradle.properties; from root use `./gradlew publishToSonatype closeAndReleaseSonatypeStagingRepository`.
- **CI:** push tag `v0.0.4` → `.github/workflows/release-android.yml` publishes to Central. Set repo secrets: `SONATYPE_NEXUS_USERNAME`, `SONATYPE_NEXUS_PASSWORD`, `GPG_PRIVATE_KEY`, `GPG_PASSPHRASE`.

## Demo

From root: `bash platforms/android/build-android.sh`, then open `demo/android` in Android Studio and run.

**Troubleshooting:** UnsatisfiedLinkError → run `build-android.sh`. NDK not found → install NDK 26+ or set `ANDROID_NDK_HOME`.
