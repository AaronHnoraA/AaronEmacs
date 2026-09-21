# LaTeX 数学预览

本文是 Emacs 内数学公式预览的权威说明:支持哪些公式写法、宏从哪里来、出问题先看哪里。
文档编译(latexmk/XeLaTeX → PDF Tools/SyncTeX)属于另一条链路,见
[settings-cookbook.md](settings-cookbook.md) 第 3 节。

## 取向

- 预览后端是 **RaTeX**:一个 KaTeX 兼容的 Rust 渲染器,vendored 在
  `site-lisp/ratex.el/`,通过 JSONL over stdio 与一个本地二进制通信。
  选它而不是 node + MathJax 的方案,关键理由是 Noema 网页端 vendored 的也是
  KaTeX —— 同一套语义意味着 buffer 里看到的和发布出去的一致。
- 预览形态是**光标上方的 posframe 弹窗**,不是 inline overlay。
  `ratex-inline-preview` 保持 `nil`;不要为了"更像 Org 预览"去打开它。
- 只在 TeX 系列 major mode 生效(`latex-mode` / `LaTeX-mode` / `tex-mode` /
  `plain-TeX-mode` / `docTeX-mode`)。Markdown 走 Noema 的 CM6 编辑器,
  由 KaTeX + MathLive 负责,不经过这里。

## 支持的公式写法

检测器在 `site-lisp/ratex.el/lisp/ratex-math-detect.el`。

| 写法 | 样式 | 说明 |
|---|---|---|
| `$ … $` | inline | 单行;`\$` 转义与 `%` 注释内的 `$` 会被忽略 |
| `$$ … $$` | display | 可跨行 |
| `\( … \)` | inline | |
| `\[ … \]` | display | 可跨行 |
| `\begin{ENV} … \end{ENV}` | display | 白名单见 `ratex-math-environments`,支持同名嵌套 |
| `#+begin_display_latex` | display | Org 专用块 |

环境白名单只收引擎真正实现的那些(`align` `aligned` `alignat` `array`
`cases` `dcases` `drcases` `equation` `gather` `gathered` `matrix` `pmatrix`
`bmatrix` `Bmatrix` `vmatrix` `Vmatrix` `rcases` `smallmatrix` `split`
`subarray` `darray` 及其带星变体),外加需要兼容改写的 `multline`。
`\begin{figure}` 之类不是数学,不会被送去渲染。

嵌套时取**最宽**的那个:`\[ \begin{cases} … \end{cases} \]` 选外层 `\[`,
`\begin{align}` 里嵌 `\begin{cases}` 选 `align` —— 渲染整块才是对的。

`$$`、`\[` 和 `#+begin_display_latex` 会补 `\displaystyle`;
环境类**整体原样下发**,因为 `\begin{align}` 本身就是排版指令。

## 宏:一份资产,两个渲染器

`site-lisp/noema/resources/` 是数学宏和 TeX 兼容规则的唯一来源,
Emacs 侧通过相对软链接接过来:

```
etc/katex-macros             -> ../site-lisp/noema/resources/katex-macros
etc/prose-accepted-words.txt -> ../site-lisp/noema/resources/prose-accepted-words.txt
templates/latex              -> ../site-lisp/noema/resources/templates/latex
templates/tex                -> ../site-lisp/noema/resources/templates/tex
templates/noema              -> ../site-lisp/noema/resources/templates/noema
```

这些链接**必须是相对路径**。历史上它们指向一个后来搬走的绝对路径,
断链之后 Emacs 会把死路径通过 `AARONNOTE_KATEX_MACROS_DIR` 交给 Noema,
于是两边都加载不到任何宏,而且没有任何报错。
`my/health-critical-check`(`make doctor`)现在会检查这五条链接。

`lisp/lang/tex/init-latex.el` 读同一批 `.tex`,编译成前言注入每次渲染:

- 解析的语法子集与 `shared/katex-macros.mjs`(JS)和 `kernel/noema/katexmacros`
  (Go)一致,并由同一份 `shared/katex-macro-fixtures.json` 约束 ——
  测试在 `test/init-writing-tests.el`,不要再写第四份解析器。
- 输出统一是 `\def` 形式,不是 `\newcommand`:`\N`、`\vec` 等名字在引擎里已有
  定义,`\newcommand` 遇到重定义会报错,那会让**整个前言连同每条预览**一起失败。
- 前言对所有公式相同,所以缓存键里只放它的哈希 token,不放正文。

TeX 兼容改写(目前 `multline → gathered`、`\displaylines`)的规则表在
`site-lisp/noema/shared/tex-compat-rules.json`,TS 和 Elisp 各自读它。

`\tr` 是带一个参数的转置,`\Tr` 是迹,`\im` 是 `im`。
`assignment.cls` 曾经另外声明 `\tr`=Tr、`\im`=Im,但生成的
`aaronnote-macros.sty` 会覆盖 `.cls`,结果作业 PDF 里 `\tr{A}` 被静默渲染成
转置。冲突声明已从 `.cls` 移除,以 `katex-macros/` 为准。

## 出问题时

先跑 `M-x my/latex-preview-doctor`。它一次性报告:后端是否存活、在途请求数、
本 buffer 缓存条数、最近一次错误、宏目录与宏条数、前言/兼容钩子是否接上、
以及光标处检测到的 fragment。

常见情形:

- **弹窗里显示红色错误文字** —— 渲染确实失败了,文字就是引擎的报错。
  公式写到一半时这是正常的;写完还在报,说明用了引擎不支持的命令。
- **弹窗根本不出现** —— 先看 doctor 的 "fragment at point"。
  如果是 `none at point`,是检测器没认出来,不是渲染问题。
- **后端 down** —— `M-x ratex-diagnose-backend` 看二进制路径;
  需要重建时 `M-x ratex-build-backend`(需要 cargo)。
  `ratex-auto-download-backend` 被刻意设为 `nil`:默认的 `t` 会在启动失败时
  删掉本地编译的二进制,再去 GitHub 拉一个可能与 vendored `ratex-core`
  版本不匹配的 release。
- **想看日志** —— `ratex-debug` 置 t 后 `M-x ratex-debug-open-buffer`。
  日志已做截断和缓冲区上限;在此之前打开它本身就会让 Emacs 更卡。

运行时开销看 `M-x my/performance-watch` 的 "Math preview" 与
"Pending renders" 两行。

## 本地 fork 说明

`site-lisp/ratex.el/` 是 vendored 上游并带有本地改动,详见
[maintenance.md](maintenance.md)。
