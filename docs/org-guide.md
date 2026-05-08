# Org Guide

这套配置的 Org 是重功能路线，不主动对远程和大文件降载。

如果你要看的不是 Emacs 里的按键和模块，而是 `~/HC/Org/` 这个个人知识库 / 站点源目录怎么组织、`daily/` 和 `roam/` 怎么分工、tag 怎么约定，另见 [org-kb-guide.md](org-kb-guide.md)。

如果你要看 agenda、TODO 状态、任务 tags 和 `H-o w` 状态工具，另见
[org-task-workflow.md](org-task-workflow.md)。

## 1. 目录结构

在 [lisp/org/init-org.el](../lisp/org/init-org.el) 里默认是：

- `~/HC/Org/`
  主目录
- `~/HC/Org/roam/`
  org-roam
- `~/HC/Org/daily/`
  每日 capture
- `~/HC/Org/references/`
  参考文献和 bib

## 2. 核心入口

- `C-c a`
  `org-agenda`
- `C-c c`
  `org-capture`
- `C-c n`
  org-roam 前缀
- `C-c C-x v`
  手动刷新当前可视区域 LaTeX 预览

## 3. Capture 模板

当前默认模板在 [lisp/org/init-org-capture.el](../lisp/org/init-org-capture.el) 里，包括：

- `i`
  Idea
- `b`
  Inbox
- `m`
  Mail
- `n`
  Note
- `t`
  Meeting
- `u`
  Uni Task
- `l`
  Life Task

这些模板会落到 `daily/` 下不同子目录。

## 4. 新建文件模板

文件模板放在 [templates/org/](../templates/org/)，通过 `SPC f t`
或 `M-x my/template-switch` 手动选择并套到当前 Org buffer。`org` 默认不在
auto-insert allowlist 里，所以这些模板不会干扰 `org-capture` / `org-roam`
的新建流程。

占位符沿用统一模板系统：

- `{{title}}`
  套模板时提示输入标题
- `{{date}}` / `{{year}}` / `{{month_year}}`
  当前日期
- `{{author}}` / `{{user}}` / `{{file}}`
  当前用户和文件信息
- `{{cursor}}`
  套完模板后光标落点

当前 Org 文件模板覆盖这些场景：

- 基础笔记：`default.org`、`atomic-note.org`、`concept-note.org`、`summary-note.org`、`misc-note.org`
- 阅读和文献：`reading-summary.org`、`book-note.org`、`literature-note.org`、`paper-summary.org`、`zotero-note.org`
- 研究和学习：`research-question.org`、`experiment-log.org`、`dataset-note.org`、`course-lecture.org`、`problem-solving.org`、`flashcard-seed.org`
- 学科专题：`math-note.org`、`quantum-note.org`、`philosophy-note.org`
- TCS / 数学论文写作：`paper-outline-tcs.org`、`technical-overview.org`、`theorem-note.org`、`definition-bank.org`、`lemma-bank.org`、`proof-sketch.org`、`proof-attempt.org`、`proof-debug.org`、`reduction-note.org`、`lower-bound-proof.org`、`construction-note.org`、`counterexample-note.org`、`algorithm-analysis.org`、`notation-glossary.org`、`related-work-matrix.org`、`reviewer-response.org`
- 项目和工程：`project-plan.org`、`decision-record.org`、`code-investigation.org`、`bug-debug.org`
- 写作和输出：`writing-draft.org`、`talk-outline.org`
- 复盘和索引：`daily-review.org`、`weekly-review.org`、`index-map.org`
- 实验性工作流：`idea-lab.org`、`prompt-playground.org`

## 5. Agenda

Agenda、TODO 状态、DONE/CANCELLED 完成态、refile、archive、priority、effort
和 tags 快选现在集中在 [lisp/org/init-org-agenda.el](../lisp/org/init-org-agenda.el)。
具体状态语义和 tag taxonomy 见 [org-task-workflow.md](org-task-workflow.md)。

### 基本逻辑

- 默认不后台扫描 `my-org-root`
- 打开 agenda / tags view / refile 时才临时扫描
- 在 `~/HC/Org/` 下打开时扫整个 Org 库；在其他项目里扫当前项目；没有项目时扫当前目录
- TODO 流程是 `TODO -> NEXT/WIP/WAIT/HOLD/REVIEW -> DONE/CANCELLED/DROPPED`
- refile 目标默认是当前文件和 `org-agenda-files`
- archive 默认归到同文件的 `_archive` datetree

### 常用命令

- `C-c a`
  打开 agenda
- `org-agenda` 里 `o`
  打开 Overview / Dashboard 自定义视图
- `H-o t` / `H-o s` / `H-o d` / `H-o r`
  todo / schedule / deadline / refile
- `H-o w`
  打开任务状态和标记工具面板

## 6. Org Roam

当前前缀是 `C-c n`：

- `C-c n f`
  `org-roam-node-find`
- `C-c n i`
  `org-roam-node-insert`
- `C-c n t`
  添加 tag
- `C-c n a`
  添加 alias
- `C-c n o`
  创建或复用当前位置引用目标，并复制 Org link：标题复制 `[[id:...]]`，`\\[...\\]` 公式里必要时在 `\\]` 后插入 `<<eq-...>>`，`#+begin` 块里必要时在 `#+end_xxx` 后插入 `<<xxx-...>>`
- `C-c n I`
  手动插入 `id:` 链接
- `C-c n T`
  手动插入 dedicated target 链接
- `C-c n l`
  切换 roam buffer

macOS GUI 下，`Option` 映射到 `Hyper`，Org 常用入口也放到了 `H-o` 前缀：

- `H-o a` / `H-o c`
  `org-agenda` / `org-capture`
- `H-o b`
  打开 Org maintenance board
- `H-o m` / `H-o k` / `H-o O`
  重命名当前 `file:` 链接文件 / 给当前链接文件加清理豁免名 / 用系统 open 打开光标处链接或文件
- `H-o f` / `H-o i` / `H-o l`
  roam find / roam insert / roam buffer
- `H-o o`
  创建或复用当前位置引用目标，并复制 Org link
- `H-o H-o`
  创建或复用当前位置引用目标，并只复制裸 ID/target
- `H-o I` / `H-o L`
  插入 `id:` link / target link
- `H-o T` / `H-o v`
  刷新 overview TOC / 预览当前可视区域 LaTeX
- `H-o w`
  打开 agenda / TODO / tag 状态工具
- `H-o t` / `H-o s` / `H-o d` / `H-o r`
  todo / schedule / deadline / refile
- `H-o q` / `H-o p` / `H-o e` / `H-o A`
  tags / property / effort / archive
- `H-o z`
  从 BibTeX block 填充 Zotero 模板字段

剪贴板图片粘贴到 Org 的入口是 `H-y`。

Org 库清理入口是 `H-o b` / `M-x my/org-maintenance-board`。这个 board 会扫描
当前 scope：在 `~/HC/Org/` 里扫整个 Org 库，在其他项目里扫当前项目，没有项目时扫
当前目录。

- 找出未引用图片；图片只看同目录/子目录里的 Org `file:` 链接，不做全库交叉引用
- 找出 `attachments/`、`attach/`、`files/` 下未引用附件；附件按当前项目/scope 全量引用判断
- 找出损坏的本地媒体链接
- 统计并清理 `ltximg/` 里的 Org LaTeX preview cache
- 删除清理后留下的空 `img/`、`images/`、附件、`ltximg/` 目录
- 跳过 `public/`、`publish/`、`dist/`、`build/`、`css/`、`js/`、`CV/` 和 LaTeX preview cache 目录
- 文件名带 `keep-` 前缀或 `-keep` 后缀时豁免清理；`H-o k` 可以给当前链接文件加豁免名

同一 scope 的 Org link 解析结果会按文件签名缓存，所以 board 里连续执行清理动作不会
反复 parse 全部 Org 文件。

说明：

- `org-roam` 的“模板”来自 `org-roam-capture-templates`（见 [lisp/org/init-org-roam.el](../lisp/org/init-org-roam.el)），不走新建文件 `auto-insert` 模板系统。
- 另外这套配置里 `org` 的 `auto-insert` 默认是关闭的（不在 allowlist），避免新建 roam/capture 文件时发生模板叠加。

## 7. Org UI 增强

默认启用的增强包括：

- `mixed-pitch`
- `valign`
- `org-modern`
- `org-modern-indent`
- `org-appear`
- `olivetti`
- 自定义 special block overlay
- LaTeX 可见区域按需预览

现在这些增强默认不再因为远端或大文件整体关闭；其中高频成本比较明确的部分会按需启用或跳过无效刷新：`valign` 只在 buffer 已有表格或新插入表格时打开，`org-appear` 在 point/编辑状态未变化时不重复解析，`org-roam` 侧边 buffer 跟随刷新会短延迟合并，避免每个命令都重算。

## 8. Org Localleader

在 Org buffer 里用 `SPC m`：

- `SPC m a`
  archive subtree
- `SPC m d`
  deadline
- `SPC m e`
  effort
- `SPC m n`
  roam insert
- `SPC m N`
  roam find
- `SPC m r`
  refile
- `SPC m s`
  schedule
- `SPC m t`
  todo
- `SPC m v`
  预览当前可视区域 LaTeX
- `SPC m z`
  从 BibTeX block 填充 Zotero 模板字段

## 9. Org 跳转引用

这里讲的是 Org buffer 里的点击跳转，不是 LaTeX/PDF 导出时的 `\label` / `\ref` / `\eqref`。

### 基本原则

Org 里的 TOC 不是独立引用对象。TOC 只是标题链接列表；要引用某一级 TOC，实际应该引用那一级标题。

公式也一样。Org 里要能点击跳转，应该给公式附近放 Org 自己认识的 target，或者把重要公式放进带 `ID` 的 heading 下面。LaTeX 的 `\label{}` 主要服务导出，不是 Org 的链接跳转机制。

### 同文件标题

临时引用可以直接用标题链接：

```org
[[*章节标题]]
[[*章节标题][显示文字]]
```

这类链接依赖标题文字。标题改名后，链接可能失效。

更稳的是给标题加 `CUSTOM_ID`：

```org
* 章节标题
:PROPERTIES:
:CUSTOM_ID: sec-intro
:END:
```

然后引用：

```org
[[#sec-intro]]
[[#sec-intro][章节标题]]
```

### 跨文件标题

按标题跳转：

```org
[[file:math/algebra.org::*群的定义]]
```

按 `CUSTOM_ID` 跳转：

```org
[[file:math/algebra.org::#group-definition][群的定义]]
```

长期知识库里，更推荐给重要标题创建 `ID`：

```org
* 群的定义
:PROPERTIES:
:ID: 20260428T120000-group-definition
:END:
```

然后任何文件都可以引用：

```org
[[id:20260428T120000-group-definition][群的定义]]
```

这对 org-roam 最友好。文件移动、标题改名时，`id:` 链接比 `file:*标题` 更稳定，也更容易出现在 backlinks 里。

这套配置里 `C-c n o` / `H-o o` 会创建或复用当前位置引用目标，并复制 Org link。`H-o H-o` 会创建或复用同一个目标，并只复制裸 ID/target。用在标题上时，它适合给当前标题补一个稳定 `ID` 引用点。

同一个按键在 `\\[...\\]` display math 里会改为给公式创建 Org target：

```org
\[
E = mc^2
\]     <<eq-20260428T122000>>
```

target 会插到 `\\]` 后面。`C-c n o` / `H-o o` 复制 Org link；`H-o H-o` 复制裸 target 到 kill ring。

同一个命令也支持 `#+begin_...` / `#+end_...` 块。point 在 block 内时，会在结束行后插入 dedicated target：

```org
#+begin_src emacs-lisp
(message "hello")
#+end_src
<<src-20260428T122000>>
```

然后复制：

```org
[[src-20260428T122000]]
```

### TOC 各级别

这套配置的自动 overview TOC 由 [lisp/org/init-org-core.el](../lisp/org/init-org-core.el) 生成，TOC 项目前默认是指向标题的 fuzzy link：

```org
[[*一级标题][一级标题]]
[[*二级标题][二级标题]]
```

所以“引用 TOC 的二级/三级条目”本质上就是引用对应 heading：

```org
[[*二级标题][二级标题]]
[[id:20260428T121000-some-section][二级标题]]
```

临时目录跳转用标题链接即可。需要长期稳定、跨文件、被 roam 追踪时，给那个 heading 加 `ID`，然后用 `id:` 链接。

如果某个标题不想进入自动 TOC，可以给它加 `:no_toc:` tag。

### 同文件公式

公式本身不是 Org heading。要在 Org 里点击跳转，给公式后面放一个 dedicated target：

```org
\[
E = mc^2
\]     <<eq-einstein>>
```

同文件引用：

```org
见 [[eq-einstein][质能方程]]
```

这种方式适合轻量公式、临时公式和同文件跳转。`H-o o` / `C-c n o` 只创建和复用这种公式 target。

### 跨文件公式

跨文件引用公式 dedicated target：

```org
[[file:physics.org::<<eq-einstein>>][质能方程]]
```

对长期笔记，更推荐把重要公式包在一个 heading 下面，并给 heading 创建 `ID`：

```org
* 质能方程
:PROPERTIES:
:ID: 20260428T122000-einstein-equation
:END:

\begin{equation}
E = mc^2
\end{equation}
```

然后跨文件引用：

```org
[[id:20260428T122000-einstein-equation][质能方程]]
```

这个模型比较适合 org-roam：公式对应的是“一个可引用的知识节点”，而不是只在 LaTeX 导出阶段存在的编号。

### 选择规则

- 临时同文件标题跳转：`[[*标题]]`
- 稳定同文件标题跳转：`[[#custom-id]]`
- 稳定跨文件标题跳转：`[[id:...]]`
- 临时公式跳转：`<<eq-name>>` + `[[eq-name]]`
- 跨文件公式跳转：`[[file:xxx.org::<<eq-name>>]]`
- 长期重要公式：放进带 `ID` 的 heading，用 `[[id:...]]`

### Snippets

`org-mode` 里新增了几个手写引用用的 snippets：

- `orgid`
  插入 `:PROPERTIES:` / `:ID:` drawer
- `otarget`
  插入 `<<target>>`
- `eqtarget`
  插入带 target 的 `\\[...\\]` 公式块
- `blocktarget`
  插入带 target 的 `#+begin_...` block
- `idlink`
  插入 `[[id:...]]`
- `tlink`
  插入 `[[target]]`
- `ftlink`
  插入 `[[file:xxx.org::target]]`
- `feqlink`
  插入 `[[file:xxx.org::<<eq-id>>]]`
- `hlink`
  插入 `[[*标题]]`

## 10. 学术写作

### LaTeX

- Org 里启用了 `cdlatex`
- 可见区域 LaTeX 预览走自定义 `xelatex + dvisvgm` 异步 SVG 流程：当前窗口优先入队，窗口下方会比上方预取更多行，滚动产生的旧可视区域队列和旧自动渲染进程会自动回收
- 正在编辑的公式预渲染走独立 cache-only 管线：它不会被可视区域回收取消，必要时会让自动可视渲染让出并发槽；离开公式后只挂缓存或等待同一个预渲染结果，短延迟复查并补挂 overlay
- LaTeX 预览的滚动触发有短 debounce，长滚动中会低频补偿刷新；当前可视区域有未缓存公式时，会让旧的自动预取渲染先让出并发槽
- 与 `RaTeX` 共存时，公式编辑期间会忽略预览 UI 引发的合成滚动事件，避免可视区域预览被误触发
- `org-fragtog` 已不再默认常驻；公式进出编辑状态由自定义异步预览管线处理
- 这些 Org 性能优化不会降频 LaTeX overlay 的挂载路径；视觉装饰层和 roam 侧边栏可以合并刷新，公式预览 overlay 仍按可视区域优先尽快挂上
- AUCTeX 和 `pdf-tools` 已配置
- AUCTeX 支持 `latexmk -pvc` 实时编译和 Sioyek 双向同步

### 引用

- `org-ref`
- `bibtex-completion`
- Zotero / MarginNote 自定义链接

### PDF

- `pdf-tools` 已启用
- AUCTeX 查看器走 Sioyek

## 11. 如果 Org 看起来不对

优先检查：

1. 字体是否齐全
2. `~/HC/Org/` 是否存在
3. `xelatex` / `dvisvgm` 是否可用
4. [tools/org-xdvisvgm-hires](../tools/org-xdvisvgm-hires) 是否有执行权限

## 12. 想改 Org 的入口

看 [settings-cookbook.md](settings-cookbook.md)：

- 改 Org 根目录
- 改 capture 模板
- 改 agenda / TODO / refile 策略
- 改 org-roam 模板
- 改 heavy UI
