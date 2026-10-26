# neo-emacs 清单：按模块和功能分类

上游 `ad8d42b`。neo-emacs 的每一项功能都在这里有一个去处，按 Doom 分类排列，目标是本配置的 `lisp/分类/模块/`。

处理方式：

- **吸收**：本配置没有，直接加进目标模块。
- **合并**：本配置已有同类实现，逐条比较后把更好的细节并进来。
- **换实现**：功能保留，但他的做法违反本配置的硬约束（lsp-mode 唯一、Company、Portal/Ghostel、flymake、Remote-first、`etc/` 存值），在本配置的机制上重写。
- **待定**：会改变用户习惯或需要外部条件，等你决定。
- **不收**：写明理由。

“启用”指他 `init.el` 里打开的模块，“未启用”的模块（modeline、windows、alpha、addspace、eredis、telega、java）他自己也没在用。

## 0. 框架层（第一阶段已解决）

| 他的 | 去处 | 处理 |
| --- | --- | --- |
| `doom!` 声明、`+flag`、`modulep!` | `aaron!`（`lisp/init-modules.el`） | 已有 |
| `packages.el`（`package!`，straight） | 模块内 `use-package :ensure t`；GitHub 源走 `aaron-package-register-vc` | 换实现：每个包随它的模块迁 |
| `map!`、`after!`、`setq-hook!`、`defadvice!` | `aaron-keybinds` / `aaron-lib` 同名宏 | 照抄 |
| Doom popup 规则 | `set-popup-rule!` | 照抄 |
| 模块 `readme.org` | 模块 `README.md` + `docs/` | 已有 |
| `config.el` 私有胶水（724 行）、`keymap.el` | 拆进下面各类 | 不保留形式 |

## 1. 键位（`:editor evil`）——最大的待定项

他的键散在 `config.el`、`keymap.el` 和各模块里。按前缀分组：

| 组 | 键 | 命令 |
| --- | --- | --- |
| `;` 前缀（normal/visual） | `; x` `; w` `; b` `; q` `; f` `; e` `; c` `; r` `; t` `; i` `; d` `; g` `; a` | M-x、保存、切 buffer、ace-window、dirvish 开关、expand-region、注释行、string-inflection、翻译、eww、agent 面板、行尾/行首 |
| 单键 | `f` `L` `J` `K` `C-j` `C-k` `M-j` `M-k` `g l` | avy 跳字符、join、左/右切 workspace、半屏滚动、上下拖行、ialign |
| insert | `C-h` 删除（并 `keyboard-translate C-h`）、`TAB` yas-expand、`s-y` yank-from-kill-ring、`C-i` consult-yank-pop | |
| leader | `SPC f w/n/g/j`、`SPC t n`、`SPC o t/e/w`、`SPC z`、`SPC v c/v`、`SPC r r`、`SPC c I`、`SPC e c/l`、`SPC d a/c` | ace-window、复制 文件:行、复制文件到剪贴板、Java 类跳转、新 workspace、终端、elfeed、eww、consult-find、ripgrep、项目终端、quickrun、实现跳转、SQL 连接、agent 会话 |
| dired / magit | `h` `l` `c` `J` `K` `TAB` | 上级、进入、新建文件、切 workspace、subtree |

处理：**已定：以本配置现有的键为准。** 他的键只在空位上考虑，各模块审计时逐个列出。

- `J/K`、`C-j/C-k`、`f`、`C-h` 会覆盖 Evil 原生键，因此不收。
- dired 的 `h/l/c` 和 Dirvish 那张跟 yazi 对齐的键表冲突，因此不收。

## 2. `:ui`

| 功能 | 他的位置 | 目标 | 处理 |
| --- | --- | --- | --- |
| 字体：JetBrains Mono 17、CJK 方正悠宋 20，big-font 时 CJK 同步放大 | config.el | `ui/theme` | 机制**吸收**（CJK 字号联动），具体字体**待定** |
| 按 major mode 换字体（python→Fira Code、java→Noto Sans Mono、minibuffer→M PLUS Code） | config.el | `ui/theme` | **待定** |
| 行号 face 斜体、当前行号随 hl-line 背景 | config.el | `ui/theme` | **吸收**候选 |
| 主题 tokyonight-night；换主题后统一 org block/table 背景、去 help-key-binding 边框 | config.el | `ui/theme`（aaron-ui） | 主题**待定**；“换主题后修 face”的做法**吸收** |
| window-divider 底边宽度 0（去 modeline 下横线） | config.el | `ui/windows` | **合并**比较 |
| diff-hl 竖线位图加高（CJK/org 高行之间断裂），三色 foreground 可见 | config.el | `tools/git` | **吸收**：中文行高下同样会断 |
| modelinexp（1057 行）：powerline 箭头段、按相邻段色差自动配色、modeline 透明度、去掉 modeline 鼠标行为、Java buffer 段、big-font 联动 | modelinexp | `ui/theme`（doom-modeline） | 配色算法、去鼠标**吸收**候选；powerline 外观**待定** |
| modelinexp 里的微信未读数、护眼计时（AppleScript + launchd plist） | modelinexp | — | **不收**：每秒轮询外部脚本（`run-with-timer 0 1`），违反资源规则；要做的话换成事件源 |
| awesome-tray（未启用） | modeline | — | **不收** |
| ultra-scroll 平滑滚动、neoscroll、pulsing-cursor | config.el / packages | `ui/scrollview` | **合并**比较 |
| 毛玻璃 lr-macos-glass、transwin 透明（未启用）、avy 标签不透明 | lisp/、alpha、config.el | 新 `ui/glass` | **待定**：依赖他打过 `ns-glass-effect`、`ns-opaque-background-faces` 补丁的 emacs-plus；要做就在 `os/neomacs` 兼容层判断能力是否存在 |
| popwin 右侧布局（未启用） | windows | `ui/popup` | **不收** popwin；想要“*Messages*/*Help* 放右侧”就写成 `set-popup-rule!` |
| dashboard 去 footer、logo | common | `ui/theme` | **待定**（外观） |
| Doom `indent-guides` `ophints` `vi-tilde-fringe` | init.el | `ui/*` | **待定**：本配置没有这三项 |

## 3. `:editor`

| 功能 | 他的位置 | 目标 | 处理 |
| --- | --- | --- | --- |
| drag-stuff 上下拖行 | config.el | `editor/editing`（已有 move-text） | **合并**，不并存两个包 |
| ialign 交互对齐 | config.el | `editor/editing` | **吸收** |
| 复制 `文件:行` / `文件:起,止`、复制文件本身到剪贴板（osascript） | common | `emacs/files`、`os/macos-tools` | **合并**进已有的 `+copy-current-filename`；“复制文件本身”**吸收** |
| yank 路径用绝对路径 | config.el | `emacs/files` | **合并**比较 |
| 中英文间自动加空格（未启用） | addspace | 新 `:input` 模块 | **待定** |
| undo-tree 持久历史、静默加载失败 | common | `aaron-base` | **合并**（已有） |
| 片段：java 17、nxml 12、org 28、sql 21、sh 3、go 2、vue 2、yaml 2、python 1、gitconfig 1 | snippets/ | `editor/snippets` | **合并**：同名逐个比较 |
| file-templates、fold、whitespace +trim | Doom | 已有同名模块 | 已有 |

## 4. `:input`（新分类，Doom 有）

| 功能 | 他的位置 | 目标 | 处理 |
| --- | --- | --- | --- |
| Rime：通过鼠须管 CLI 切 ascii。进入 insert 时按前一个字符是否中文决定中英，离开 insert、进 minibuffer 和获得焦点时切英文 | rime | 新 `input/rime` | **吸收**（前提是你用鼠须管） |
| emt：macOS 原生中文分词，按词移动 | packages + libEMT dylib | `input/rime` 或 `editor` | **吸收**候选；dylib 不进 git |

## 5. `:emacs`

| 功能 | 他的位置 | 目标 | 处理 |
| --- | --- | --- | --- |
| dired：`dired-auto-revert-buffer nil`、`dired-recursive-copies never` | find | `emacs/dired` | **合并**比较 |
| dirvish：`TAB` subtree、`; f` 开关 | find | `emacs/dirvish` | **待定**（键表与 yazi 对齐） |
| recentf 保留远程条目且不探测；tramp-recentf-cleanup 置空 | config.el | `remote/framework` | **换实现**：按 backend 声明的代价判断，不用 `file-remote-p` |
| 退出：已修改的远程 buffer 逐个问 保存/放弃/取消；退出期间关 diff-hl/flycheck、TRAMP 超时 5s、关 VC | config.el | `remote/framework` + `config/default` | **换实现**：走 `remote-workspace-disconnect-target` 和不可达链路的有界尝试；先对照 remote-parity 看是否已覆盖 |
| bookmark 文件放进 org-roam、bookmark+ | find、packages | `ui/session` | 路径**不收**（私有）；bookmark+ **待定** |
| magit 同窗显示（diff 除外）、magit-delta | git | `tools/git` | **合并**比较 |
| with-editor 的 emacsclient 路径 | common | `config/server` | **合并**比较 |

## 6. `:term`

| 功能 | 他的位置 | 目标 | 处理 |
| --- | --- | --- | --- |
| vterm：按“目录/文件”命名并复用终端、光标随 evil 状态变 bar/box、窗口 fringe/margin 清零、隐藏 modeline、滚动时自动进 copy-mode、escape 不退出 | shell、config.el | `term/portal`、`term/shell`（Ghostel） | **换实现**：本配置不用 vterm，逐条看 Portal/Ghostel 是否缺 |
| vterm 专用 fontset（Powerline 区间强制 JetBrains Mono） | config.el | `term/shell` | **换实现**候选 |
| eshell：隐藏 modeline、`\033[G` 转 `\r`、`C-j/C-k` 滚动、quickrun 中断修复、切到 eshell 自动 normal 并独占窗口 | shell、config.el | `term/shell` | **待定**：先确认你是否用 eshell |
| fish 作 shell、`JAVA_*_HOME`/`MAVEN_HOME`/代理 `setenv` | shell | Remote 环境胶囊 / `etc/` | **不收**形式；工具链走胶囊 |

## 7. `:checkers`

| 功能 | 他的位置 | 目标 | 处理 |
| --- | --- | --- | --- |
| flycheck 在 org/clutch 模式不开 | config.el | `checkers/diagnostics` | **换实现**：flymake 下检查是否也有同样的空 checker 刷屏 |
| `flymake-no-changes-timeout 30` | eglot | `checkers/diagnostics` | **合并**比较 |

## 8. `:tools`

| 功能 | 他的位置 | 目标 | 处理 |
| --- | --- | --- | --- |
| eglot + JDT：`jdt://` 反编译 URI 处理和缓存、Maven settings、Java home、`java.apply.workspaceEdit`、反编译文件进 java-ts-mode、去掉补全里重复的注解 | eglot | `lang/java` + `tools/lsp` | **换实现**到 lsp-java；逐条看 lsp-java 是否已有 |
| lsp-java `execute-command` 包装（未启用） | java | `lang/java` | **合并**比较 |
| corfu + kind-icon 外观 | eglot | `tools/lsp`（Company） | **换实现**：只映射外观参数 |
| tramp-rpc：release 部署策略、本地二进制缓存目录 | config.el | `remote/backend` | **合并**比较 |
| agent-shell 面板（683 行）：右侧面板、独立输入窗（compose）、`; d` 开关和最大化、opencode/Cursor ACP 会话、禁止下载远程图片 | agentshell | `tools/agent` + Noema agent | **待定**：本配置已有 Noema/agent-shell 入口，只吸收缺的交互，不另开一套 |
| clutch：超时与行数上限、`.sql`/`.redis` 关联、执行 XML 标签内的 SQL（去注释）、结果/记录视图键 | sql | `tools/clutch` | **合并** |
| ejc-sql（db-work.el）、eredis（未启用） | neoemacs/、eredis | — | **不收**：clutch 已取代 |
| verb（org 里写 REST 请求） | org、packages | 新 `lang/rest` 或 `lang/org` | **吸收**候选 |
| Java 栈帧跳转（光标处 `Class:line` 跳源码）、MyBatis Mapper 方法跳 XML | find、eglot | `lang/java` | **吸收**；查找走 `remote-*` 进程 API |
| projectile 根用 `git rev-parse`（按 `file-remote-p` 分支） | find | `tools/project` | **不收**：违反 Remote 规则，且本配置有自己的根解析 |
| keyfreq 按键频率统计 | common | 新 `config/keyfreq` | **吸收**候选（数据放 `var/`） |
| gc、`read-process-output-max`、递归深度、`font-lock-maximum-size` | common | `aaron-base` / config 注册表 | **合并**比较（已有 gcmh 和 LSP 性能项） |

## 9. `:lang`

| 功能 | 他的位置 | 目标 | 处理 |
| --- | --- | --- | --- |
| web/vue：缩进 2、不用 tab、关 electric-indent、web/js 开 rainbow-delimiters、Volar、禁 deno、`lsp-idle-delay 0.5` | frontend | `lang/web`、`lang/javascript` | **合并**；服务器走 `set-language-server!` |
| go/python/elisp 的 mode hook | eglot | `lang/*` | **合并**比较 |
| Org：TODO 关键字和 face（TODO DOING BLOCK TEST DONE REPORT）、时间戳格式、log done | org | `lang/org` | **待定**：Org 在本配置不是主笔记（Noema 是） |
| Org：org-modern、org-modern-indent、org-appear、focus、RET 只在表格/链接生效、element cache | org | `lang/org` | **合并**比较 |
| Org：plantuml/mermaid/dot 路径、ob-mermaid、HTML 导出图片和 MathJax、LaTeX 导出、org-roam-ui、capture 模板 | org | `lang/org` | 功能**合并**；硬编码路径**不收**，改 `etc/`；org-roam-ui **不收**（Noema 有图谱） |
| Java（未启用）：lombok jar、Eclipse/IntelliJ/p3c codestyle、dap、org-babel java 编辑准备、`\uXXXX` 转换 | java、neoemacs/ | `lang/java` | **合并**比较；jar 和 codestyle 不进 git |
| xml-format | eglot | `lang/web` | **吸收**候选 |
| d2-mode、gnuplot、yaml、docker-compose | packages | `lang/filetypes` | **吸收**候选 |
| Doom 语言：go、ruby、swift、rest、plantuml、yaml | init.el | — | **待定**：本配置没有，按你是否写这些语言决定 |

## 10. `:app`

| 功能 | 他的位置 | 目标 | 处理 |
| --- | --- | --- | --- |
| 翻译（gt + 有道；单词弹窗、句子插入） | translate | 新 `app/translate` | **吸收**候选 |
| elfeed RSS（12 个订阅） | elfeed | 新 `app/rss` | **吸收**候选；订阅列表放 `etc/` |
| telega（未启用） | telega | 新 `app/telegram` | **待定** |
| emms（Doom） | init.el | — | **待定** |
| eww 键 | elfeed | `app/browser` | **合并**（已有） |

## 11. `:os macos`

| 功能 | 他的位置 | 目标 | 处理 |
| --- | --- | --- | --- |
| 按屏幕分辨率（1k/2k/4k）设置窗口位置和大小 | config.el | `os/macos` | **待定**：和 `config/background` 的 frame 策略一起看 |
| 去标题栏代理图标、清空标题 | config.el | `os/macos` | **待定**（外观） |
| 启动后用 AppleScript 反复抢焦点 | config.el | — | **不收**：和后台 app（隐藏启动）冲突 |
| `custom-file` 指向 null-device、`enable-local-variables :safe` | config.el、common | — | **不收**：前者丢掉 Customize 存储，后者和本地变量信任策略冲突 |
| 编码全设 utf-8-unix | common | `aaron-base` | **合并**比较 |
| `kill-do-not-save-duplicates`、`save-interprogram-paste-before-kill` | config.el | `config/default` | **合并**比较 |
| `package-archives` 用清华镜像 | config.el | — | **不收**：包由 lock 管理 |
| `display-line-numbers-type nil`、`truncate-lines nil` | config.el、common | `config/default` | **待定**（默认值差异） |

## 12. 资源与补丁

| 他的 | 处理 |
| --- | --- |
| `neoemacs/font/`（汉仪新人文宋、MonoLisa、NFM、VictorMono） | **不收**进 git：字体系统安装，`config-health` 检查是否存在 |
| `lombok*.jar`、`plantuml.jar`、codestyle XML | **不收**进 git：外部依赖，路径走 `etc/` |
| `rime-macos/`、`libEMT-aarch64.dylib` | **不收**进 git：随所属模块写安装说明 |
| `tramp-rpc-binaries/` | 本配置已有部署机制 |
| `ultra-scroll/`、`org-modern-indent/` | 改为包（VC 包注册） |
| `dotsk.el`、`ds2.el`（ob 扩展）、`db-work.el` | 前两者**待定**（是否用 dot/ds2），后者**不收** |
| `patches/emacs31`：cursor-animation、ns-alpha-background、ns-glass-effect、ns-opaque-background-faces | **待定**：需要自编 emacs-plus；不编时相关功能必须安静失效 |
| `emacs-user.ahk`、`question.org`、`mermaid-config.json` | 前两者**不收**（Windows/个人），mermaid 配置随 `lang/org` |

## 13. 汇总

- **直接可做（吸收/合并，不改习惯）**：
  - diff-hl 竖线修复
  - 换主题后修 face 的机制
  - CJK 字号联动机制
  - ialign
  - 复制文件到剪贴板
  - 片段合并
  - clutch 细节
  - web/vue 缩进
  - Java 栈帧跳转和 Mapper→XML 跳转
  - Org 显示细节比较
  - dired 两项设置
  - keyfreq
  - Rime 自动切换（若用鼠须管）
- **换实现**：
  - JDT/eglot 细节转到 lsp-java
  - vterm 细节转到 Portal/Ghostel
  - 远程退出流程和 recentf 转到 Remote 框架
  - flycheck 规则转到 flymake
  - corfu 外观转到 Company
- **待你决定**：
  1. 键位总原则（§1）
  2. 主题和字体（§2）
  3. 毛玻璃和自编 Emacs 补丁（§2、§12）
  4. modeline 外观
  5. Org 的范围（§9）
  6. 新 `:app` 模块要哪些（翻译、RSS、Telegram）
  7. `:input` 分类（Rime、emt、中英空格）
  8. 他启用而本配置没有的 Doom 模块和语言
- **不收**：
  - AppleScript 轮询和抢焦点
  - ejc-sql 和 eredis
  - popwin 和 awesome-tray
  - 镜像源
  - `custom-file` 指向 null-device，`enable-local-variables :safe`
  - 按 `file-remote-p` 分支的项目根
  - 所有硬编码的私人路径和代理
