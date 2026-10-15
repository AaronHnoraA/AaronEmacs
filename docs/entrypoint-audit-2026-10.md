# Emacs 入口与生命周期审计（2026-10）

这是一份逐步整合的工作记录。只记录已核对的入口和已验证的问题；后续每审计一个领域，再把结果补到这里。

## 当前入口图

| 用途 | 用户入口 | 所属实现 | 当前状态 |
| --- | --- | --- | --- |
| 启动 | `early-init.el` → `init.el` → `lisp/init-modules.el` | 启动模块表 | 模块顺序有单一来源 |
| 找命令 | `M-x`、`SPC`、`H-,` | amx、Evil leader、Hyper transient | `command-hub` 与 `keybinding-guide` 提供可搜索名称 |
| 改设置 | `config-board`、`SPC h c` | `site-lisp/config/` | 已有统一设置入口 |
| 看健康 | `config-health`、`config-health-menu`、`SPC h H` | `lisp/init-health.el` | 本轮修复了刷新阻塞与渲染错误 |
| 编译配置 | `config-compile`、`config-compile-menu`、`SPC c b` | `lisp/init-compile.el` | 本机配置编译、清理和板面固定本机上下文 |
| 后台工作 | `activity-board`、mode line 指示器 | `lisp/init-activity.el` | 定时器、异步取消归属及 provider 失败隔离已复核 |
| 项目和远端 | `project-workbench`、`remote-board` | `lisp/init-project.el`、`lisp/remote/` | 切换、取消、离开与 workspace 关闭已有生命周期测试 |
| Noema / Jupyter | `noema-menu`、`noema-wiki-menu`、`noema-ai-menu`、`noema-wiki-home`、`noema-agenda`、`noema-agenda-web`、`jupyter-board` | `lisp/roam/`、`lisp/init-jupyter-board.el` | 主菜单、Wiki、AI、Agenda 与首页可按用途搜索；刷新和清理分离，未确认关闭保留运行时供重试 |
| 外部启动与动作 | `bin/emacs-app` → Raycast 白名单 | `lisp/init-background.el` | 客户端调用有 4 秒超时，动作只取白名单；接收失败回传 Raycast，延迟执行失败在 Emacs 中显示 |

## 与成熟配置的结构对照

目前按 [casouri/lunarymacs](https://github.com/casouri/lunarymacs) 作为用户提到的参考项目；若实际是别的仓库，应重新核对这里的对照。这些项目的源码和官方文档只用于检查入口设计，不直接替换当前的 `package.el` / `use-package` 或 Remote 生命周期。

| 参考 | 已核对的做法 | 在本配置中的落实 |
| --- | --- | --- |
| [Lunarymacs](https://github.com/casouri/lunarymacs/blob/master/init.el) | `init.el` 按领域加载 `star/*.el`，[`star/key.el`](https://github.com/casouri/lunarymacs/blob/master/star/key.el) 集中全局键位，`cheatsheet-display` 按 major mode 找单独的速查文件。 | `init-modules.el`、`init-evil.el` 已有加载和键位边界。新增 `mode-keybindings` 展示当前实际键位；总索引保留 `keybinding-guide`，无需为每个 mode 维护另一份易过期文本。 |
| [Doom Emacs](https://github.com/doomemacs/core/blob/master/docs/getting_started.org) | 模块区分早期 `init.el`、运行期 `config.el`、依赖 `packages.el`，有模块和包的就近帮助。 | 用现有 `early-init.el` / `init-modules.el` / 领域模块和 `config` 注册表表达阶段；健康板与编译板承担诊断。后续继续核对模块的帮助和依赖可见性。 |
| [Spacemacs](https://www.spacemacs.org/doc/DOCUMENTATION.html) | Leader 分组由 which-key 命名，major mode 有局部 leader 和实际键位浏览。 | 保留已有 `SPC` 分组、`my/local-leader!`；`SPC h m` 直接调用 which-key 的当前 major mode 键位视图，避免只靠长文档搜索。 |
| [Prelude](https://prelude.emacsredux.com/en/latest/configuration/) | 启动欢迎页指向配置、模块和手册；预加载、核心和个人配置有明确先后。 | Dashboard、设置 cookbook 和模块表已有对应入口；继续核对 Dashboard 的文档直达项与实际加载阶段。 |

## 本轮逐入口核对

| 入口层 | 实际路径 | 核对结果与处理 |
| --- | --- | --- |
| 启动 | `early-init.el` → `init.el` → `init-modules.el` | 必需模块按表加载；可选模块登记后在健康板区分已加载、等待触发与失败。设置菜单所需的 `config-tools` 是必需模块，加载失败时不留下指向缺失命令的按键。 |
| 设置 | `config-board` / `config-dispatch` → `config-set`、`config-save`、`config-refresh-store-files` | 主 store 的打开入口使用本机配置文件上下文；保存失败直接报错并阻止刷新丢掉未保存值，刷新先完整解析所有 store 再替换内存；`config-check` 显示写入与解析问题。 |
| `M-x` | amx + Emacs 命令历史 | 保留内部 helper 的 `my/` 名称；高频管理入口有 `config-*`、`project-workbench`、`activity-board`、`jupyter-board`、`command-hub`、`keybinding-guide` 和 `mode-keybindings` 等用途优先名称。顶层 Transient 菜单直接以可搜索名称定义，键位、面板和嵌套菜单指向同一命令；未批量为 helper 造别名。 |
| Leader `SPC h` | `init-evil.el`，健康菜单在 `init-health.el` | 原来 `SPC h c` 在两个模块里分别指向 `helpful-command` 和设置菜单，后加载者覆盖前者。现在 `h c` 固定为设置，`h C` 是 `helpful-command`，`h H` 是健康菜单，`h K` 是总指南，`h m` 是当前 major mode 的实际键位。 |
| Leader `SPC c` | 各代码工具的领域模块 | 原来 `c ?` 诊断菜单被末尾加载的编译菜单覆盖，`c s` workspace symbol 被语言服务器菜单覆盖。现在 `c ?` 诊断、`c B` 编译、`c s` 语言服务器、`c S` workspace symbol。 |
| Leader `SPC r` / `SPC N` | 运行、维护、Noema 模块 | 原来 `r R` 的重跑被快照恢复覆盖，`r d` 调试、`r o` 输出被 Noema 全局键覆盖。现在 `r R` 重跑、`r V` 恢复快照、`r d` 调试、`r o` 输出；Noema 直达键在独立的 `SPC N` 组，`SPC n` 继续用于结构导航。 |
| 其他 Leader 重复项 | `init-evil.el` 与各领域模块 | 删除基础表中已被后续模块覆盖的 `SPC SPC`、`SPC b x`、`SPC p x` 与重复的 `SPC c j` 声明，也去掉 smerge 和项目局部模块重复声明的 `g`、`p` 分组标题。最终动作仍分别是 Telescope、普通 scratch、项目 scratch、调试菜单；`SPC g m`、`SPC p l` 保留。关闭 buffer+window 仍有 `SPC w x`，移除已知项目仍在项目菜单中。 |
| Emacs Lisp 局部 Leader `SPC m` | `init-editing-workbench.el` | 原来 `init-evil.el` 先声明内建 `eval-defun` / `eval-last-sexp`，后加载的编辑模块在同一位置覆盖为 Eros 命令。现在由编辑模块统一拥有 `i`、`e`、`g` 下的全部局部键，运行时 `SPC m e d/e e` 均指向 Eros。 |
| Hyper `H-` | `init-macos.el` → `init-hyper.el` | `H-,` 指向主菜单，`M-x command-hub` 可按名字找到同一菜单；全局绑定改用项目约定的 `general-define-key`。补上文档中曾声称存在的 `H-p` 项目菜单与 `H-a` Noema 日程，移除不存在的 `H-l`、`H-y`、`H-?` 说明及 Hyper 菜单 `J` 项。主菜单的 `f`/`i` 现在也能找到文件/编辑菜单。 |
| 功能键 | `init-function-keys.el` | `F1`–`F8`、`F10` 改走 `general-define-key`；给原先文档声称却未绑定的 `F9` 补上 Noema 日程，删除已退役的 `F12` Claude 菜单说明。 |
| Dashboard | `init-ui.el` | 导航按钮委托给设置板、Noema、性能监控和升级命令，未另建业务实现；异步 Agenda 卡片有已有的缓存/请求合并测试。 |
| 板面 | 健康、编译、活动、Jupyter 各自模块 | 健康与编译刷新只展示或读本地状态；编译板在本机目录与环境中创建，直接从其他 mode 调刷新会报错。活动板打开时触发一次 provider poll，自动刷新默认关闭且关闭 buffer 后取消 timer；Jupyter 刷新异步请求，generation 防止过期回调改写板面，连接文件清理改为 `s` 显式动作或本机定时维护。重复打开不重复发起未完成请求；Remote 明确断连清空当前快照并让旧回调失效。 |
| 外部动作 | `bin/emacs-app` → `my/background-actions` | `emacsclient` 每次调用有超时，动作只从白名单取；非数字或越界动作 ID 会报错。frame 操作也在排队前验证操作名与 frame ID。Raycast 用短期请求编号查询排队后的成功或失败；超过时限的动作仍由 Emacs 报告。Noema 搜索走 Noema host，不经过 Emacs 命令白名单。 |

这张表按入口类别覆盖启动、命令搜索、三套按键、Dashboard、板面和外部调用。领域内部命令还有很多，后续按领域核对其失败提示与资源收尾。

在完整配置的批处理启动后，用 `commandp` 遍历 symbol table，测得约 1,033 个 `my/` 交互命令。这个数包括按需加载状态下已定义的命令，后续加载包可能改变它；也不等于每个命令都应该单独显示在 `M-x`。目前 `my/` 同时承担内部命名空间和用户入口名，直接全局改名会牵动快捷键、autoload、Transient、测试和保存的命令历史。

### 项目动作与 Emacs 原生命令

对照 [GNU Emacs 项目文件命令](https://www.gnu.org/software/emacs/manual/html_node/emacs/Project-File-Commands.html)、[项目命令总览](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html) 和本机 Emacs 31 的 `project.el` 源码。以下保留包装的理由来自当前配置的行为，不表示原生命令不能独立使用；原生 `C-x p` 键仍在。

| 工作台动作 | 原生对应 | 当前差异与处理 |
| --- | --- | --- |
| 找文件、切 buffer | `project-find-file`、`project-switch-to-buffer` | 工作台使用 Projectile 的项目文件与 buffer 归属，并在导航成功后发出统一的项目激活事件；原生命令继续独立可用。 |
| 项目搜索 | `project-find-regexp`、`project-search` | 原生搜索返回 Xref 或逐项匹配；工作台使用 Consult 交互搜索，并根据目标端是否有 `rg` 选择 `ripgrep` 或 `grep`。 |
| 打开项目根 | `project-dired` | 工作台包装在 Dired 打开成功后激活项目；不增加第二个文件管理面板。 |
| 关闭文件 buffer | `project-kill-buffers` | 原生默认条件还包括 Dired、编译等 buffer；当前 Projectile 策略是 `kill-only-files`。工作台菜单改为准确的“close project files”。 |
| 最近文件 | Emacs `recentf`、Projectile `projectile-recentf` | 原 Projectile 路径对项目根调用 `file-truename`，远端候选列表可能因此访问目标。工作台改为直接筛原生 `recentf-list`，保留 Projectile 的补全 UI 和导航成功后的激活事件。 |

## 本轮已处理

1. `my/health-report` 原来在打开和每次 `g` 刷新时同步运行 startup、byte、native 三项检查；其中 byte 检查会启动新的 Emacs 并编译配置。现在面板只读当前状态，耗时检查由按钮或 `s` / `c` / `N` 显式运行，结果保存在当前会话并回写面板。
2. 健康面板将字符串类型的可执行程序名传给 `symbol-name`，导致打开时出错；现在能渲染字符串或符号名称。
3. 健康报告的“Commands”检查改用 `commandp`，只把真正可从 `M-x` 调用的命令标为可用。
4. 在原有模块中添加 `config-health` / `config-compile` 及各自菜单名称，并给项目、活动、Jupyter 管理板补上 `project-workbench`、`activity-board`、`jupyter-board`。旧 `my/` 名称继续供内部调用和既有快捷键使用，避免一次性重命名造成行为变化。
5. 从远端 buffer 打开健康面板或启动批处理检查时，固定使用 Emacs 本机的目录、进程环境和 `exec-path`，避免把目标机器的 PATH 当成本机依赖状态。
6. 活动板自动刷新定时器在 buffer 被关闭时通常由 kill hook 取消；如果回调仍在队列中，它原先会从当前 buffer 读取已不存在的 buffer-local 句柄。现在回调持有自己的定时器句柄，死 buffer 分支也能准确取消它。
7. `my/reload-init` 现在明确只重读 `init.el`，缺文件或加载失败会正常报错；完成消息也说明已加载模块不会重执行。修改模块后应使用现有重启入口。
8. 顶层领域菜单增加 `project-menu`、`workspace-menu`、`project-run-menu`、`task-menu`、`test-menu`、`git-menu`、`language-server-menu`、`debug-menu`、`file-menu`、`editing-menu` 和 `jupyter-board-menu`。实现仍由各自领域模块拥有；主命令菜单补上文件与编辑入口。
9. Jupyter Board 再次打开时若 provider 尚未完成，不再启动相同的三路发现。Remote target 明确断开时清空板上旧数据并让异步回调失效；清理完成回调只刷新发起时的 target 与面板代次，不能在切换或断连后重新触发发现。
10. Raycast 动作及 frame 操作现在在关闭窗口前等待启动器的有界接收确认。启动器转发 `emacsclient` 的错误输出；frame 名称和操作先验证。动作进入延迟执行后的结果尚无回传协议，后续错误继续在 Emacs 消息区报告。
11. 模式键位帮助由 which-key 读取当前生效的 keymap，`M-x mode-keybindings` / `SPC h m` 可直接打开。`init-open.el` 的本机文档入口固定配置目录、环境和文件位置；快捷键总指南、编译板、语言服务器板与 Appine README 都经它打开，从 Remote buffer 进入时不会把本机文档路径解释到目标机。只有总指南要求只读视图。
12. 静态读取 `lisp/` 中 330 条 `my/leader!` 全局键声明，当前没有重复键；另查出 Emacs Lisp 局部 leader 的 `ed` / `ee` 在两个模块重复声明，已合并到 `init-editing-workbench.el`。全局清单只覆盖这套声明语法，不能代替运行时键位检查或其他 package keymap 的审计。
13. 编译板在 Remote buffer 中打开时曾可能继承目标机目录和进程环境，配置级命令也可能从目标机上下文读写本机路径。现在整套配置的 byte/native 编译、缓存清理和当前文件编译中的实际文件操作固定在本机上下文；当前文件资格仍在源 buffer 判断。板面刷新必须在编译板 mode 内执行。
14. Noema 的 broker 内核是目标端脱离 Emacs 的进程。原关闭路径先注销运行时，再忽略 `TERM` 的错误；失联时会丢掉唯一可重试的进程身份。现在仅在目标确认进程已不存在或接受信号后注销；失败时保留注册表项并标记 `shutdown-pending`，Jupyter 板面用警示色显示，合并 Noema 任务时也不覆盖这个状态。批量关闭继续处理其他内核，保留失败项。启动流程在注册 workspace 资源失败后还会移除已登记的自动恢复项；Remote 转发组自身也在登记失败时回滚全局表和可能留下的 workspace 资源。进程已接收 `TERM` 后是否真正退出仍需目标端探测，当前关闭协议尚不等待进程退出。
15. Raycast 的动作和 frame 请求现在保留 `emacsclient` 标准错误，接收阶段若失败可看到实际错误。frame 在验证与延迟执行之间被关闭时，Emacs 会提示失败；其他延迟 frame 错误也统一在 Emacs 中显示。Raycast 的启动/重启/退出类命令仍独立于视图运行，但现在等待子进程确实启动才关闭 Raycast；脚本不存在会报告错误，不会触发未处理的子进程异常。启动成功不代表这些长时动作最终成功。
16. Noema 主菜单、Wiki 菜单、AI 菜单和 Wiki 首页原来只暴露 `my/` 名称；新增 `M-x noema-menu`、`noema-wiki-menu`、`noema-ai-menu` 与 `noema-wiki-home`，保留内部名称与既有键位。四项在完整批处理启动后均通过 `commandp` 核验。
17. Activity 板的“停止”原先在取消回调返回后立即注销活动，异步取消仍在运行时会让任务从计数中消失。现在只通知所有者，由所有者在结束时注销。轮询或快照 provider 抛错时会记录一次警告并在板面的“Provider problems”区显示；其他 provider 继续执行，恢复后错误自动清除。定时扫描即使遇到未预料的异常也会重新安排下一次扫描。`make activity-test` 原缺 Remote load-path，现可直接运行且 16 项通过。
18. 启动健康板原先在没有可选模块失败记录时直接写“optional modules loaded”，会把等 Org、TeX 等触发的延迟模块误报成已加载。`init-modules.el` 现在登记所有可选模块和延迟触发条件；健康板逐项展示 loaded、waiting for 触发项或失败原因。失败后重试成功会清除失败记录。完整配置启动和健康板 11 项测试已通过。
19. 设置 cookbook 中还保留几个常用的 `M-x my/...`：字体重应用、LaTeX 预览诊断、活动图像重载和 Jupyter 板。前三项在所属模块提供 `font-refresh`、`latex-preview-doctor`、`activity-reload-sprites`，LaTeX 的新名称也有延迟加载入口；Jupyter 文档改用已有的 `jupyter-board`。旧命令仍可用，健康板的命令可用性测试覆盖新入口。
20. 设置板的“打开主 store”现在与其他本机文档入口共用客户端目录和环境。`config-save` 原来在 `config--persist` 写盘失败只发 warning 后仍提示“stores saved”；现在写入失败会报错并标记未保存状态，`config-check` 可见，修复后用 `config-save` 重试。此期间拒绝从磁盘刷新，以免覆盖内存里的改动。`config-refresh-store-files` 原先先清空覆盖，再读文件，文件截断或格式错误时可能只读入一部分并仍提示成功；现在在临时状态中完整解析全部文件，通过后再应用。对应的失败和恢复路径已加测试。多文件保存仍逐文件原子写入，若中途失败，已成功写入的文件不会自动回滚；保留内存状态并重试 `config-save`。
21. 设置组件中 `config-board` 在 `config-custom.el` 和 `config-tools.el` 各定义一遍，现只由 Customize 适配层提供；设置菜单继续指向同一个 Emacs 原生 Customize group。注册的专题配置文件原有 `config-file-open` / `config-file-reload` 函数却无法从 `M-x` 或设置菜单使用；现可按文件名补全、通过菜单编辑和重新加载，在 Remote buffer 中仍使用本机目录与环境，缺失文件可正常创建。健康板只引用设置模块的完整性结果并链接到同一个 `config-check`，不另造设置面板。原生 Customize 的“保存全部”适配器在 store 写入失败时可能留下半清空的 `saved-value`；恢复保护现在覆盖整个处理过程，失败后可重试。
22. 找命令路径复核了 `M-x`、`telescope`、`command-hub`、Dashboard 与模式按键帮助。`telescope` 本身是 Transient 菜单，实际文件、buffer、行和历史选择继续委托 Consult 或 Emacs 现有功能；Dashboard 的设置按钮委托原生 Customize 适配层，没有另一套设置表单。`command-hub` 原先缺少设置和按键帮助的直接入口，现在 `C` 调用同一个 `config-dispatch`，`K` 调用基于 which-key 的当前模式实际键位，`G` 打开静态完整指南。三项 Transient suffix 已在完整启动后核对。
23. 诊断入口有 Consult 快速 picker、自制持久面板和 Emacs 原生 Flymake 项目列表三层。原来自制项目面板使用固定 `*Diagnostics: Project*` buffer 名，跨项目切换会关闭上一项目的视图；它的行渲染还假定每条诊断都有存活 buffer，无法覆盖 Flymake 原生支持的未打开文件 locus。主项目入口（`SPC c P`、mode line、Telescope `D`、诊断菜单 `p`）现在复用按项目分开的原生 Flymake 列表，保留再次触发关闭当前项目窗口的手感；未打开文件的 LSP 诊断仍经既有 bridge 进入 Flymake。当前 buffer 保留有过滤和工具栏的自制面板。项目严重度子入口改用既有 Consult picker，明确只覆盖已打开文件。菜单不再同时列出两个指向同一原生列表的项目动作。LSP→Flymake bridge 的项目归属匹配补上目录边界，防止 `/proj-other` 被误收进 `/proj`。跨项目入口和边界测试已通过。
24. 项目工作流继续由 Projectile 管已知项目和文件选择、原生 `project.el` 提供项目对象、Perspective 管视图、Remote 管 target；现有封装负责激活事件和跨组件同步，有独立价值。`my/project-open-workbench` 实际只转发给 `my/project-switch`，原菜单却同时显示“switch project”和“open workbench”两个相同行为；菜单保留一个切换动作，旧 `SPC p o` 和 `project-workbench` 名称继续兼容。项目指南及每日索引原来还声称 `SPC p x` 会移除项目，但该键已用于项目 scratch；文档改为真实入口 `SPC p .` 后按 `x`。
25. 项目退出路径原用 `file-in-directory-p` 逐个判断 buffer 是否属于项目，对 `/fs:` 目标会产生 Remote metadata 操作；已有规范化逻辑身份现在直接用带目录边界的前缀判断，邻居目录不会误收。Emacs 31 的 `project-list-exclude` 已提供排除机制，删去对私有 `project--remember-dir` 的限制 advice，让原生 `project.el` 可独立记住未加入 Projectile 的项目；显式登记、清理原生列表均改用公开的 `project-remember-project`、`project-known-project-roots` 和 `project-forget-project`，移除时仍能按规范路径匹配符号链接。忽略列表改为原子写入，失败时回滚内存状态。移除项目时 `kill-buffer` 的否决会准确计入保留 buffer；某个组件清理失败不会中断其余组件，最后明确报错，移除入口可从已忽略项目中选根重试。
26. 忽略项目状态文件若截断或出现多余表单，启动原先会中断，且部分读取结果可能被覆盖。现在启动记录错误并在健康板显示文件与恢复入口，暂停自动清理项目列表，拒绝覆盖坏文件；修好后用 `project-reload-ignored-roots` 或健康板的 Reload 重新加载，成功后恢复清理，健康板立即更新。项目套件在关闭本机损坏的 native compiler trampoline 后 41/41 通过；健康板套件 13/13 通过。
27. 项目菜单的登记、批量发现和完整移除原来只能在 `M-x` 搜到 `my/` 实现名。现在有 `project-add-to-workbench`、`project-discover-in-directory`、`project-remove-from-workbench`，菜单使用相同名称，旧命令和快捷键继续有效。名称明确这是跨 Projectile、Perspective、Treemacs 与原生项目列表的工作台动作，避免与 Emacs 自带的 `project-remember-project`、`project-forget-project` 混淆。文件查找、搜索和缓冲区动作仍由现有 Projectile/Consult 与原生包执行，未增设第二个 picker。
28. 远端项目登记先打开 Remote workspace，再验证目录与项目标记。验证失败时原先会留下本次新建的 workspace；现在失败时只关闭这次新建的 workspace，已由别的功能打开的同根 workspace 保留。失败恢复路径有单独测试。
29. 补齐项目登记中途失败的补偿：先检查 Projectile 的其他排除规则，拒绝因排除而静默“登记成功”；项目忽略文件损坏时，手动登记和批量发现都先停止。进入写入阶段后，记录原有归属；后续组件报错时只撤销本次新加入的原生项目与 Projectile 记录，并恢复原有忽略状态。补偿后重新检查 owner 是否仍记得该根，未清干净会明确告警。批量发现会逐项处理；失败项从扫描新加入的 Projectile 列表撤回，其他成功项保留，最后汇总失败。扫描或逐项登记时 `C-g` 会撤回尚未完成的新条目，保留已成功的条目。各包仍各自写盘，这属于失败补偿而非跨文件原子事务。临时本地项目通过真实 Projectile 和原生 `project.el` 的写盘接口验证；项目套件 50/50 通过。真实远端和并行 Emacs 进程间的写入冲突尚未实测。
30. 对照本机 Emacs 31 的原生项目动作后，保留承担 Projectile 项目归属、Remote 路由或激活事件的包装；完整启动时 `C-x p f` 和 `C-x p k` 实际仍指向原生 `project-find-file`、`project-kill-buffers`。最近文件入口是一个真实重复且有远端代价的地方：不再调用 Projectile 的 `projectile-recentf` 来生成候选，直接从原生 `recentf-list` 筛项目文件，仍借用 Projectile 补全。目标端候选测试确认无 `file-truename` 探测且相邻目录不混入；本地文件测试确认仍以原生路径打开。`k` 菜单标签按当前 `kill-only-files` 策略改为“close project files”。项目套件 52/52 通过。
31. Tab Bar 只负责窗口布局，Perspective 负责工作区 buffer。原 `SPC t d` 在原生新建的 tab 尚未关联 Perspective 时，会按当前 Perspective 名称调用 `persp-kill`，错误删除别的工作区。切换、关闭和关闭其他 tab 改用原生 `tab-switch`、`tab-close`、`tab-close-other`；显式删除工作区仍走 workspace 菜单。新建配对 tab/workspace 的命令增加名称冲突预检。
32. Roam 新建入口：`amx` 的持久排名里留有已删的 `my/noema-roam-new-note`，其重建缓存会把失效命令再次列入候选，选择后触发 `commandp chosen-item` 断言。重建前筛除非交互命令；旧名称不恢复，新增可从 `M-x` 搜到的 `roam-new-note`。交互入口用明确参数打开新建面板，避免被程序调用层级影响。模板列表只读 Noema runtime；新增 Roam 与 regular 两组实际文件，用模板头 `type` 区分并在创建时校验，避免选中模板却静默生成空白文件。
33. Dirvish 自带的展开子目录已让 `dired-current-directory` 指向光标所在层级，但自定义 `a` 新建动作只从 `default-directory` 读字符串，跳过了原生路径补全。现在由当前条目决定目录：目录行在其内创建，文件行在其父目录创建；`read-file-name` 提供原生路径补全，保留文件名末尾 `/` 创建目录的习惯。纯 Dired 的原生命令和键位不变。
34. 同一目录规则已用于 Dirvish 的粘贴、覆盖粘贴、三种链接、终端、复制目录路径、目录跳转与两种搜索；原生 `p m` 粘贴菜单通过其公开配置项复用这些动作。`M` 文件传输菜单及弹出目录选择也取光标所在目录。跨机器粘贴仍交给已有 `remote-files` 传输队列；客户端可直接访问的同目标复制与链接调用 Dirvish 原生命令并显式传入目标目录。`m l` 与已有 `M-t` 都调用原生 `dirvish-layout-toggle`，在普通视图和含左侧上级目录的布局之间往返。真实 Dirvish 会话、展开子目录及 `/fs:local:` 路径已有针对性测试。
35. 显式删除 Perspective 时，若仍有同名 tab，先通过原生 Tab Bar 关闭它们，再删除 workspace；唯一 tab 会复用为保留的 workspace。关闭 tab 被预关闭钩子阻止时保留 workspace 并报错。测试覆盖当前、非当前、唯一 tab 和关闭阻止的路径。
36. Dirvish 的 Cmd+Shift+Enter 打开光标项到独立的新 Emacs frame，使用已有 frame 创建入口，路径仍交给原生 `find-file`；失败时释放刚建的 frame。普通 Enter 仍是原生 `dired-find-file`，并提供可从 `M-x` 搜索的 `dirvish-open-new-frame`。
37. 统一的新 frame 文件打开动作现在也用于 `C-x C-f`、`SPC f f` 与 `H-f` 的文件名提示：Enter 交给原生 `find-file`，Cmd+Shift+Enter 使用当前输入的文件名在新 frame 调用原生 `find-file`，支持尚不存在的文件。Vertico 与原生文件名 minibuffer 都有该键；`M-x file-open-new-frame` 提供直接入口。新 frame 的创建与失败收尾只有一个实现。
38. frame 边界：顶部 workspace tab 保持 Emacs 原生的 per-frame 列表，Perspective 本身也以 frame 参数保存当前工作区。居中的 buffer tab 原先直接取 Perspective 全部 buffer，跨 frame 串列；顺序代码还引用不存在的 `get-current-persp`，实际退回全局顺序。现在以原生 frame 的 `buffer-list` 参数和当前显示窗口取实际访问集合，再与当前 Perspective 交集，顺序存到当前 frame 与 Perspective 的组合，并在 Perspective 删除时清理。相同 buffer 可同时存在于多个 frame，各自独立排序。Noema xwidget 页面虽无 `buffer-file-name`，其虚拟 `.md` 文件现用于 tab 收录、文件图标和提示；纯 Noema 应用及其他后台 buffer 不入列。Ibuffer 沿用已有类型分组，增加 `Frame` 列及 `s F` 排序，`F1` 等标记只给用户可见且被对应 frame 访问过的 buffer；被共享的 buffer 显示多个标记。批处理测试覆盖 frame 访问集合、Perspective 顺序生命周期、Noema 虚拟文件和后台 buffer，实际 Ibuffer 列渲染已检查；多 GUI frame 的交互还需在运行中的桌面会话观察。
39. Buffer 切换入口：`C-x b` 与 `H-b` 原为 Consult 的全局 buffer 列表，`SPC b b` 却用原生 `switch-to-buffer`，三者的候选范围和操作手感不一致。现在在默认 `consult-buffer` 调用期间临时绑定 Consult 提供的 `consult-buffer-list-function`：主 Buffer 来源取当前 frame（含埋藏 buffer）与当前 Perspective 的交集，`<o` 调出 Consult 自带的隐藏 Other Buffer 来源以访问其他 frame/workspace，最近文件与 bookmark 等来源保持可用。限定作用域只包日常 buffer 命令族；`consult-project-buffer` 显式传入项目来源，仍保留 Consult 原本的全项目候选，多 buffer 搜索也不受影响。`SPC b b` 指向同一个 `consult-buffer`，`SPC b B` 与 `H-B` 指向 Consult 原生的 other-window 变体；`SPC t i` 仍保留 Perspective 的纯工作区 buffer 提示。Ibuffer 继续承担全局盘点，不再另建 picker。埋藏 buffer 从可见 tab 消失，但 Consult 主来源与 Ibuffer 的 frame 标记仍保留它。实际测试覆盖当前/其他来源、项目范围、埋藏生命周期与键位。
40. Ibuffer 分组曾在 `init-base.el` 定义一套，之后被 `init-windows.el` 的第二套 `use-package ibuffer` 覆盖，前者成为死配置。现在只由 `init-base.el` 持有当前实际生效的 Noema、xwidget、Code、Dired、Shell、Emacs 分组及首次进入排序，窗口模块不再重配 ibuffer；图标、Frame 列等显示扩展仍由 `init-doom-extra.el` 提供。完整启动后实际打开 ibuffer 核对分组及列渲染。
41. 新 frame 打开 buffer：Emacs 原生 `switch-to-buffer-other-frame` 会优先复用已显示 buffer 的窗口，必要时以目标 buffer 创建 frame；这和明确“每次新建 frame”的意图不同，Noema/xwidget 也可能被带到第二个 frame 而发生渲染问题。保留原生 `C-x 5 b` 语义，另加可搜索的 `buffer-open-new-frame` / `SPC b F`：Consult 只给 buffer 来源，`<o` 可跨 frame/workspace；普通 buffer 由中性 frame 工厂创建新 frame，和文件打开共用失败收尾。Noema 页面复用已有独立 pane 的新 frame 流程，其他 xwidget 页面拒绝直接共享并提示使用所属应用的窗口命令。中性 frame 工厂在尺寸或定位失败后删除刚创建的 frame。批处理覆盖候选来源、普通 buffer、Noema 分流、非 Noema xwidget 阻止和创建失败收尾；实际 GUI frame 与 Noema host 异步完成还需桌面验证。
42. 文件工作台：`SPC f .` 的“selection buffer”把 `*... selection*` 这个显示名放在 `dired` 的目录参数里，原生 Dired 会将它解释为相对路径，临时视图因而指向不存在的目录。现在以真实 `default-directory` 和所选文件调用原生 `dired-noselect`，仅在这次创建中禁止复用同目录的普通 Dired buffer，然后给新视图命名；原目录列表保留。复制路径、计算大小和选择项视图共用的路径获取也改为优先读原生 Dired 标记，即使光标回到标题行仍使用已标记文件。新文件父目录的 Lisp 默认策略从不在设置类型中的 `nil` 改为 `ask`，与现有 store 和文档一致。真实临时目录测试核对标题行标记、创建选择视图、正确文件与原列表保留；文件工作台相关套件 20/20 通过。
43. 编辑工作台的 Ediff 入口：kill-ring 最近两项比较原先把临时 buffer 清理函数挂在全局 `ediff-cleanup-hook`，任何 Ediff 会话先退出都会杀掉这场比较的临时 buffer。现在通过原生 `ediff-buffers` 的启动回调，在本场控制 buffer 上安装局部 cleanup 与关闭 hook；启动失败也会立即释放临时 buffer。双窗口 `Ediff DWIM` 原先在两者都有未修改文件名时改走 `ediff-files`，会重新读取磁盘而非比较屏幕上的内容；现在始终使用原生 `ediff-buffers` 比较这两个可见 buffer，同 buffer 的两窗口给出明确提示。Ediff 的窗口恢复原先在 Git 模块使用一个全局 `local-ediff-saved-window-conf`，交错的会话会互相覆盖；现统一归编辑模块，按 frame 保存基准布局并在该 frame 最后一场 Ediff 退出后恢复，setup 报错或中断时回滚本次窗口变化。若直接杀掉控制 buffer，零延迟回调释放该会话状态；正常退出则等 Ediff 完成全部窗口调整后同步恢复。定向测试覆盖会话隔离、双窗口路由、交错退出、直接关闭和 setup 失败收尾；真实批处理 Ediff 会话验证退出后原窗口 buffer 列表恢复，frame 状态清空。多 GUI frame 的交错退出仍需桌面验证。
44. 测试入口：`test-menu` 已可搜索，四个高频动作新增 `test-nearest`、`test-file`、`test-project`、`test-rerun` 名称，内部 `my/` 实现仍供原快捷键调用。自动 runner 原只按根目录标记固定优先级选 Cargo，混合仓库里从 Go/Python/Node 文件运行测试也会落到 Cargo；现在从当前源文件的语言选对应工具，无可识别文件时才按项目标记回退。项目本地 `:test` 覆盖先于 runner 检测，避免额外文件探测。Go 根包的文件范围从 `./...` 收窄为 `.`，nearest 的 `-run` 参数按字面正则与 shell 参数两层引用。`compile` 同步失败或被取消时不覆盖“上次测试”记录。临时混合项目、Go 参数 shell 解析和启动失败回归测试已通过。
45. Frame 输出边界：原测试、任务、构建、运行 profile 和 Bazel 共用全局 `*test*`、`*task*` 等名字，最近输出及各类“重跑上次”记录也跨 frame 覆盖。现在统一经原生 `compile` 启动，但输出名带稳定的 frame 短 ID；每个 frame 独立保存最近输出和分类型的成功启动记录。Bazel 异步选目标回调会回到发起它的 frame，原 frame 已删除则不再打开别处的输出。原生 `M-x compile` 保留 Emacs 默认命名。批处理覆盖真实编译输出、类型与 frame 归属、启动失败不污染重跑记录和原有 frame tab/ibuffer 测试；多 GUI frame 同时运行仍需桌面验证。
46. 窗口退出状态：`ibuffer` 等临时页的 `q` 走原生 `quit-window`，其精确返回目标或临时窗口删除先由 Emacs 处理。旧配置总跳过 dashboard，缺少其它历史 buffer 时会回落到 `*scratch*`；现在只在原生结果是 dashboard／scratch 且仍有更好的同窗口历史页时替换，否则临时分屏关闭、唯一编辑窗口显示 dashboard。`SPC b z` 和仅剩一个 frame 时的 `H-w` 保持埋藏动作，只复用同一退回收尾，已复现并修复无历史页时落到 scratch 的路径。`M-W` 和 `C-x k` 都直接保留 Perspective 的交互式 buffer 选择。删除路径通过原生 `kill-buffer-hook` 记录可见窗口，再在关闭完成后收尾，不给 `kill-buffer` 或 `kill-current-buffer` 内建函数装 advice：本机的原生编译器缺少 `emutls_w`，装 advice 时生成 subr trampoline 曾使 GUI 启动中断在 `init-windows.el`，Raycast 因 server 未启动而显示没应答。修复后实际 GUI 启动、`emacsclient`、Raycast `state` 和 frame focus 均已验证；窗口定向测试通过。
47. Markdown 新建：原表单虽有 `TYPE`，但截图中的 `KIND note` 被误读成 Roam/普通 Markdown 选择。主选择现明确显示 `CREATE AS`（Roam node／Regular Markdown）及稳定 ID/图谱差异，`KIND` 改名为次级 `RENDER KIND`；切换类型会重新筛选并校验模板。新增可搜索的 `markdown-new-document`，从当前笔记目录预选普通 Markdown。共享 Noema 模板目录扩充到 13 种 Roam 场景与 11 种普通文档场景；运行时逐一创建 24 个临时文件，验证类型约束及 Roam ID／`roam: off` 元数据。Emacs 新建面板定向测试通过。
48. GUI 原生编译启动边界：`Emacs.app/Contents/Info.plist` 的 `LSEnvironment:LIBRARY_PATH` 仍指向已移除的 Homebrew GCC 16.1，当前 GCC 是 16.2；此前安装内建函数 advice 时需要现编 subr trampoline，因找不到 `libemutls_w` 中断初始化，Raycast 只能看到未应答状态。`early-init.el` 现在先从 `gcc/current` 找到真实 `libemutls_w.a`，只修正本次 Emacs 进程的 `LIBRARY_PATH`，不改应用包。以旧路径模拟 GUI 环境的批处理实际完成 trampoline 编译与完整配置加载。
49. Raycast 状态入口：原扩展在新的 `state` 请求完成前显示缓存状态，且查询失败时保留旧值，进程退出后仍可看到旧的橙色“未应答”及 frame。现在列表打开时先显示 Checking，成功后只用实时状态和候选；请求或返回值出错时显示 Could not check Emacs，并保留 Show or Start Emacs 恢复入口。Noema 两个独立入口不受 Emacs 状态影响。扩展的 TypeScript 检查与 Raycast 构建通过；另以 `npm run dev` 将新版载入当前 Raycast，实际界面显示后台 Emacs，状态行回车后 frame 变成可见。
50. GUI frame 实测：通过正在运行的 Emacs 建立两个临时 GUI frame，分别打开测试 buffer；各 frame 的 tab 和 Consult 来源只包含自身 buffer，ibuffer 标记为不同的 F1/F2。测试对象随后删除。书签入口审计又发现自制列表与 picker 会在生成候选时逐个 `find-file-noselect`，可能打开远端文件并留下 buffer；现使用 Emacs 原生 `list-bookmarks` 与 Consult 的 `consult-bookmark`，其他窗口跳转用原生 `bookmark-jump-other-window`，保留行书签扩展。`C-x r l` 恢复原生列表入口。
51. Scratch 生命周期：同名项目原来共用 `*scratch:项目名*`，后打开者会把同一 buffer 的持久化 ID 改成另一个项目；现在 buffer 名带稳定的项目 ID 后缀。更严重的是保存钩子进入 `with-temp-file` 后才读取正文，实际把临时空 buffer 写到状态文件；现在先在源 scratch buffer 捕获正文、光标、mode 和目录，再通过同目录临时文件原子替换状态文件。两个同名临时项目的写入、关闭、重新打开与分别恢复已有测试。坏状态文件在恢复前报错，不清空当前草稿；读取时禁用 read-time evaluation。状态文件读写固定在本机配置目录上下文，恢复时不探测远端工作目录；断连的 `/fs:` 工作目录测试不会尝试连接目标，状态仍写在本机。默认及项目 scratch 的 toggle 现在只查当前 frame 的窗口，不会关闭另一个 frame 中显示的 scratch；实际 GUI 两 frame 测试确认从第二个 frame 开关时，第一个 frame 的 scratch 始终保留。可搜索入口 `persistent-scratch`、`project-scratch` 与原生 `scratch-buffer` 分开，原生命令保持原义。

52. Perspective 会话保存：原本工作台命令第一次运行才设置 `persp-state-default-file`，原生 `M-x persp-state-save` / load 则起初没有默认路径；现在模块启动时配置 Perspective 自己的默认文件，并登记到配置板，工作台与原生入口共用它。`workspace-session-save` / `workspace-session-load` 可直接从 `M-x` 搜索；`SPC r s` / `SPC r l` 指向同一动作。保存仍由 Perspective 原生序列化，但通过同目录临时文件和 rename 提交；序列化出错时旧文件保留。工作台从远端 buffer 调用时仍把状态写在本机，缺失状态文件不会在加载时创建空目录。退出自动保存原先吞掉错误，现在发出 warning。定向测试覆盖原文件保留、远端上下文、缺失文件；完整启动中用原生 Perspective 完成一次临时路径保存，并确认原生命令和工作台命令均可搜索。原生加载仍按 Perspective 的语义创建 frame，重复加载可能产生额外 frame。

53. LiveTeX 键盘与退出路径：Emacs xwidget 恢复通道把 `TAB`/`RET` 误编码为 `Ctrl+i`/`Ctrl+m`，`Cmd-Shift-/` 也缺少 Slash/Shift 信息；现统一成浏览器键名，独立公式工具板在恢复通道优先接收分类切换、方向键、Enter 和搜索输入，不再落入正文或 MathLive。Studio 的 × 原先复用 Apply，原公式定位失败时无法退出；现 × 明确取消草稿并关闭，✓ 仍负责写回。渲染器更新遇到未提交的 Studio 草稿会等待关闭后再尝试刷新。相关 TypeScript 与 74 项公式测试、4 项 Emacs 恢复按键测试通过；实际打开的 Noema 页面已加载新构建，鼠标点击 × 的真实 GUI 流程尚待用户现场复核。

54. 公式工具板与多行行内公式：工具板补入半直积、花环积、正规子群、函子及一批不常见关系、箭头和集合符号；搜索会同时匹配命令、中文名和多个英文关键词，新增符号均由 KaTeX 渲染测试核验。`\(…\)` 在源码中可含软换行，收起后仍留在原段落里按行内数学渲染；只有跨行的定界符进入现有数学索引，普通单行公式继续走逐行扫描。索引只重扫发生编辑的行，代码区域中的定界符不接管正文。公式内 Enter 插入原样换行，不触发 Markdown 列表续行，也不自动改写为 `\\`；需要数学行对齐时由作者写 `aligned` 等 LaTeX 环境。Insert 模式的左右方向键可从定界符边缘进入源码，上下方向键跨过折叠公式时也能进入；xwidget 实际键盘路由已有回归测试。定向测试、5 MB 输入延迟测试与 TypeScript 检查通过，渲染器已构建并在打开的 Noema 页面加载。

55. 公式字形与跨行源码：截图中变量、黑板粗体及可伸缩结构字形不协调，实测页面的 `.katex` 与 `.mathnormal`、`.mathbb` 均被 Noema 自己的通配 `font-family: … !important` 覆盖。移除编辑器、主题和浮动预览的重复字体强制规则，保留颜色、公式基线与布局；按用户选择统一恢复 KaTeX 默认字体分工，不再单独覆盖普通变量。跨行公式展开时原先没有单行公式的源码标记和 TeX 高亮；现在同一局部 StateField 为展开公式添加逐行源码样式与 TeX token 标记，折叠时仍保持原子范围。源码标记覆盖多行与环境名、对齐符、数字的测试通过。

56. Jupyter broker 待关闭状态：Emacs 退出时 target 不可达，原实现虽在本进程保留 `shutdown-pending`，重启后便丢失 runtime 身份。现在每次关闭前原子写入本机 `pending-shutdown` 记录，确认关闭后删除；Jupyter Board 按 target 被动列出重启后遗留的待关闭项，只提供带确认的 Retry shutdown，并在页头提示跨 target 的待关闭数量。同一会话的待关闭 kernel 也只提供 Retry shutdown。重试前核对目标进程命令行仍含本 runtime 的专属连接文件名；断线、PID 复用或核对失败都保留记录，不发送信号。损坏的本机记录只在 Board 里显示诊断，不弹出额外警告窗口，也不触发远端操作。与旧 Noema task 同时出现时只展示一行 broker 待关闭项。启动 kernel 后若通道或 workspace 注册失败，回滚也走同一持久关闭路径；回滚时远端断线不再丢掉运行时身份和连接文件。定向 ERT 覆盖记录权限、跨会话恢复、PID 不匹配、启动回滚失败和 Board 合并；本机真实独立进程组的 TERM、连接文件和记录清理均已通过。Linux SSH 目标上又以三个独立 Emacs 会话验证了记录保留及真实 Remote 路由回收，`test/jupyter-pending-remote-live-smoke.py` 提供显式 opt-in 的复测入口；中段不可达状态是模拟的，远端为睡眠进程而非完整 ipykernel。后来关闭协议再补有界的 TERM 后进程退出确认，未确认时保留记录；本机真实独立进程组对此通过，远端同一分支仍需复测。执行中途真实断线与 `kill -9` 且未进入关闭流程仍需要单独处理。

57. Jupyter broker 强制退出恢复：`pending-shutdown` 记录原来只在关闭开始时写入，Emacs 被 `kill -9` 时脱离进程可能没有任何本机身份记录。现在目标 launcher 返回有效 PID 后、配置五通道之前就原子保存；本会话的活动 runtime 在 Board 中遮住其记录，正常关闭会删除它。下次 Emacs 启动若没有内存中的 owner，Board 将其显示为上一会话的 broker kernel，只能先核对 PID 命令行中的专属连接文件名再重试关闭。定向测试覆盖“记录早于通道注册”和“丢失内存表但保留记录”；真实 Linux SSH 目标上的 `test/jupyter-abrupt-exit-live-smoke.py` 对正式 broker 启动路径运行 SIGKILL，新 Emacs 会话经 Remote 路由找回、核对并回收远端睡眠进程。该探针未执行完整 ipykernel。

58. Linux Jupyter 目录与完整内核：CSE 登录节点有 `ipykernel`，但 `/usr/bin/jupyter` 缺 `jupyter-kernelspec` 可执行文件；原命令选择只看到分发器就认为可用，导致 Board 和 broker 的 kernelspec 发现失败。现在完整 CLI 继续优先，缺子命令时走目标机官方 `jupyter_client.KernelSpecManager` 与 `jupyter_core.paths` API，维持同一 `kernelspec list --json` / `--runtime-dir` 输出契约；七项命令选择单测通过。Linux 目标上的直接与 Board 异步发现、运行目录查询均通过。新增显式 opt-in 的 `jupyter-linux-kernel-live-smoke`，实测真实内核启动、执行、补全、五通道恢复后变量保留及关闭，远端临时目录和进程清空。一次 Homebrew SSH 路径在 `requestKernelInfo` 阶段超时，另一次转发握手收到信号 9；现由 `remote-client-ssh-program` 在 macOS 默认选系统 `/usr/bin/ssh`，统一 Remote 显式命令、TRAMP 方法和 tramp-rpc 启动。取消探针自己的 PATH 钉选后，Linux 真内核 E2E 再次通过。另加 `jupyter-linux-disconnect-live-smoke`，在真实执行 busy 后切断五条测试转发；Remote 自动恢复，前端请求有界失败，目标确认原内核死亡，broker 保留身份记录到确认、报告状态丢失并能启动空白新内核。重连前后都在同一节点，内核死亡原因尚未确认；完整 Noema 页面交互仍待验收。

59. SSH 多端口与实际主机身份：转发命令原来同时带 `-S none` 和受管 `ControlPath`；`ssh -G` 证实前者会取消后者，五个 Jupyter 端口可能经负载均衡登录名落到不同节点。现在有 ControlPath 时共用 workspace SSH master；无此选项时才禁用隐式复用。Linux 普通真内核测试通过，固定 `vx25` 的执行中途断线测试也恢复了五通道、确认旧内核死亡并完成新内核清理。另一次动态别名重连后原 `/tmp` 目录不可达，符合落到另一节点的迹象；内核现在持久记录启动主机，探活和发送信号在目标端同一命令内核对主机，不能确认时保留记录，Board 显示原主机。测试遗留的原节点进程组已确认退出，临时文件和记录已清理。动态别名的状态型内核仍应选择固定物理节点 target；完整 Noema 页面故障交互待验收。

60. SSH mux 转发就绪：原检查只认普通 OpenSSH 的本地监听日志；复用受管 master 的客户端只输出 mux session 就绪日志，导致五条转发各等满 5 秒才做后备确认。现在收到 mux 就绪后立即对本地监听做一次连接确认，未收到日志仍走原超时路径。针对真实 mux 输出的测试通过；CSE 普通真内核完整探针耗时 9.53 秒，固定 `vx25` 执行中断恢复探针耗时 24.19 秒（含故意等待 12 秒的执行请求超时），均完成清理。这里只记录本次总耗时，未用不同网络条件下的旧运行时间计算加速比。

61. Noema Jupyter 输出页状态：注册表的 `get` 有意过滤 `disconnected` 和 `dead`，原页面快照却只用它读取内核，断线会显示为 `not-started`、所有 cell 变成过期。新增只读 `inspect` 读取状态，执行路径仍用 `get`；快照区分连接失效、进程确认死亡和 `stateLost`，断线时保留已执行源码版本。输出页可见时每 5 秒查询轻量状态，恢复或内核身份变化才重读输出；远端文件刷新失败保留当前输出和 widget，不把暂时不可读误当成空文档。确认换核时清空旧执行版本，页面标明状态丢失。类型检查、定向 41 个测试和 Noema 页面生产打包通过；没有在真实 Noema GUI 中拔网线验收，动态登录别名返回原物理节点仍未解决。

62. Jupyter 关闭确认：原 broker 发送 `TERM` 后立即注销 runtime、删除连接文件和本机待关闭记录；若进程仍运行，就失去后续清理身份。现在本机记录写入失败会在发信号前停止，发送 `TERM` 后有界等待进程组确认消失；仍存活或探测不确定则保持 `shutdown-pending`。跨会话 PID 的 `ps` 查询与远端文件清理都在目标端同一命令中核对记录的物理主机，坏记录还不能指向旁边的任意 log。10 项定向关闭测试、真实本机独立进程组的 TERM/退出/清理、远端删除的主机保护测试和完整 `make jupyter-test` 通过。当前动态登录名实际落在另一台节点；经跳板直连原节点的首次主机密钥尚未建立，未替用户绕过验证或擅自信任，因此自动返回原节点仍不能保证。远端 TERM 后确认分支还需真机复测。

63. Dashboard Agenda 缓存与说明：卡片实际已经改为“先显示本机最近快照、可见时合并异步查询、15 秒超时、手动重试”，但日常文档仍描述已移除的重试间隔和超时设置。现已按实际入口更新。缓存读取原来允许 Lisp reader 求值且接受首个表达式后的垃圾；现在禁用求值并要求读完整文件。写入原来直接覆写旧缓存，失败可能丢掉最后好快照；现在同目录临时文件写完后原子替换，失败清理临时文件并保留旧缓存。损坏缓存不会阻止 host 后续刷新。定向 Dashboard 12 项测试通过；其中原有的 Chunlian 测试改为真实可见窗口前提，隐藏 Dashboard 仍不触发重绘。

64. Dashboard 窗口边界：本配置的 `dashboard-initialize` 包装会调用 `delete-other-windows`，核对已安装的 Dashboard 实现后确认它由 `emacs-startup-hook` 启动；日常的 `dashboard-open` 和 `dashboard-refresh-buffer` 只切换或刷新当前窗口，不调用该包装。临时页退出回到 Dashboard 因此不会触发启动期的分屏清理。可见 Dashboard 的重绘测试通过，隐藏页面不重绘。

65. Raycast frame 边界：原 `my/background--editing-frames` 把所有顶层 frame 纳入列表和可见性判断；TTY frame 没有 macOS `window-id`，会给 Raycast 一个重复的 `"nil"` ID，也可能使最后一个图形窗口被误判为仍有其它可见 frame。现在 Raycast 的 frame 列表、显示／隐藏和最后图形 frame 的判断只看图形顶层 frame；在 TTY 上直接隐藏当前 frame 给出明确错误。混合 GUI／TTY 的定向测试覆盖了列表、最后窗口和错误提示。

66. 临时页返回候选：Emacs 原生 `switch-to-prev-buffer` 会检查 frame 的 `buffer-predicate` 和退出时的跳过规则；原补救函数在 scratch/dashboard 回落时直接挑同窗口历史首项，可能把 frame 排除的 buffer 或普通历史中的旧 xwidget 页拉回来。现在补救候选遵守同两条规则；`quit-window` 按弹窗来源精确返回 Noema xwidget 的原生路径保持不变。Ibuffer、Cmd+Shift+W、弹窗精确返回与候选过滤的 15 项定向测试通过。

67. Dirvish 子进程路径：现有目录规则测试只拦截了 `dirvish-yank` 参数。实际 `/fs:local:` Dired 中的标记与落点传入原生 Dirvish 后，它启动 `emacs -Q`，该子进程不认识 `fs` 方法；临时文件实测返回 `Method ‘fs’ is not known` 且没有复制。现在通过 Remote 框架的 `remote-client-file-name` 把客户端可见的来源与目标投影为本机路径，再调用原生 Dirvish 的复制、移动和三种链接。真实 `/fs:local:` 异步复制与相对符号链接均已生成正确文件。跨目标标记按来源 target 分别预检和排队；来源文件重名会先报错，避免独立任务竞争覆盖；预检失败不启动部分任务且保留标记，yank 菜单失败时也保留复制或剪切状态以便重试；跨目标剪切仍拒绝。客户端不可见的同目标路径现经 `remote-exec-async` 在目标端执行复制、移动和三种链接，先检查冲突，再进入现有传输队列；断开、取消、重试及日志共用队列生命周期。混合来源会在跨目标任务入队前预检同目标操作。`/fs:local:` 强制走目标端的真实文件操作、五种命令与覆盖预检、展开子目录刷新均通过。真实 SSH 目标 `login.cse.unsw.edu.au` 的 Dired 光标入口五种操作及上传、下载已通过。真机回归还发现异步目标进程退出后 TRAMP 的“文件不存在”缓存没有自动失效；现在在队列任务完成前按目标目录与移动来源目录定向刷新进程路由及文件元数据路由的缓存，覆盖失败或取消时可能留下的部分写入。另在真实 SSH 进程上触发队列的目标断开回调，取消并确认进程退出；传输测试 39/39。另在独立批处理 Emacs 中终止测试自己的 SSH 传输进程，确认任务不再卡于运行中、本地 relay 收尾且日志标明远端结果未知；真实网络阻断后的重连仍待验收。

68. Remote Files 弹窗与同步预览：目录选择原先在调用排队回调前就关闭；路由或工具缺失时，用户会失去标记和当前目录。现在先启动任务，成功后再关闭弹窗并更新“对面目录”历史；失败时保留弹窗和标记。同步预览的“应用”原先在正式任务启动前清除并退出，启动失败后无法重试；现在启动成功才清除并退出。远端传输测试 20 项中 19 项通过，1 项真实 SSH 探针因没有 opt-in target 跳过；新增失败后保留弹窗与预览的定向测试。

69. Dirvish 命令搜索：`h` / `l` 原先各经一层不加行为的包装调用 `dired-up-directory` / `dired-find-file`，现在直接绑定原生命令。当前行目录的新建、粘贴、链接和回收站是实际扩展，给这些顶层动作提供 `dirvish-create-here`、`dirvish-paste-here` 等可搜索名称，并让按键和原生 yank 菜单指向相同名称。固定目录跳转、排序、详情显示和数字标签页只用于按键，现在以未 intern 的可读命令符号绑定，继续出现在按键帮助中，但不污染 `M-x`；用户可搜索 `dirvish-go-directory` 输入任意目录。完整配置下通过 `M-x` 所用命令补全核对 12 个名称、实际 keymap 及四类按键专用命令；Dirvish 15 项定向测试通过。

70. 文件工作台入口与临时视图：`file-menu` 的路径复制、所选文件大小和选择项临时列表原先只有 `my/` 名称，现在分别有 `file-copy-paths`、`file-copy-parent-paths`、`dired-selected-size` 和 `dired-selection`，菜单指向相同的可搜索命令。菜单里的图片裁剪与 `editing-menu` 重复，其包装还额外限制 `image-mode`，会拒绝 Emacs 原生 `image-crop` 可处理的光标下图片；现只在编辑菜单保留原生命令。选择项列表的原生 `q` 原来返回原 Dired 却把每次新建的临时 buffer 留在列表中；现仅在该 buffer 上设置 Emacs 31 的 `quit-window-kill-buffer`，`q` 返回原目录并销毁临时列表。完整启动下验证命令补全、菜单后缀与真实 Dired `q` 返回；文件工作台相关 26 项测试通过。

71. 传输队列与日志窗口：队列自身已经复用同一个 buffer，`q` 经原生 `quit-window` 返回打开前的页面；重复打开两次的真实窗口测试通过。旧的历史裁剪会直接杀掉超限任务的日志，即使用户正在阅读；现在可见日志暂时保留，后续不再占用历史名额的其他完成项照常裁剪。手动 `c` 清理已结束任务时，用原生 `quit-windows-on` 关闭可见日志并恢复该窗口的上一页，不把它换成 scratch。日志 `q` 返回队列、裁剪中保留可见日志、清理后恢复队列均经真实窗口测试。远端传输套件 22 项中 21 项通过，1 项真实 SSH 探针因没有 opt-in target 跳过。

72. Roam／普通 Markdown 新建表单生命周期：原 `*roam-new-node*` 虽复用 buffer，每次进入都会重新运行 mode 并重建 draft，重复触发同一入口会清掉尚未提交的输入；现在同一类型、同一目录且没有显式预填草稿时直接回到现有表单，不重新读取模板。原 `q` 只 bury 表单；现在表单局部启用 Emacs 原生 `quit-window-kill-buffer`，返回来源并释放草稿，按钮明确写“Cancel”。普通 Markdown 表单的 `R` 原来默认重置为 Roam，而且渲染时旧 widget 又会覆盖重置结果；现在保留当前文档类型、跳过旧 widget 回写。模板刷新先吸收已输入字段，再重读目录、校验已失效模板并渲染。真实窗口 `q`、重复打开、普通文档重置与模板刷新有定向测试；Markdown Roam 套件 60/60 通过。

73. 新建后打开的交接：原表单在 runtime 创建文件后先杀掉自身，再直接请求 Noema；冷启动回调会失去来源窗口，打开异常时再次按创建还会生成另一个文件。现在先记录已创建路径，使用 Markdown 的原生 `find-file` 路由显示文件；打开失败时保留表单并进入只可重试打开、重置或关闭的状态，不重复调用 runtime。联动测试又发现 Markdown redirect 在 `fundamental-mode` 前设置的 buffer-local 标记会被模式切换清除，使冷启动回调找不到占位页；现改为模式切换后设置。定向测试验证失败后重试只创建一次、冷启动时占位页存活并由回调接管，完整 Markdown Roam 套件 62/62、Noema Emacs 套件 197/197 通过。真实图形 xwidget 冷启动尚未在此批处理环境验收。

74. Markdown 占位页状态：进一步核对发现主机已就绪时 `my/noema-open-file` 会在 `find-file-noselect` 的 auto-mode 阶段立即打开页面，随后 `find-file` 再显示 Markdown 占位页，把 Noema 页面盖住；现在将页面打开安排在原生文件访问完成后的可取消一次性 timer。占位页采用独立模式，局部提供 `g` 重试、`e` 原生 Markdown、`q` 原生退出，超时后显示失败状态；实际按键核对发现 Evil normal state 会盖住这三键，现于文件访问完成后切入 Emacs state。退出或改用原生编辑会取消 timer，异步旧回调按 buffer 和请求代数失效。Noema 打开成功后才按设置关闭临时 buffer。对仍存活但未就绪的 web host，手动重试现在也会重新建立 10 秒 watchdog；同步启动失败移除无法执行的回调。定向测试覆盖热启动、冷启动、最终按键解析、超时重试、旧回调失效、原生编辑和成功后释放占位 buffer；完整 Markdown Roam 64/64、Noema Emacs 199/199 通过。真实图形 xwidget 尚待手工验收。

75. Diagnostics 面板边界：原当前 buffer 的错误、警告、提示分别创建不同名字的持久面板，重复打开可在不同 frame 间复用同一来源状态；板面还保留一套没有任何入口调用的自制项目诊断分组，而项目列表实际已交给 Emacs 原生 Flymake。现移除未使用的项目分组代码，三种严重程度只改变当前 frame 单一面板的筛选；从面板再次调用入口会保留来源与筛选，换来源会重置。原生项目列表的关闭只作用于当前 frame。Flymake 保存钩子只为可见且对应来源的面板安排合并刷新，不再扫描隐藏面板或把阅读位置拉回顶部；来源 buffer 被杀掉时用原生 `quit-windows-on` 关闭其无效面板，frame 关闭时释放该 frame 的面板 buffer。完整配置下实际渲染并 `q` 返回来源；定向测试 6/6 通过，覆盖筛选复用、来源切换、可见来源刷新、来源死亡、frame 清理及原生项目列表边界。多 GUI frame 的实际窗口切换仍需桌面验证。

76. Activity 与 Remote Transfers 的交接：目标端文件操作借用 Remote 的临时 stdout buffer，原 Activity 的 `RET` 优先打开它，导致与队列保存的日志不一致；现在任务行优先展示 owner 的日志。`i` 原来直接中断本地进程，`X` 直接杀 relay，绕过 Remote Task／Transfers 的取消状态；这两个入口对 Remote Task／Transfers 现在交给 owner，其他进程保持原生 SIGINT／SIGKILL。`x`、`i`、`X` 对这两类任务共用取消分派，传输队列仍是状态与重试的唯一归属。定向测试覆盖临时 stdout 与正式日志同时存在、传输和远端任务两种 owner 的取消路径。

77. Activity 面板的 frame 生命周期：原来所有 frame 共用 `*Activity*`，一个 frame 设置的筛选和自动刷新会改变另一个 frame 的板面，重复打开还可能沿用别处的阅读位置。现在每个 frame 复用自己的面板 buffer，任务清单仍来自 Emacs 的同一全局进程表；`q` 沿用原生 `quit-window` 返回，关闭 frame 时释放其面板与 timer。开启自动刷新后若面板被隐藏，下一次计时会停止，不再长期唤醒隐藏页面；显式重新打开会先手动刷新。首次创建时若渲染失败，释放新 buffer 并清除 frame 引用。批处理测试覆盖原生 `q`、同 frame 重开、模拟双 frame 隔离、frame 关闭和隐藏后定时器收尾；真实多 GUI frame 的窗口交互仍待桌面验收。

78. Activity 详情页：`d` 原来把进程和 OS 属性写进全局 `*Activity Process*`，不同 frame 会互相覆盖；现由各自 Activity 面板持有可复用的临时详情 buffer，退出 `q` 使用原生 `quit-window` 并销毁该 buffer，关闭 owner 面板也清理详情。实际加载测试发现单用 `special-mode` 时 Evil normal state 把 `q` 解释成录制宏，页面会等待下一个键；详情改用专属 `Activity Detail` 模式及 Emacs state，键绑定核对和真实窗口退出测试通过。`list-processes` 和 `proced` 仍直接交给 Emacs 原生管理器；自定义详情仅补充 Emacs 与 OS 的单进程属性。

79. Remote Transfers 键盘与 frame 视图：队列和日志原本都进入 Evil normal state，实际 `q` 被解释为录制宏，队列 `RET`、`k` 等也没有走 mode map；旧测试只直接读取 map，没检查最终绑定。现在队列和日志分别使用 Emacs state，日志保留独立的只读模式，完整配置下核对实际键绑定并用真实窗口验证 `q` 的返回。任务和日志保持全局一份，队列表格按 frame 独立复用，排序与阅读位置不互相影响；进度更新所有可见队列，清理历史也同步刷新，关闭 frame 只释放其视图。队列与日志的 `pop-to-buffer` 显式只复用当前 frame 的窗口，避免被 Emacs 可配置的跨 frame 复用策略带到别处。独立 Remote Transfer 套件 26 项中 25 项通过、真实 SSH 项未启用而跳过；完整配置 Activity 联动 25/25 通过。多 GUI frame 实际交互仍待桌面验收。

80. Remote 主面板和历史页：完整 Evil 配置下，主面板 `q` 被当成录制宏、`RET` 和 `c` 也被 normal state 接管；路由、连接及 SSH 日志的 `q` 同样失效。主面板和日志现在使用 Emacs state，按键实际解析到对应命令，`q` 经原生 `quit-window` 返回；临时历史页退出后释放，SSH 诊断输出继续保留。主面板和历史页按 frame 持有视图，重复打开主面板保留排序，状态变化刷新所有可见主面板，关闭 frame 只清理视图，不动连接。完整配置下实际窗口验证主面板 `q`、`RET`、`c` 与两种历史页 `q`；模拟双 frame 测试验证视图隔离和关闭清理。真实多 GUI frame 的窗口交互仍待桌面验收。

81. 临时机器列表：原 `*Remote Machines*` 单例在每次打开时重建 mode，清掉排序；Evil normal state 还会接管 `RET`、`e`、`p`、`q`。现在每个 frame 复用自己的列表视图，按 `q` 经原生 `quit-window` 返回，关闭 frame 只释放该视图。新增、编辑、删除后刷新所有可见机器列表和 Remote 面板，不再只按固定 buffer 名刷新第一份。仅换密码时 SSH pipeline 配置字节可能不变，原配置重载会保留旧会话；现在通过 Remote 配置层淘汰该 target 的旧会话，下次操作使用新凭据。完整配置的实际按键与窗口返回测试、模拟双 frame 的更新与清理测试、只换密码的会话失效测试通过；真实多 GUI frame 的窗口交互仍待桌面验收。

82. Remote Doctor：原报告是全局单例，第二个 frame 的诊断会覆盖第一个；完整 Evil 配置下 `q` 变成录制宏，报告还从末尾打开。现在每个 frame 持有独立的临时报告页，从顶部显示，`g` 重跑当前 target，`q` 用原生窗口返回并释放报告；关闭 frame 也清理其报告。consumer 扩展返回格式错误时只显示该扩展的错误，不再中断整份报告；probe 进程因信号退出时保留原因和已有输出。完整配置实际按键与窗口测试、模拟双 frame 隔离、probe 失败及错误 consumer 的定向测试通过。真实多 GUI frame 的窗口交互仍待桌面验收。

83. 临时机器持久化失败回收：原新增操作先改内存，再写 `machines.json` 和派生的 `machines.authinfo`；实测第二份文件写入失败时，命令报错但内存和 JSON 已有新机器，target 尚未注册。原删除操作甚至在写盘之前断开旧连接。现在四种更新入口共用一次提交：先保留私有备份，写盘、执行必要的断开、重载 target；写入或重载失败时恢复原内存和两份文件，若注册表已经切换也重新加载原记录。删除只在保存成功后开始断开。Remote 主配置缺失时不再假装添加成功，而是报错并回收写入。故障注入覆盖凭据写入、主配置缺失、普通重载、提交后 hook、提交中连接失效与删除前写盘失败，均能恢复记录；删除已断开而后续重载失败时，记录会恢复，连接仍须下次操作重建。
84. 临时机器跨进程冲突：保存操作持有共享 `machines.json` 的 Emacs 原生文件锁，并在锁内比较内存加载时的文件摘要。另一 Emacs 已保存新版本时，当前操作报错且不写盘；管理列表 `g` / `M-x remote-machines-reload-from-disk` 重读记录、重载 target 后可重试。ERT 覆盖外部版本冲突、重读后的再次保存，以及两个独立 Emacs 进程同时抢锁；抢锁测试还暴露并修复了无效 `file-locked` 错误类型。此保证针对使用这套提交入口的 Emacs 会话；不使用文件锁的外部编辑器仍可能在摘要检查后并发改写文件。
85. Raycast 延迟结果：原 `act` 和 `frame` 只确认定时器排队，随后执行失败时 Raycast 已关闭，只能去 Emacs 消息区看错误。现在两个入口返回短期请求编号，`bin/emacs-app result ID` 查询 `pending`、`ok`、`error` 或 `unknown`；Raycast 在限定时间内查询，立即失败留在列表并提示，成功后再按原交互关闭或刷新。超时不重发动作，明确说明最终结果未确认，由 Emacs 接管。结果表在读写时按年龄清理，不安装常驻 timer；Emacs 重启或记录过期会得到 `unknown`。实际部署核对发现 Raycast 新扩展可先于运行中的 Emacs 更新，因此排队前在同一次客户端求值中检查协议版本；旧 Emacs 不会接收无法查询结果的动作，避免用户误重试。Emacs 端定向测试覆盖排队、成功、延迟失败和消失的 frame；本机运行中的 Emacs 热加载后，真实 `emacsclient` 和 `bin/emacs-app result` 返回 `ok`／`unknown`，无效动作返回非零退出。Raycast 打包、shell 语法检查通过；真实 Raycast UI 的前后台切换仍需桌面验收。
86. Noema Agenda 入口归一：原生日程同时有 `my/noema-agenda` 与再转发一次的 `my/noema-roam-agenda`，F9、Hyper、Roam 键图和两处菜单指向不同层；文档又把原生与 Web 视图都写成难以按用途查找的 `my/` 命令。现在原生公开入口为 `M-x noema-agenda`，Hosted Web 总览为 `M-x noema-agenda-web`，月历、日志、Gantt、项目、时钟表、检查与搜索也各有 `noema-agenda-*` 直达名；各按键与菜单统一指向对应公开命令，内部实现和项目 scope 激活仍由原模块处理。完整配置测试核对全部名称可被命令补全找到，`amx` 重建后也能收录原生与 Web 入口；实际键图核对 F9、Hyper-a 与 Roam `A` 均指向原生入口。原生／Web 路由及 Agenda 测试共 115 项通过。
87. 项目运行与任务菜单：两个模块原来分别读取 `package.json`、Makefile 与 justfile；运行 profile 仅凭文件存在或全文中出现 `"dev"` 就列出命令，可能选到不存在的脚本或目标。现在复用任务模块的脚本与目标解析，运行菜单仅显示真实候选；项目本地 `:run` 覆盖先于手工 profile，手工配置先于自动发现。混合仓库的任务同名时只给冲突项加来源前缀，避免多个 `run` / `test` 选择同一个命令。运行命令从 Transient 的源 buffer 取项目和 direnv 环境，启动失败不会覆盖本 frame 上次运行记录。直接执行与重跑有 `project-*`、`task-*` 可搜索名；F5、Hyper-R 与 leader 仍打开原有菜单。任意临时编译命令继续使用原生 `C-x p c` / `project-compile`。候选发现核对时，`make project-workflow-test` 11 项（含 `/fs:local:` 项目发现）、健康键位 ERT 13 项及相关模块 byte compile 通过。进程生命周期的后续核对见第 88 项。
88. 项目任务生命周期：实测原统一输出入口在 `/fs:local:` 下仍调用原生 `compile`，workspace 资源表里没有该进程，关闭 workspace 后 `sleep` 仍存活。现在已有 owning workspace 的普通本地路径与逻辑 `/fs:` 路径都通过 `remote-task-run` 启动；未进入 workspace 的普通本地目录保留原生 `compile`。运行、构建、测试、Bazel 均传递项目环境；输出窗口的 `K`、`R` 与原 Remote task 的 `g` 保留各自取消、重跑语义。workspace 关闭或输出 buffer 销毁会取消任务；从 `/fs:` 按需新开的 workspace 若启动失败则回收，原有 workspace 不受影响。本地项目工作流 ERT 16/16、Remote task 3/3 与严格 byte compile 通过。真实目标 `login.cse.unsw.edu.au` 的 `tramp-rpc` 验收覆盖成功、失败码、目标端取消与 workspace 收尾，并从项目输出入口启动任务、手动重跑且保留环境。故障注入确认该入口遇到传输断线时状态为 `interrupted`、退出码未知，不自动重跑；workspace 恢复后按 `R` 才重新启动，并能在目标端确认取消。`aaron-pc` 在连接阶段超时，尚不能判断其任务行为。
89. 输出页的 GUI frame 边界：原入口只保存本 frame 最近的一份输出，关闭后即使上一份 buffer 仍在也会报无输出；`pop-to-buffer` 还没有明确限制当前 frame。现在从 frame 的原生 buffer 历史找回其上一份输出，打开时限制复用当前 frame；`output-menu`、`output-switch` 可直接从 `M-x` 和 amx 找到，leader 与 Hyper 指向公开名称。实际运行中的单 frame Emacs 创建两个隐藏 GUI frame 验收：A、B 的输出历史不混合，关闭 B 最新输出后可找回 B 上一份，打开仍在 B 且 A 的窗口不变。另从隐藏 GUI frame 启动 `/fs:local:` 的活任务，确认输出只在启动 frame 显示；关闭 frame 后任务继续由 workspace 持有，输出 buffer 原先留下已删除 frame 引用，现由 frame 关闭 hook 释放。探针收尾后恢复原可见窗口、取消任务并关闭临时 workspace；项目工作流 ERT 17/17 通过。可见 GUI 中对标签外观和手工点击仍待核对。

90. 缩小/展开入口：`SPC n n` 在 Org 源码块原来调用 `org-edit-special`，意外进入独立编辑 buffer；在其他块又调用缺少必需参数的 `org-in-block-p`。现在沿当前 Org 元素的父级寻找块，调用原生 `org-narrow-to-block`，普通位置仍用原生 `org-narrow-to-subtree`。公开命令 `M-x narrow-or-widen` 可搜索，leader 指向同一入口。完整配置下的编辑工作台 ERT 27/27 通过，实际键绑定及命令补全也已核对。

91. 导航返回历史：`M-,`、`SPC n b` 与鼠标菜单原来直接调用 `xref-go-back`，而定义和结构跳转把原位置记在 Better Jumper；实际文件跳转后返回键没有移动。现在这些入口优先走当前窗口已记录的跳转列表，仅在没有实际跳转时交给原生 xref；前进同理。Better Jumper 的公开 post-jump hook 用来确认是否真的跳转，避免用“光标位置是否变化”误判。Noema 页既有的按键 remap 保持指向自己的页面历史。真实文件跳转、空 Better Jumper 时的原生 xref 回退与编辑工作台 ERT 29/29 通过。

92. 结构上跳：`SPC n u` 原先只要 Emacs 提供 Tree-sitter API，就不走原生 `backward-up-list`；在 Emacs Lisp 等未启用语法树的 buffer 中实际报“没有外层结构”。现在先尝试 Tree-sitter 的父节点，没有合适节点时调用原生括号导航；C 的 `compound_statement` 等语句节点也纳入匹配，从内层代码逐级上跳而不直达函数开头。确认目标后才记录跳转，失败不污染历史。公开命令 `M-x structure-up`、leader 和鼠标菜单指向同一动作。Emacs Lisp 原生路径与真实 C Tree-sitter 路径均已实测；编辑工作台 ERT 31/31、入口键位/可搜索性测试和字节编译检查通过。

93. 文件位置入口：`SPC n l` / `C-c C-j` 原先用 Emacs 的 `expand-file-name` 处理所有路径，从 `/fs:target:` buffer 输入裸绝对路径会误落到客户端；现在只在逻辑文件边界用 Remote 的路径解析，普通本地 buffer 继续用原生路径。公开 `M-x open-file-location` 与原键位指向同一入口。行号超出文件时原先已切到目标 buffer，前缀参数还会先开第二个窗口；现在预先在未显示的文件 buffer 内校验，失败时保留来源窗口和已存在 buffer 的光标/缩小状态，清理新建且未修改的 buffer。成功后复用原生 `find-file` 内部的显示函数，不重复访问文件。目标身份、逻辑本机真实打开、无效行号两种窗口方式和已有 buffer 的保留测试通过。

94. Ibuffer 的 Noema 重复条目：此前 `RET` 和 `M-RET` 的特殊分支直接调用参数数量已改变的 `ibuffer-unmark-forward`，选择重复条目必然报 `wrong-number-of-arguments`；即使修正参数，分支还会绕过 ibuffer 自带的单窗口前缀、不选中窗口和标记保留语义。现在只在访问动作期间把该行的目标解析为 Noema 主页面，窗口切换、bury 和标记行为仍由原生 ibuffer 命令处理。用实际 ibuffer 列表先复现两条访问路径的报错，再验证单窗口前缀、其他窗口不选中以及标记保留。

95. 新 frame 文件入口：`C-x C-f`、`SPC f f`、`H-f` 提示中的 Cmd+Shift+Enter 与 `M-x file-open-new-frame` 最终共用 `my/file-open-new-frame`。该函数原先在创建 frame 前直接 `expand-file-name`；从 `/fs:target:` buffer 输入裸绝对路径时会变成本机文件，和来源 target 脱离。现在只在逻辑文件边界使用 Remote 路径解析，普通本机路径继续交给原生 `expand-file-name`，并在建 frame 前固定路径身份。实际测试覆盖不同 target 的显式逻辑文件名、相对路径、普通本机路径、`/fs:local:` 真实文件访问及原有失败时释放 frame。另核对 Amx 源码与真实模块 `load`：取消其 idle timer 后，`load` 仍会让下一次 `M-x` 按需重建缓存，因此没有另加扫描或定时器。

96. 文件提示的一致性：继续对照原生 `read-file-name` 后发现，用户在 `/fs:` buffer 清掉默认目录并输入裸绝对路径时，Emacs 原生返回本机路径；之前改好的 Cmd+Shift+Enter 会打开 target 文件，普通 Enter 却会打开本机同名文件。现在当前窗口与新 frame 的分支都在提示返回后通过 `my/open-resolve-file-name` 解析一次，实际访问和窗口显示仍交给原生 `find-file`；`file:line` 入口也复用同一解析，避免三份规则漂移。原生 `M-x find-file` 未改写；配置的统一提示新增可搜索 `M-x file-open`，`C-x C-f`、`SPC f f`、`H-f` 指向它。用原生 `read-file-name-default` 实测裸绝对路径返回本机名字，再用 `/fs:local:` 真实文件验证统一提示的 Enter 路径；新 frame、导航、Amx 缓存和入口键位定向测试通过。

97. 文件菜单补齐与公开名称：`file-menu` 原来只列路径复制和 Dired 辅助动作，打开文件还要退出菜单另找入口。现在 `o` 和 `O` 分别直接调用现有 `file-open` 与 `file-open-new-frame`，不复制路径解析或窗口生命周期。检查还发现原菜单布局登记在 `my/files-dispatch`，公开别名 `file-menu` 虽可执行却不能按该名称查询 Transient 后缀；现由 `file-menu` 直接拥有菜单，`SPC f .`、主命令面板及健康检查也指向公开名称。完整启动下核对实际键位、主菜单后缀、公开菜单后缀和命令补全；文件打开的八项定向测试通过，包括 `/fs:local:` 真实访问。

98. 公开菜单身份：沿 `file-menu` 复查其他菜单，实测 `command-hub`、项目、工作区、运行、任务、测试、Git、LSP、调试、编辑、输出、健康、编译、Jupyter 以及 Noema AI/Wiki 的公开别名虽 `commandp` 为真，`transient-get-suffix` 却找不到该名称的布局。现各菜单直接以可搜索名称定义，快捷键、主菜单后缀、板面按钮、鼠标菜单和健康检查使用同一符号；移除了这些旧 `my/*-dispatch` 菜单名称。运行、任务、测试三个菜单原放在延迟加载的 `use-package` 配置段中，改为按需加载模块时立刻登记公开菜单。Noema 主菜单还原先经 `noema-menu` → `my/noema-pop` → `my/noema-dispatch` 两层纯转发，现由 `noema-menu` 直接拥有 Transient 布局，顶栏菜单与 `H-o` 指向它；Markdown 的 Wiki 菜单也去掉只做转发的 `my/noema-roam-dispatch`。完整启动下逐项查询 18 个公开菜单布局及其实际 suffix，验证 Noema 顶栏项和快捷键；健康、文件工作台、项目运行、工作区 58 项、Jupyter Board 43 项、Markdown Roam 65 项测试通过，`make health-byte` 通过且没有新菜单名称警告。

99. 诊断与代码动作入口：`SPC c ?`、`H-d`、LSP Hub、Telescope 和鼠标菜单原来都指向只能用 `my/diagnostics-dispatch` 搜索的菜单；`SPC c .`、`H-e`、主命令面板和鼠标菜单同样使用 `my/code-actions-dispatch`。现在菜单直接定义为 `diagnostics-menu` 和 `code-actions-menu`，所有入口使用同一公开名称；后者从延迟加载的 `use-package` 配置改为模块加载时登记。核对功能后，诊断的项目页仍委托 Emacs 原生 Flymake 列表，代码菜单的诊断项仍复用既有 picker，未再建面板。完整启动下验证两菜单可由 `M-x` 补全、可查询真实 Transient 后缀、leader/Hyper 键位一致；诊断生命周期 6/6、健康入口 14/14 与 `make health-byte` 通过。

100. LaTeX 预览入口：原文档只列 `my/latex-preview-dispatch`，现在 `latex-preview-menu` 直接拥有 Transient 布局并可从 `M-x` 搜索，四个后缀及行为保留。菜单可从非 TeX buffer 调起，但原先选择原生 AUCTeX 构建／查看命令时模块可能尚未加载，得到 `void-function`；四个动作现在先检查文件支持的 LaTeX 上下文，再加载 AUCTeX 的 `tex`，给错误上下文明确提示。已构建 PDF 的右侧窗口是配置扩展，`V` 仍交给原生 `TeX-view`。完整配置下核对菜单后缀与命令补全，定向测试 2/2、`make health-byte` 通过。Noema 公式字体继续使用 KaTeX 默认字形分工，未添加新的字体覆盖。

101. 合并冲突的按键生命周期：`smerge-mode` 本来就提供 `C-c ^` 原生操作键图；配置另有便捷键，却在启用 hook 中用 `local-set-key` 和 `evil-local-set-key` 改写当前 buffer，关闭 mode 后仍残留，且非 Evil normal 状态的普通文字键会被拦截。现在原生键图保留，便捷键只登记在 Evil normal 状态的 `smerge-mode` 局部键图中，随 mode 自动生效或失效；插入、选择及 Emacs 状态不再接管字母键。`merge-conflict-menu` 直接拥有可搜索的 Transient 布局，`SPC g m`、Git 菜单和 Hyper 菜单指向它，菜单动作仍委托原生 Smerge／Ediff。完整启动下验证模式启停、插入状态、原生前缀与菜单后缀；定向测试 2/2、健康入口 14/14、`make health-byte` 通过。

102. 调试 profile 的 frame 与项目边界：原 `my/debug-profile-dispatch` 是唯一菜单名称，最近一次 profile 的配置和目录存为全局变量；从另一个 frame 或项目按“重跑”会启动前一处的调试配置。启动前就更新全局记录还会使一次同步启动失败覆盖上次可用配置。现在 `debug-profile-menu` 直接拥有可搜索布局，`SPC r d`、`H-D` 与 Dape 总菜单指向它；`dape` 和活动会话的原生重启入口保留。最近一次配置按 frame 与项目根保存，捕获发起 frame，Dape 接受调用后才记录；同一项目可重跑，换 frame 或项目给出明确提示，启动同步失败保留旧记录。定向测试用两个 frame 身份和两个逻辑项目根核对重跑隔离与失败保留；健康入口 14/14、`make health-byte` 通过。其他语言 adapter 与断线恢复仍按 Remote parity 表逐项验收。

103. 书签入口和原生边界：普通设置书签的配置包装层只写入全仓库没有读取者的 `my-project-root` 属性；Emacs 原生 `bookmark-store` 覆盖同名书签时原位更新旧记录，包装层却给列表第一条写属性，可能标错另一书签。移除此包装层及无调用者的自制删除／重命名包装，`SPC b m`、`SPC b M`、`SPC b r`、`C-x r m` 和菜单改用原生 `bookmark-set`、`bookmark-set-no-overwrite`、`bookmark-rename`；原生按已配置的 `bookmark-save-flag 1` 保存。公开菜单为 `bookmark-actions-menu`，避开 Emacs 已有的 `bookmark-menu` 函数，保留 Consult 跳转、原生书签列表与其他窗口跳转，补上原生同名新增和删除。行书签是实际扩展，其当前位置查询改在已打开的当前 buffer 算行号，不再调用 `find-file-noselect`；书签列表仍为 Emacs 全局列表，跳转在当前 frame 显示。定向测试覆盖菜单和键位、行书签不重访文件；健康入口 14/14，`make health-byte` 通过且不再有原生 `bookmark-menu` 冲突警告。远端来源的本机书签文件保存见第 104 项。

104. 书签持久化的 Remote 来源：先在 `/fs:local:` buffer 中用临时本机文件验收，随后把来源改为一个未注册的非本机 `/fs:` target，原生 `bookmark-save` 在 `bookmark-write-file` 内部 buffer 查询继承的目标目录属性时失败，不能完成本机书签写入。现在只对本机绝对路径在原生 writer 周围固定客户端目录，并修正 writer 临时 buffer 的目录；显式 Remote 文件名及相对文件名维持原生放置语义。隔离测试在未注册 target 上下文完成写入和重新加载，因此不会为保存本机书签建立目标连接；显式 Remote 与相对路径的边界也通过测试。书签定向测试 4/4、健康入口 14/14、`make health-byte` 通过；真实 SSH target 上的书签跳转仍未做端到端验收。

105. 编辑菜单的格式复制：原 `my/copy-as-format-dispatch` 无可搜索的用途名称；七个包装命令调用 `copy-as-format-<format>` 时会永久改写包的全局默认格式，选区复制还会被包内部调整光标与 mark。现在公开 `copy-as-format-menu`，编辑菜单的 `c` 指向它；内部包装只在本次调用动态绑定格式，继续交给包的原生 `copy-as-format` 实现复制。无选区时临时选整篇，有选区时保留范围，调用后恢复光标与 mark。工作台测试 32/32、健康入口 14/14、`make health-byte` 通过。

106. macOS 工具入口与失败反馈：原工具菜单、拾色和光标打开命令只有 `my/` 名称，且拾色把任何 AppleScript 错误和只读 buffer 的插入失败都报为“取消选择”。现在 `SPC a m` 与 `M-x macos-tools-menu` 共用可搜索菜单，两个动作为 `macos-pick-color`、`macos-open-at-point`；内部目标归一与系统 `open` 仍由原模块负责。拾色只把 AppleScript 的取消错误映射为取消，其他脚本错误原样报告；带前缀插入失败时颜色仍在剪贴板，报真实的只读错误。定向测试覆盖入口可搜索、菜单和键位、脚本失败、取消与只读 buffer；工作台测试 34/34、`make health-byte` 通过。

107. 鼠标动作板与 Git 状态段：原 `my/mouse-dispatch` 是给外设单键打开的可点击动作板，不是设置菜单；它列出 buffer、项目、代码与跳转的直接命令，右键源码／状态栏菜单则按点击位置选动作，所以保留两类入口并把动作板公开为可搜索的 `mouse-actions-menu`。Git 状态段原来同时绑定按下和释放事件，用全局 350ms 时间戳抑制重复；这也吞掉了快速点击另一个窗口的独立动作。现按下时打开，释放时只消费事件，不再以全局时间节流；与 Emacs 原生 VC 状态段的按下事件保持一致。定向测试 2/2 覆盖菜单入口、外设键位、连续两次点击与两种鼠标键的按下／释放映射；健康入口 14/14、`make health-byte` 通过。真实 GUI 跨 frame 点击仍待桌面验收。

108. Bazel 原生边界与异步入口：已检查安装的 `bazel.el`，它原生提供 build/test/run、目标补全、当前测试和 mode 键位；本配置的 target-side 查询、项目环境、Remote 执行与 frame 输出是额外能力，保留两套明确入口。扩展菜单为 `bazel-target-menu`，直接命令使用 `bazel-target-*` / `bazel-output-*`，`SPC r z` 和任务菜单指向同一菜单。原构建入口已按发起 frame 处理异步目标选择，但插入标签、跳转目标和输出目录会落到回调时选中的 frame；现它们也记住发起 buffer/frame，关闭来源后不打开别处，插入命令在成功、查询失败及同步启动失败后释放 marker。空查询有明确提示，缓存写失败不阻断已经取回的目标。定向测试 7/7、健康入口 14/14、`make health-byte` 通过；真实 GUI 多 frame 与实际 Bazel target 仍待验收。

109. Noema 报告入口：Wiki 菜单已有网页报告总览，六个 Emacs 原生报告却挂在没有菜单入口的 `my/noema-roam-reports-native` 下；帮助页还提示不存在的 `M-x reports`。现在 `noema-reports-menu` 汇集网页总览及六种 Emacs 报告，主 Noema 菜单 `!n`、Wiki 菜单 `D` 可打开，`!r` 与 Wiki `!` 原来的网页直达行为保留。删除没有调用者的 `my/noema-roam-reports`、仓库管理／索引状态兼容转发函数，以及未使用的重复统计扫描。日常使用文档中与当前 Noema 菜单不符的报告、Knowledge、Index 和 Format 键位已按代码更正。完整 Roam 测试 64/64 通过；frame 生命周期见第 110 项。

110. Noema 临时板的 frame 生命周期：原 `my/noema-roam--prepare-ui-buffer` 按固定名字复用全局 buffer，两个 frame 打开同类报告时会覆盖彼此，`q` 返回后还留下隐藏临时板。现在公共入口按 frame 持有并复用板，同一 frame 刷新保留原 buffer，另一个 frame 获得独立 buffer；`q` 经原生 `quit-window` 返回并释放该板，关闭 frame 清理其板，杀 buffer 时解除登记。Wanted 与孤立附件两条异步 API 回调捕获发起 buffer/frame，来源关闭后不在别处弹出报告。定向测试覆盖双 frame 身份、重复打开、`q` 返回、关闭 frame 与异步回调；Roam 测试 67/67 通过。真实双 GUI frame 的视觉和点击交互仍待桌面验收。

111. Roam 新建表单的 frame 草稿：原 `my/noema-roam-new` 独立使用全局 `*roam-new-node*`，第二个 frame 打开时会重置或复用第一个 frame 的草稿；前一轮的临时板隔离没有覆盖这个表单。现在把 frame buffer 分配和所有权收尾提取为共享内部函数，表单与报告各按发起 frame 持有自己的 buffer。同一 frame、同类型和目录的重复入口仍恢复已有草稿，跨 frame 不共享标题、路径或模板；`q` 和关闭 frame 只释放各自表单，创建成功／打开失败的既有处理不变。完整 Roam 测试 68/68 通过，包含双 frame 草稿隔离与单 frame 创建回退。真实双 GUI frame 的表单输入仍待桌面验收。

112. 标签重叠报告与公式字体：原标签重叠命令没有菜单入口，用全局 `*roam-tag-overlap*` 和独立的 `special-mode` 显示；现作为 `noema-reports-menu` 的 `t` 和可搜索的 `noema-report-tag-overlap` 入口，沿用 frame 持有的临时板、原生 `q` 返回及异步回到发起 frame 的行为。公式字体按用户确定保持 KaTeX 默认：当前 Noema 工作树已移除覆盖所有 KaTeX 子元素的自定义字体规则，保留现有颜色。Roam 测试 69/69、`make health-byte` 通过；真实 GUI 显示仍待桌面验收。报告的数据字段随后在第 113 项核对修正。

113. 标签报告数据契约与重复编辑入口：继续核对 Noema host 后发现第 112 项报告仍按旧 `pairs` 字段取值，实际 API 返回 `duplicateCase`、`overlaps`、`tagCount`，所以真实报告会错误显示为空。现在分别呈现大小写变体与高度重叠的标签对，并显示共享笔记数、各标签笔记数、包含率和 Jaccard 比例。邻近的两个 Emacs 标签重命名／删除命令没有调用点，还发送 host 不接受的 `oldTag`／`newTag`，把返回的数组当整数格式化；已移除这两条失效路径。批量修改继续由 Noema 编辑器现有 Tools 面板负责，它已有候选、明确确认与修改结果。成功修改后 host 只发一条 `notes-mutated` 事件清除 Emacs 侧 Roam 缓存，无论这次改写多少文件。报告回归测试使用 host 当前字段结构；Roam 套件 69/69、Noema 桥接套件 201/201、host 语法检查及 `make health-byte` 通过。

114. Roam 移动笔记：旧命令提示只能改文件名，先调用 `fs:rename`，再手工调用 `rewrite-path-refs`；host 的 `fs:rename` 已经负责绝对／相对引用改写，第二次调用重复且在首次移动成功后可能另行失败。现在公开为可搜索的 `noema-move-note`，`noema-menu` 的 `m` 同路；目标使用 Emacs 原生文件名补全，可选择已有目录，且只发一次 `fs:move`。发起时阻止未保存的原生 Emacs buffer 或已被占用的目标；若源文件在一个 Noema 网页页打开，命令定向送到该页，先保存草稿再移动并打开新路径，多页同时打开时要求先关闭多余页面。host 在移动后通知 Emacs 原生 buffer 跟随实际路径；干净的 buffer 重读 host 改写后的内容，移动途中新增的编辑保留并提示核对。批处理测试覆盖单次 host 操作、目标目录、`/var` 符号链接路径、网页页路由与移动事件；Roam 73/73、Noema 网关 202/202、TypeScript 检查、网页构建、`make health-byte` 通过。真实 GUI 交接待桌面验收。

115. 当前笔记移到废纸篓：原网页入口确认后允许丢弃未保存草稿，且成功后任意打开索引中的第一篇笔记；确认框停留时若页面切换，也会继续删除旧文件并重置新页面。现在 `noema-trash-note` 可直接由 `M-x` 搜索，Noema 菜单 `!d` 与顶栏铅笔菜单共用它；命令要求当前选中明确的 Noema 页面，避免从普通 buffer 把删除命令广播到所有网页页。网页输入 `TRASH` 后先提交 LiveTeX、保存草稿并重新核对文件身份，移动期间暂停编辑。成功后取消旧文件的保存重试，优先恢复该页访问过的上一篇与光标位置；没有可打开的上一篇则回 Wiki 首页；若移动期间有程序触发新的编辑，仍在当前页保留为未命名草稿并提示复制。host 删除与同文件的保存排在一条写入队列，带旧版本的迟到保存遇到文件不存在时返回冲突，避免旧网页页把已删文件重新建回。目录重命名、复制与删除仍由原生 Dired/Dirvish 入口承担，未增建重复的 Noema 文件管理面板。服务器保存定向测试 13/13、公开入口 2/2、页面定向入口 1/1 和 TypeScript 检查通过；真实多网页页与原生 buffer 同时打开时的删除后呈现还需桌面验收。

116. 删除后的其他视图：第 115 项只处理发起操作的网页页；同一文件若在另一个 Noema 网页页或原生 Emacs buffer 打开，仍会显示旧内容。现在 host 对笔记删除和文件 API 的 Markdown 废纸篓操作广播一次 `note-trashed`，并向 Emacs 发送同名事件。网页页在保存队列收尾后，干净时返回自身的上一条有效历史，否则进入 Wiki 首页且用 replace 避免回退到已删页；有未保存修改或未能提交的 LiveTeX 时解除文件身份、保留内容并提示复制，页签持续显示 `Trashed draft: …`。Emacs 原生 buffer 的干净副本关闭并沿用 `init-windows` 的上一页／Dashboard 回退；修改过的副本解除访问文件名、保留内容与修改状态，命名为 `Trashed draft: …`，可自行另存。该名称不使用 `*…*` 包装，以便留在同 frame 的 tab line。批处理测试覆盖原生干净／修改两态与 gateway 转发；Roam 75/75、Noema 网关 204/204、Noema 定向 18/18、Noema 全量复跑 3237/3237、Go 内核、Emacs research/Jupyter、TypeScript 检查、网页构建和本机安装通过。全量 Noema 测试首次有一项文件监视器测试在 5 毫秒等待后未收到事件；该项单独复跑 2/2 通过，第二次全量运行也通过。由原生 Dired 在 host 之外直接删除／重命名文件时，仍依赖文件监视器的通用变更事件；它不携带目标路径，需继续审计。

### 外部设计对照

- [Doom Emacs 的 workspaces 模块](https://github.com/doomemacs/modules/blob/main/modules/ui/workspaces/README.org)把常用 buffer 列表限制在 workspace，同时允许回到原生全局列表；这里保留相同的“日常局部、按需全局”边界，不复制 Doom 的 workspace 框架。
- [Spacemacs layouts](https://www.spacemacs.org/layers/%2Bspacemacs/spacemacs-layouts/README.html)区分布局内 buffer 选择与默认全局布局，并把关闭布局、删除布局及其 buffer 作为不同操作；本配置沿用已经存在的原生 Tab Bar 与 Perspective 生命周期，不增加另一套布局对象。
- [Consult 的 buffer 来源](https://github.com/minad/consult/blob/main/consult.el)已经支持 frame buffer 列表和隐藏的 Other Buffer 来源；这里通过其公开配置项组合当前 Perspective，而不另写候选器。
- [Prelude 的 Vertico/Consult 模块](https://github.com/bbatsov/prelude/blob/master/docs/modules/vertico.md)也把 `C-x b`、其他窗口和其他 frame 的打开动作交给 Consult 的同一命令族，并为来源窄化配置 `<`；本配置沿用已有的 Consult 与 Vertico，不再引入另一套补全框架。

## 后续审计顺序

| 优先级 | 领域 | 尚需核对的事实 |
| --- | --- | --- |
| 1 | Jupyter 与 Noema 内核 | 关闭失败保留身份、启动失败回滚、跨 Emacs 会话记录、真实 SIGKILL 后回收、Linux ipykernel E2E 及固定节点连续两次执行中断恢复已核对。转发共用受管 SSH master；动态别名切换时的 PID 安全边界已有持久主机记录与目标端核对。页面状态代码、定向测试和生产打包已通过；继续用真实 Noema GUI 验收断线交互，以及让动态别名自动返回原节点的能力。后者目前不作保证。 |
| 2 | Dirvish 目标端文件操作 | 本机与真实 SSH 目标的五种操作已从 Dired 光标入口验收；缓存失效修复后，39/39 项传输测试通过，真实 SSH 进程的队列取消及测试批处理中 SSH 传输异常退出后的收尾亦已确认。继续验收真实网络阻断后的重连。 |
| 3 | 外部 Raycast 结果 | 排队编号与有界最终结果查询已实现，快速失败可留在 Raycast；需要回答 Emacs 提问的长动作只提示结果未确认，不自动重发。真实 Raycast UI 的前后台切换仍需桌面验收。 |
| 4 | 领域内的命令目录 | Activity 取消与 provider 失败已核对；继续按项目、远端、Noema、编辑模式检查顶层动作的名称、按键、帮助文本和资源收尾；内部 helper 保留 `my/` |
| 5 | 项目输出的可见 GUI 手感 | 两个隐藏 GUI frame 的输出隔离、历史回退、显示位置和关闭 frame 后的活任务归属已核对；仍需在可见 GUI 中看标签外观、手工切换和关闭窗口后的呈现。`aaron-pc` 当前连接超时，不能据此判断其目标端任务行为。 |

每一轮至少核对：入口名称和按键、首次打开、重复刷新、关闭后资源、失败反馈，以及在远端 buffer 中是否误用本机环境。只有确认问题后才改相应领域的代码。

`M-x my/reload-init` 保留为只重读顶层文件的兼容入口；它不会刷新已加载模块。模块源码改动使用现有重启入口。
