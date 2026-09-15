# agent-shell / ACP 审计与包管理迁移（2026-09-15）

## 当前结果：已按用户要求迁出

agent-shell 0.70.1、acp.el 0.13.1、shell-maker 0.96.1 已由 package-vc
安装到 Emacs `elpa/`。Git HEAD 与下表三个审计 revision 一致，上游已跟踪
源码没有改动；只有包管理器生成的描述文件和编译产物。
`init-ai-ide.el` 和 `package-lock.el` 固定同组版本，没有同时升级 main。
安装器和 bootstrap 现在将 revision 显式传给 `package-vc-install`，避免
只在 spec 中写 `:rev` 却实际上安装 main。已有包的普通启动不联网。

隐藏输出策略移到 `site-lisp/noema/lisp/noema-agent-render.el`：

- `always/visible/never`，Noema session 默认 `visible`；ACP 事件和权限处理照常运行。
- 每 buffer 4 MiB 近似字节上限，FIFO 常数时间入队/淘汰；重新显示时按序回放。
- 两处窄 advice 截获 upstream fragment/text 渲染；可关闭
  `noema-agent-render-mode` 回到上游原生行为，关闭前回放已缓存输出。
- 会话、恢复、事件等耦合仍由 `noema-agent-acp.el` 负责。这减少源码维护，
  不等于完全零维护；升级仍须运行渲染和 ACP 契约测试。

`noema-upstream.el` 已停止添加这三个目录，并在重新激活时移除旧路径。
旧完整源码（含 142 行性能补丁及其测试）可从以下目录恢复：
`var/backups/noema-acp-migration-20260915.pznxmi/`。
原 `upstream/{agent-shell,acp,shell-maker}` 仅留下本机忽略的过渡符号链接，
指向 `elpa/`，兼容尚未重启的 Emacs；它们不再是被维护的源码副本。
gptel、Magent 等未在此次迁移范围内，仍保留内化来源。

生效：完成当前工作后正常重启 Emacs。不要对活动会话 unload-feature，
也不要在旧源码性能补丁仍已加载时热装新渲染 advice。迁移没有终止活动进程。
冷启动检查 `symbol-file` 已确认 agent-shell、ACP、shell-maker 来自 `elpa/`。

迁移后验证：`research-test` 的全部五组通过（1 + 26 + 8 + 131 + 71），
后加的 revision 传参测试及渲染/窗口/Evil 契约共 15/15 通过；
Jupyter 四组共 160 项、Go `-tags fts5`、`make build`、`make install` 通过。
Node 默认并行全量两次各有不同的计时阈值失败；相关测试单独运行全部通过，
再以 `npm test -- --no-file-parallelism` 跑完整套得到 2353 passed / 16 skipped。
未放宽性能断言、未改动这些不相关的测试，也未进行付费模型/真实 agent 请求。

后续升级：通过 Emacs 包管理器升级这三个包，验证后将新的精确 revision
同时写入 `init-ai-ide.el` / `package-lock.el`。运行 `make research-test`、
`make jupyter-test` 和 Noema Node/Go 回归；尤其保留隐藏/可见输出、prompt、
取消、权限、恢复/fork 的检查。不要在升级时对包源码重新打补丁。

以下保留迁移前的审计记录，过去的“尚未迁移”和失败数不代表当前结果。

## 迁移前审计

结论：主体是外层组织，不是大规模改写；但当前还不能无损地直接切回上游包。
本轮没有删除 vendored 源码、切换包源或改写上游文件。弹窗接入也放在外层。

## 源码对照

从官方仓库取回 `UPSTREAMS.md` 记录的精确 commit，逐文件比较完整工作树
（包括本地尚未提交的改动，而不是只比较 Git HEAD）：

| 组件 | 记录的上游版本 | 全树对照 | 本地差异 |
| --- | --- | --- | --- |
| agent-shell 0.70.1 | `6a83589393fb67725f288d08d6f12d136564db0e` | 77 文件中 75 完全相同 | `agent-shell.el` 新增 142 行；测试新增 20 行；无删除 |
| acp.el 0.13.1 | `7d5c16ebcf2af86aa0f14ad9ae0ce45df4e8c8a5` | 8/8 完全相同 | 无 |
| shell-maker 0.96.1 | `bb5e3aef17686c1c859c366eb83831b0046dc75a` | 12/12 完全相同 | 无 |

唯一上游源码补丁是隐藏会话的延迟渲染：`always/visible/never` 策略、
每 buffer 4 MiB 有界缓存、重新可见时回放。它在两个内部渲染函数处截获
fragment/text 更新，保留 ACP 消息和事件处理。不能简单删掉补丁后仍声称
保留后台 agent 的相同性能特征。

另取审计时官方 main：agent-shell `6ae364b57b399b803f757a5634b301e8656e19a2`、
acp.el `242cef63d76cc1073485847f67a21f6d8406d158`、
shell-maker `f448a74a8eded23aa42f8d60a41c5d8d3a183d07`。
该 agent-shell main 仍没有 `agent-shell-output-render-policy` 和
`agent-shell-flush-pending-output`。这不是“升级一下已经有了”的功能。

## 外层耦合

`site-lisp/noema/lisp/noema-agent-acp.el` 集中处理窗口、会话身份、事件、权限、
上下文和停止/恢复。边界是集中的，但仍引用 11 个不同的 `agent-shell--*`
内部符号，包括 `--start`、`--state`、`--display-buffer`、`--send-request`、
`--shutdown` 和模式配置接口。改由包管理器下载并不会消除这些兼容性责任。

`noema-upstream.el` 当前主动将内化目录放到 load-path 最前；只执行
`package-install` 不会改变实际加载来源，还可能制造混合版本。

本次新 popup 仅添加 Noema 边界的 buffer-local 展示回调，并把真实 agent-shell
buffer 纳入原终端窗口池。保留 agent-shell 原生状态 header，使用 tab-line
承载共享标签，不修改 upstream、不在每次 heartbeat 上加新 advice。

## 建议的迁移顺序

1. 将上述性能策略抽成 Noema 自己的可选模块，优先寻找/推动公开的渲染扩展点。
   如暂时需要内部 advice，单独记录兼容版本与契约测试，不能宣称“完全零维护”。
2. ACP 与 shell-maker 无本地补丁，适合最先交回现有包管理器，但先锁定现在
   的精确 revision，保证与 agent-shell 同组兼容；不要同时迁移和追 main。
3. agent-shell 去除源码补丁后交回包管理器，确认 load-path 和已加载 feature
   来源一致，再用干净 Emacs 验证 prompt、取消、权限、隐藏流式输出、恢复/fork。
4. 最后升级上游版本，做同一组回归；确认稳定后才移除冗余内化目录。

本轮全量 `research-test` 仍有 5 个此前即存在的 agent/worker/Pi 路由失败，
所以没有把“依赖获取方式迁移”和当前 UI 修改叠加，以免掩盖回归来源。
本轮 popup/ACP 边界定向测试 9/9，通过真实窗口切换、隐藏保活和固定测试；
没有向真实 Claude/Codex/OpenCode 发送提示词或测试付费请求。

官方来源：[agent-shell](https://github.com/xenodium/agent-shell)、
[acp.el](https://github.com/xenodium/acp.el)、[shell-maker](https://github.com/xenodium/shell-maker)。
