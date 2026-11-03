# Portal、Ghostty、Web 与 Eshell 的 Remote 放置检查

2026-11-04，客户端 macOS / Emacs 31.1。接续 ACP session/project 检查；本轮只改
terminal/frontend 边界与其调用点，连接 backend 和进程库的并行改动保留给另一会话。
Portal native 仓库原有未提交改动未被本轮修改，GUI 验证使用其已有 build。

## 复现与落点

用户截图中的 popup 是 `*remote-terminal:*portal-popup*`，`pwd` 为 `/home/aaron`，
编辑器文件在项目子目录。源码确认 `/fs:` method 使 `+portal-create` 退成 comint，
该分支既未传入选中的 cwd，也丢弃 command；workspace 根被误用作 invocation cwd。

| 项目 | 原行为 | 当前实现 |
|---|---|---|
| Portal frontend | 按 TRAMP method 分支，SSH 自拼 argv，`/fs:` 退成 comint | 所有 target 共用 native Ghostty；framework 选 PTY route 并准备 client-native plan |
| cwd | 终端跟 workspace 根启动，子目录被覆盖 | 所属 workspace 与 invocation cwd 分开；`:directory` 明确传入 process API，重启保留 |
| project owner | 临时子目录可能创建另一个 workspace，关闭父 workspace 漏掉终端 | 先经 `remote-workspace-for-path` 查询已打开的最具体 owner |
| native 环境 | 长期运行的 native host 不知道 Emacs 后来的环境覆盖 | argv 显式携带 client process plan；backend 投影 target 环境；复用已有 capsule，不为 PTY 构建它 |
| shell 初始化 | Portal argv 入口启动一个 login shell 查程序，再启动目标 shell | 已经准备好的命令走 Portal command 入口；client `exec-path` 查找保持有效 |
| UI 集成变量 | Emacs 环境快照不含 native host 的 Ghostty 资源，清空环境会丢失本机 `cd` 事件 | 只在客户端补充 host 生成的 UI 默认值与 zsh integration；显式 plan 环境仍优先 |
| native 生命周期 | 没有 Emacs process，不能被 workspace terminal API 接纳 | `live/send/close` 回调接入相同资源表；关闭 buffer/workspace 释放 native view，关闭一个不杀 peer |
| 断线与退出 | frontend 布尔退出无法证明成功或错误 | 未知退出保留 disconnected；保存最多 256 KiB 可读滚动文本后释放 handle，手动重启保留名字与配方 |
| Eshell 身份 | 以 TRAMP prefix/method 命名，同一 target 可拆出不同队列 | context key 使用 target ID，逻辑路径和物理 SSH/RPC 写法共享身份 |
| Eshell 模块加载 | 首次远端加载 `em-unix` 的全局默认值在目标 PATH 查找 grep，触发多余连接和探测 | `eshell-load-modules` 在 client cwd/环境内加载；初始化仍在目标 buffer，冷启动回归独立验证 |
| 官方 process 环境入口 | RPC 上游读到了动态变量，标准 TRAMP 的直接 SSH plan 未收到，Eshell 导出值在两个 backend 上不一致 | `/fs:` make/start-file-process 入口统一提取客户端顶层环境的变化、动态 TRAMP 环境与具体 PATH，显式 invocation env 最后覆盖；普通 client HOME 等未变化值不复制到 target |
| Portal Web | 首次加载和打开可能继承来源 buffer 的 target cwd/PATH | 入口与 native code 加载固定为 client；普通 localhost/file URL 保持 client 含义 |

`remote-terminal-prepare-client-launch` 复用 backend 的 `prepare-process`，不引入新的
SSH 实现。非 Emacs process frontend 的回调不是伪造 process；metadata 中保留实际
route，workspace 按同一资源模型拥有它。不能执行 client-native plan 的 backend
明确失败。配方包含环境，诊断不得打印整个 argv。

Portal 的 `portal-terminal--new` 是本轮唯一新增的 package-private seam：现有公开
argv 入口会再次启动交互 login shell，而框架已经解析了 argv 与环境。配置只在这个
adapter 内使用 command 入口，不修改 Portal package 的源码；升级后须重跑 GUI 用例。

## 验证

```sh
make terminal-remote-test
TERMINAL_REMOTE_TARGET=local make terminal-remote-live
TERMINAL_REMOTE_TARGET=local make terminal-remote-gui-live
TERMINAL_REMOTE_TARGET=aaron-pc TERMINAL_REMOTE_BACKEND=tramp-rpc make terminal-remote-live
TERMINAL_REMOTE_GUI=1 python3 test/terminal-remote-loopback.py
```

本机 14 项放置/生命周期测试通过，覆盖 client cwd 报告保持原 target、动态环境与显式 capsule
优先级等；既有 framework terminal 13 项通过；Portal、popup、浏览器与 Eshell 的
回归中仅原生新 frame 的条件用例跳过。Eshell 独立的
38 项契约测试也通过。批处理真实 local 的 comint/Eshell 通过，原生 GUI 两项在 batch
中按条件跳过，不能算 GUI 验证。

独立 GUI runner 运行的真实 Ghostty/Web 两项均通过：native cwd/环境/双向输入、
释放 handle 前保留历史、同名手动重启、client Web 页面加载、普通交互 shell 的
`cd` 回写、同项目 peer 独立关闭、workspace 结束后释放 view。最后复验另暴露了
配置里已有的延迟空终端清理：native view 释放后，timer 会误删仍由 workspace
拥有的 disconnected 历史。已在调度与执行处保留这个状态；新增定序回归和真实
GUI 再次通过，这次采样为 2.350 s 与 0.959 s。没有把它当作网络延迟或普遍提速比例。
`var/agent-api-scope-state-check` 的全启动比较为零差异。

临时本机 SSH 服务使用独立 host/client key、用户态 sshd、私有 SSH config，不改
用户 `authorized_keys` 或已保存的 Remote 配置。fixture 显式声明默认 RPC、PTY
`ssh-pty`，再分别指定 Eshell 的 process-async 为 TRAMP 和 RPC；不能把异步进程
首选误当成所有 filesystem/environment 操作的首选。每个实际 process 或 native
terminal metadata 都核对 target、backend 与 capability。

这份真实 SSH fixture 的 TRAMP / RPC 两路 Eshell 均通过：目标 cwd 与导出变量、
两个目录中的队列顺序、超时后后续命令继续。Ghostty 与 Web 的真实 SSH GUI 两项
也通过：同一份目标 command 与环境、PTY 输入输出、释放 native view 后通过目标
`kill -0` 验证原来的子进程已消失、重启、项目 peer 关闭及 workspace 释放。
这是真实 SSH 到同一台 macOS 的协议验证，不能当作 Aaron-PC/Linux 的物理远端成绩。
RPC server 使用本机缓存中与已安装 package 精确相同的版本复制到 fixture 临时目录；
它不验证首次部署。一次早期冷部署出现 `Couldn't find a proper ls command`，后续
预置 helper 的通过没有覆盖该问题。早期未声明默认 backend 的 fixture 也在标准
TRAMP 的 PATH 探测中超时，未把这些运行算作通过。

终端与 session/project API 修改后，完整 `make remote-test` 的 16 组 ERT 共 646 项，643 通过、
3 项条件跳过、0 意外结果（契约、conformance、source、transfer、ACP、
process/workspace、LSP、Copilot 与项目回归），包括延迟清理回归的终端 14 项。
SSH 两条 backend 与 native GUI 再验通过。同步状态快照仍零差异。
严格字节编译排除既有 docstring/obsolete 告警，产物仅写临时目录并清理；可选
Portal/Eshell/Term 依赖在检查前加载，Windows 的 C opener 补齐函数声明。

## 来源 URL 的后续实现

对照 [VS Code 的 asExternalUri 契约](https://code.visualstudio.com/api/references/vscode-api#env.asExternalUri)：
URL 需要从执行环境转换成客户端可访问的地址，转发生命周期由 editor 拥有，结果不能
被当成永久地址缓存。本轮没有照搬其余 URI scheme 的处理；新增的是 Remote 的明确
来源 HTTP(S) loopback URL 边界。

`remote-url-for-client` 复用现有 channel group 与 workspace recovery，不再建 URL
缓存或 SSH 实现。backend 的 `:network-location client` 让客户端网络空间保持原 URI，
不以 target ID 判断；其他网络空间只暴露客户端回环监听。同项目同端点的多个 URL
共用一项可恢复资源；另一项目独立拥有。每次读取当前资源值，恢复替换和手动关闭后
不沿用旧句柄。普通外部 URL、mailto/file/data 不探测目标；未知来源的 localhost
不能隐式改用当前 buffer。userinfo、编码后的路径、query 和 fragment 原样保留，
资源 key 与诊断不存 query 或凭据。

配置已有 `+portal/open-terminal-url`，由确切 terminal 对象取得 workspace，交给
`aaron/open-portal-url` 的新显式来源参数。GUI 用例从另一个未配置 target 的临时
buffer 发起调用，在目标启动真实 Python HTTP 服务并读取其文件，再由客户端
WebKit 核对随机页面标题。本机没有新增 forward；临时 SSH 目标确实经 forward
拿到页面。关闭 workspace 后 forward 与 native terminal 已释放，client web buffer
仍可保留。local / SSH 的真实 GUI 现在各 3 项通过；SSH 的两条 Eshell route 仍通过。
这是 HTTP 数据面的证据，HTTPS 只有 URI/透传契约，没有真实 TLS 页面验收。
浏览器按改写后的主机验证证书，不能把 network-stream 保留目标 SNI 的能力套到它。

编译门禁暴露了旧的路径排除问题：配置入口被误放进独立库 lane，因缺少 bootstrap
上下文失败。现在公共库仍在 `-Q` 环境严格编译，两个配置入口则在完整初始化后严格
编译，没有删除其检查。新覆盖的入口修复当前 TRAMP 变量名、延迟 advice/Magit
变量声明与一处 docstring 引号；`tramp-sh` 加载后的新旧变量确为同一 binding。
启动比较仍为零差异，没有改变连接选项的值。

本轮 `make remote-check` 已通过：独立库与完整配置入口严格编译都没有排除告警，
17 组 ERT 共 656 项，653 通过、3 条件跳过、0 意外结果。之后补充一个非 local
target 仍可声明客户端网络位置的回归，URL 专项现有 10 项。

原生点击接入尚未落地：Portal 的 [AGENTS.md](../../../site-lisp/portal/AGENTS.md)
要求 “Run `task help` before picking up, adding or claiming a task”，并要求独立 worktree。
本机没有 task CLI 或相邻 portal-tasks 仓库，`gh` 也未登录；已询问它们的位置。
未修改该 native 仓库的现有脏源码，也未把上述显式命令测试算作 native 点击测试。

## 通道故障与混合 backend 恢复

继续故障注入发现三处通用所有权/恢复缺口，而不是 Portal 自己的重试问题：

- 单独登记 channel/forward/group 没有记录实际 route；没有终端或 LSP 的 workspace
  匹配不到通道失败，自动恢复不触发。三个统一入口现均登记活跃路由，恢复后的
  group 也更新实际路由，不额外 resolve 或探测。
- 成功的文件路由也没有记到已有 owner。TRAMP 转发与 RPC 文件混用时，workspace
  不知道自己实际用过 RPC。现在成功的路由操作按逻辑路径查询已有 owner，内存中
  只补充尚未登记的 attachment；不创建 workspace/session。
- 重连原来交错执行 invalidate/acquire。某个 backend 关闭旧 attachment 时会碰到
  另一个刚打开的同 pipeline 会话。重连改为先清理全部旧 attachment，再打开新代。

第一轮真实 SSH 的 TRAMP-forward / RPC-file 混合测试确实失败：workspace 已报告
open，紧接着的 `write-region` 却收到 `remote-connection-cancelled`。未重试这个写入，
也未排除测试。修复登记与恢复顺序后，同一测试在 TRAMP / RPC 两条 forward route
上均通过，真实 GUI 的恢复后页面也通过。

独立 HTTP fixture 在目标启动脱离测试 SSH 客户端的服务，返回 PID/端口并保存私有
PID 文件。仅终止 forward process，等待 workspace 自动恢复和资源句柄换代，然后
写入新的标题，通过新的 HTTP 连接读取，GUI 再核对 WebKit 标题。正常结束按实际
UID、脚本路径与唯一临时目录核对 PID 后停止服务。Python 专项另测正常读写/停止、
PID 被复用时不发送信号、PID 文件写入失败时回收已启动的子进程。
SSH runner 还把本次私有 SSH fixture 路径作为 owner 标记传给 detached 服务；
即使 GUI 测试进程崩溃，runner 的 UID/唯一标记清理也能找到它。正常服务测试直接
通过 CLI 启动，验证真实子进程 argv 带标记，避免把仍运行的 Popen 对象当成结束。

最后两轮临时 SSH 的 TRAMP/RPC batch 与 RPC GUI 均通过；本机 GUI 4 项通过。
最新通用门禁 `make remote-check`：38 个文件严格编译通过、17 组 ERT 共 661 项，
658 通过、3 条件跳过、0 意外结果。URL/路由专项现为 14 项；HTTP fixture 的 3 项
Python 检查单独复验通过，输出无 ResourceWarning。全启动 state-check 仍为零差异。

上述一轮证明测试转发进程异常退出后的恢复；它不等于物理断网或半开 TCP。
local 网络没有额外 URL 转发可终止，local URL 测试只证明直连与清理，未把它算作
SSH 故障注入。

## 转发资源故障与项目隔离

继续核对发现，杀掉一个客户端 forward 也会报告整个 pipeline 传输失效，同一
transport 上的其他项目会被卷入清理。现在本机 listener 关闭、客户端信号终止和
转发进程正常退出按资源故障处理，使用已有 background 调度按 workspace/resource
稳定 key 合并恢复；不额外发送探活。进程错误退出和重建实际返回的 transport
错误仍升级到 pipeline 恢复，不把真实传输故障吞成资源成功。

资源关闭、workspace 关闭与全 pipeline 恢复均取消待决局部任务。恢复期间同 key
再次打开返回 busy；恢复回调晚到时核对 owner、资源值、job 身份和 target epoch，
清理未被采用的新句柄。回调中已有别的替换被采用时保留它；回调报错或 quit 时
释放新 group，quit 继续向调度器传播。

URL 专项现有 22 项全部通过，覆盖 peer project 的句柄保持、重复事件只恢复一次、
关闭取消、晚回调不能复活 owner、保留后来替换值、异常/quit 清理、真实重建错误
升级。通用 framework 另用真实 `exit 0` / `exit 255` 子进程区分退出事实。

`TERMINAL_REMOTE_GUI=1 TERMINAL_REMOTE_AGENT=1 python3 -B test/terminal-remote-loopback.py`
的两条 SSH backend 与 RPC GUI 均通过，页面恢复不再出现 workspace 断开/重连。
ACP 用例在两个项目各保持一个 pending prompt，终止其中一个项目的 forward，核对
实际 process/forward route、原有 client、进程、session ID、backend attachment 与
target epoch 保留；另一项目新的 `session/list` 收到实际响应。随后按项目停止仍只
取消一个池。相同 ACP 用例在 native 上通过；使用确定性 fixture，无模型请求。

最新 `make remote-check` 在当前共享工作树完成：39 个文件严格编译，17 组 ERT 共
676 项，673 通过、3 条件跳过、0 意外结果；HTTP fixture 的 3 项 Python 检查也通过。
`state-check` 对原有 baseline 得到 1 行删除、4 行增加，全部为并行 Remote 知识层 /
RPC 缓存工作的静态变化：新 `remote-knowledge` feature、RPC cache hook 新成员、
`tramp-rpc--cache-entry-valid-p` advice 和 settle-time 设置；本次没有把这份共享
工作树结果记为零差异，也未改写 baseline。未重载用户的 Emacs。

## 显式断开后的队列与迟到回调

检查 retained buffer 发现另一处生命周期缺口：Eshell 没有订阅 Remote 的
`remote-buffer-disconnect-hook`，两个待发命令在真实框架断开后都没有结束回调，
旧 dispatch timer 仍可执行。测试先复现 `2` 个回调实际为 `0`，随后才改实现。

Eshell 现在在断开前结束当前和等待命令，回调为 `disconnected`、status 为 nil；
取消 dispatch/timeout，保留提示符草稿，不执行旧队列。API 拒绝往 retained buffer
追加工作，用户明确输入命令才重新启用。目录切换可能让出执行，出队 item 现先
登记为 starting，`eshell-reset` 不再把该阶段误认成完成；如果此时发生断开，
同一回调只结算一次，返回后也不继续发送。另去掉空队列、无草稿时多安排的一次
dispatch timer，保留真正有后续工作时的异步排队。

新增三条本地生命周期测试覆盖真实 target teardown、running child 退出、草稿
恢复、其他 owner 不受影响、目录切换中断以及明确手动重启。相同 live 用例在
native、临时 SSH 的 TRAMP 和 RPC 通过：读实际 process route，断开结束两个命令，
草稿恢复，自动提交被拒绝，手动命令后 API 再次成功。fixture 同时独立绑定 Eshell
history/last-dir 路径，跨测试不沿用已删除的临时目录。

ACP 的对应窗口也已复现并修复；详情见 Agent 检查记录的“显式断开与协议回调”。
最新临时 SSH runner 五组共 22 项：14 通过，8 为 batch 中按条件跳过的 GUI 项；
独立 GUI 组的 4 项均通过。两条 backend 都完成 Eshell 和 ACP 的实际断开/重连。
消费者的 ACP/API/生命周期/Eshell 合并回归共 133 项全部通过。`make remote-check`
17 组共 683 项，680 通过、3 条件跳过；随后扩充同一严格编译 gate，43 个文件通过，
其中新增 4 个 Eshell / ACP / queue / Agent config 消费模块，不排除 docstring 告警。
编译也修正了 Eshell 用 `source-directory` 作形参遮蔽 Emacs 动态变量、Vertico
动态设置缺少声明，以及几条已有 docstring 宽度问题；未改变构建命令与参数位置。

本轮编辑前另存了启动基线。最终 `state-check` 的差异为 2 行删除、3 行增加，全部
是并行 backend 工作的三个 listing/completion advice：`remote-backend-tramp-rpc--keep-listing-a`、
`--keep-listing-attributes-a` 和 `--keep-completions-a`；没有 Eshell/ACP 的全局 hook、
按键、设置或 timer 差异。未改写 baseline，也未把共享树的结果称为零差异。

## 尚未证明

- Aaron-PC 当前 SSH 配置指向 `10.31.1.109:22`；本轮实测返回 `No route to host`。
  终端真机用例因此未通过，不能沿用上一轮 ACP 的 Aaron-PC 成绩替代本轮验证。
- Portal native API 只报告 `exited`，不提供退出码；未知退出没有被当作成功。
- Portal engine 只提供客户端主机的 OSC 7 路径。其他 target 的交互 `cd` 尚不能
  回写 Emacs cwd，buffer 保留已确认的 target 与启动目录；需要扩展 native 事实 API，
  不能伪装 OSC 主机名或把 target 路径当作本机路径。
- native Terminal 的 URL 回调当前直接调用 `NSWorkspace.shared.open`，尚不携带
  owning target 进入 Emacs。显式来源接口已通过 workspace channel 转发和 HTTP
  页面实测，但仍需把 native 点击事件接到这条入口。
- 官方入口已经保留 unset 请求，但标准 TRAMP 的 direct-async argv 生成目前跳过
  nil 环境值；这不等于目标继承值已被真正删除。RPC 的对应删除语义也没有真机证明。
  环境差分单测不能替代 backend 的 unset 验收，后续仍须补齐这一执行契约。
- 真实物理断网与远端长期运行仍未验收；显式断线注入和正常关闭不替代它们。

后续也处理了 ACP API 的精确会话查询与项目分页热点，测量和兼容边界见
[ACP 检查记录](agent-remote-performance-2026-11.md)。
