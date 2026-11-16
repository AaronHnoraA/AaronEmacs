# Noema 自动记忆

自动记忆作为 Agent 上下文的选择、预算与回执规则由 [agent-context.md](agent-context.md) 维护；本文负责记忆后端、模型、凭据和索引。

Noema 的普通 ACP 会话和研究 Run 共用一个项目记忆范围。会话完成后后台提取记忆；下一次提问时自动召回相关内容。ACP 的可见输入和原生会话记录保持原样，召回内容通过额外的上下文块传入。研究 Run 在冻结 RunSpec 时把召回内容纳入既有的 64 KiB 上下文预算；`@@ctx(none)` 同时关闭这部分自动上下文。

记忆只提供未核实的会话背景。研究结论仍通过 Noema 原有的 Finding 审核与证据路径；未经审核的记忆不会成为 Finding 或研究证据。对话和 Run 使用同一个 Hy Memory 索引，以 Noema 项目根目录隔离：普通会话按 `noema-project-scope` 取分区，子目录中的会话与 Run 落在同一分区，项目外按目录分区。

Agent 想用别的问题查时调用 `agent:memory:search`，见 [Agent 管理](../../design/agent-management.md) C52。自动召回的失败会静默跳过，这个接口则如实返回 `state`：只有 `ok` 时空结果才代表没有。

Run 的记忆由冻结与完成流程处理，普通 ACP 钩子跳过 Run、probe、side、MarginNote
和 LaTeX 导出；`/` 开头的原生命令也不召回或写入记忆。没有配置时返回
`unconfigured`；失败、超时或格式错误进入 `suspended` 并带重试等待时间，
不能把缺少 `items` 的异常响应解释为空结果。

## 引用闭环

借鉴 Open Math Harness 的记忆读取侧：被用到的记忆留在前面，长期没人用的记忆退出注入。
Hy Memory 的存储不变，闭环只作用在召回上。

- **M1 召回的记忆可引用。** 注入块里每条带 id 的记忆以 `[mem:前八位]` 开头，并请模型只在
  某条记忆改变了它的做法时引用它。
- **M2 只有给过的记忆才算被引用。** 回合结束时从回复里解析 `[mem:…]`，只认这一回合召回过的
  id，同一条只记一次；编造的 id 不计。
- **M3 只在项目里留账。** 召回与引用以 `recall`、`cite` 两种事实追加到 Noema 项目根下的
  `.agent/memory/citations.jsonl`（带 `.agent/.gitignore`）；项目之外什么也不写。
- **M4 一回合一组事实。** 召回成功时写 `recall`，回合结束时写 `cite`（没有引用就不写），
  然后清掉这一回合的召回记录。
- **M5 计数只在桥里。** `scripts/noema-hy-memory.py` 的 `search` 读账本：被引用过的排在前面，
  被召回 6 次以上却从未被引用的不再返回；仍按 4 条、3600 字符的上限截取。
  `agent:memory:search` 的结果带每条的 `recalled` 与 `cited`。
- **M6 查不了不等于没有。** Hy Memory 在 embedding 失败时返回空结果而不报错；桥的传输层记下
  embedding 请求的失败，搜索没有结果且有失败时如实报错，调用方据此进入 `suspended`。

测试：`tests/elisp/noema-hy-memory-tests.el`（M1–M4）、`tests/hy-memory-citations.test.ts`（M5）。

## 模型和数据

- 记忆提取使用 Noema Pi 已配置的 `openai-codex/gpt-5.6-luna` 与现有 Codex 登录态。Noema 的 Pi 管理器和记忆层复用同一套凭据入口；记忆请求通过短暂运行的 Pi 模型适配器发送，不保存第二份令牌，也不使用 OpenAI Platform API key。
- Pi 运行时由 Emacs 的 `make pi-setup` 固定安装在 `var/agent/pi/runtime/`，不查找系统 Pi。Pi client 与记忆适配器使用相同的系统 CA bundle 约定，显式 `NODE_EXTRA_CA_CERTS` 优先；原有 Hy Memory 索引与 embedding 后端保持同一份。
- embedding 使用胖胖的私有 OpenAI 兼容服务：`http://100.64.0.2:19528/v1`（NAS 的 Tailscale 地址），模型 `Xenova/bge-small-zh-v1.5`，输出 512 维。Codex 登录态本身不提供 embedding。Bearer 令牌保存在权限为 `0600` 的 `~/.config/noema/hy-memory.env`，不会写进仓库。Mac 可通过 `ssh Aaron-nas-remote cat .config/pangpang/embedding-token` 取得令牌。NAS 换地址时，服务必须监听新地址（`Aaron-nas-remote` 的 HostName），客户端的 `MEMORY_EMBEDDER_BASE_URL` 随之更新；服务只绑旧地址时召回按 M6 报失败。更换 embedding 模型或维度时，应新建索引并迁移，不能直接在现有索引上混用向量。
- 该 Mac 上 Python 直连 NAS 偶发路由错误，embedding 请求使用系统 `curl` 发送。令牌和请求体仅放在临时私有文件中，Pi 请求仍使用本机回环地址；无需驻留代理进程。
- 私有索引位于 `~/.local/share/noema/hy-memory/`。SDK 虚拟环境位于 `~/.local/share/noema/hy-memory-venv/`。不需要 Ollama 或本地模型。
- 凭据未配置时，自动记忆保持关闭；Noema 原有会话和 Run 路径照常工作。召回最多等待 3 秒，失败后跳过记忆并在 60 秒内停止重试；写入始终异步进行。服务错误不会写入会话日志。

## 安装与验证

运行 `make memory-setup` 安装固定版本的 Hy Memory SDK，再运行 `make memory-test` 检查桥接层。Emacs 的隐藏 ACP 上下文契约测试位于 `tests/elisp/noema-hy-memory-tests.el`。

当前 Mac 已写入胖胖服务凭据并完成真实写入与召回验证。可用以下命令查看配置就绪状态：

```sh
~/.local/share/noema/hy-memory-venv/bin/python scripts/noema-hy-memory.py status
```

输出 `{"configured":true}` 表示接点已配置；运行时服务故障仍由 ACP 和研究 Run 接点按超时与断路策略跳过记忆。
