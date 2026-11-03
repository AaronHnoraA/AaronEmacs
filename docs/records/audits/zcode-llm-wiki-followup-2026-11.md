# ZCode 与 LLM Wiki 接续核对：权限、取消和来源归档

接续 [前次研究](zcode-llm-wiki-agent-2026-11.md)。ZCode 仍按固定修订
[`a60ba6a`](https://github.com/zai-org/ZCode/tree/a60ba6a7e5a06742270ca605f5d2cc2766afa7e1)
核对；LLM Wiki 参照[原文](https://gist.github.com/karpathy/442a6bf555914893e9891c11519de94f)。
这里记录源码对照和本轮验证，不替代现行契约。

## ZCode 的执行归属与本地缺陷

ZCode 的 [turn cancellation](https://github.com/zai-org/ZCode/blob/a60ba6a7e5a06742270ca605f5d2cc2766afa7e1/apps/zcode-cli/docs/design/v2/turn-cancellation.md)
把取消限定在一个活动 turn；模型、工具和权限等待接收同一取消信号。
[permission responder race](https://github.com/zai-org/ZCode/blob/a60ba6a7e5a06742270ca605f5d2cc2766afa7e1/apps/zcode-cli/docs/design/v2/permission-responder-race.md)
让 hook 与人的应答竞速，首个决定收口，迟到方不再生效。这是 ZCode 自己的
执行器契约；Emacs 不接管 ACP provider 的工具循环（本地 C4）。

本地可复现的问题在边界上：Noema Research 的审批交给异步 broker 后，用户停止
回合，agent-shell 先对原生请求发出 `cancelled`；broker 随后回传 `allow`，旧的
`:respond` 闭包又向同一 ACP 请求发送一次 `allow`。真实 stdio fixture 的事件日志
在修复前依次记录两次回应。C15 的回调身份检查原来只覆盖安装 responder 时已有的
待处理请求，没有覆盖新到请求传给 responder 的直接路径。

修复在 Emacs 的 ACP 边界包装领域 responder 的回调：仅当原 buffer、物理 state、
client 和 request ID 仍待决时转发。它不替 provider 决定批准或拒绝；取消、重启、
另一次审批已收口时，迟到回应只失效。`agent-manager-c15-a-*` 两项真实 stdio
测试覆盖正常应答一次，以及取消后的迟到应答；Agent 生命周期 48 项通过，
Noema worker 的权限归属两项通过。

## LLM Wiki 的来源层与 Noema 现状

LLM Wiki 的 [core idea](https://gist.github.com/karpathy/442a6bf555914893e9891c11519de94f)
区分不可变原始材料与可修订 Wiki；一次 ingest 可以更新已有实体、概念和综述页，
并随新材料校正矛盾。前次研究把 Noema 的 `agent:document:edit` / `propose` 记作
“已有 Ingest”。准确说，它们是**单篇 Markdown 的版本化写入与审阅接口**
（[Pi C25](../../design/pi-sessions.md)），Agent 可以多次调用，但目前没有一个
以“这份来源”为输入、搜索既有页面、建议跨页更新并核对引用的成套工作流。

现有 `/file`（C58）把上一条回复交给新页工作台，由人检查标题、位置和标签；
相同标题会被拒绝，近似标题会提示。它解决聊天结果易丢的问题，但不表示回复
已经核对原始来源，也不自动更新既有页。Wiki 的搜索、Wanted 与 Reports 已能
给 Agent 发现材料和链接，`wiki.db` 是 Git Markdown 的投影，不应再复制索引。

后续若实现来源 ingest，应先定四个产品问题：来源文件或 URL 如何标识及重读；
事实与原文的引用如何保留；已有页的修改何时直接应用、何时提交审阅卡片；
部分页面冲突时如何报告已写与未写的范围。单文档 CAS 和审阅卡片是可复用的写入
边界，不能把若干独立写入称作一次原子跨页事务。

项目级的人工维护路径现已写入 [Wiki 设计](../../noema/design/wiki-workspace.md#source-driven-project-wiki-maintenance)、
[项目工作流](../../noema/guide/research-workflow.md#用新来源维护-wiki)和 Pi 的
`noema-project-docs` Skill：从可重读来源出发，搜索并更新相关旧页，标明来源、
解释和不确定处，最后报告逐篇结果。这是现有搜索与单篇编辑能力的使用方式；
自动来源 ingest 仍需另行设计。
