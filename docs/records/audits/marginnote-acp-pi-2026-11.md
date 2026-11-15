# MarginNote / ACP / Pi 协作链复核

2026-11-06 至 07，按用户要求停止访问 iPad，以 Mac 验证后续优化。
当前契约在 [MarginNote agent 接口](../../noema/design/marginnote-agent-bridge.md) MN-C11、MN-C28–29、
[Pi 会话](../../design/pi-sessions.md) C11 及 [Agent 管理](../../design/agent-management.md) C60–62。
本记录不代替这些文档。

## 结果

- **设备与来源。** 文档 OCR 的 `env`、插件状态、`discover` 和导出显式使用同一设备；仅环境
  检查省略时为 Mac，来源查询与导出必须指定设备，在访问前拒绝缺失参数。
  不再顺带探测另一端，也不让进程环境变量改变 discover 的选择。带设备的原生来源身份纳入
  workflow 增量键；没有设备的旧离线来源仍可读取，但不推断其设备或迁移进度。
- **历史与增量。** 新增 `mn_history.py`，只读运行时公布的学习集检查点目录，保存来源、完整目录
  指纹和有界摘要；可比较两次目录的新增、消失及元数据变化。Mac 实测由 695 到 698 个检查点，
  新增 3 个；这不表明当前笔记变了 3 次，也不能决定某个区域能否复用 OCR。
  当前源库存、视图/素材完整性和 OCR 基线继续独立核对。工具不恢复、不创建快照、不读取旧版正文。
- **ACP 与 Emacs 入口。** MarginNote 的专用默认为 Terra/xhigh；初始化后使用公共 ACP 核验实际
  model/effort，未生效保留草稿。发送后的未知结果不变成新的“未发送草稿”。普通会话设置不变。
  实际后台 Mac worker 为 `gpt-6.1-terra` / `xhigh` / session-local；旧任务和 Pi native ID 保留。
- **服务。** 磁盘 `emacs:agent` 描述已有任务参数，长期运行的 host 仍用旧注册表。确认没有
  Jupyter 实例、导出任务、待保存写入且 worker 空闲后，由 Emacs owner 在原端口重载 host，
  live describe 已返回 `modelId` 和 `effort`；不重启 ACP client。新增 MCP → HTTP 封装 →
  registry → Emacs gateway 的字段透传回归。
- **Pi 与 skills。** 修复三个开发 skill 中未加引号的 YAML 描述，实际全局目录及逐项读取通过。
  OCR 协作说明明确 Pi 可委派取材并读取 Markdown 范围摘要，不能自称看过图像。全局 Pi 经
  真实工具只读复核新参数和两个协作 skill，确认其执行边界，无需用户搬运图片或结果。

## 验证与复核

证据保存在 `var/noema/marginnote/mac-strategy/`，包括设备观察、历史回执、环境报告、任务回执、
测试日志、设置 SHA 和复核报告。关键记录为 `live-agent-api-after.json`、`history-live-final.json`、
`agent-31819-16-result.json` 与 `final-review.md`。具体命令和最终计数见 `verification.json`。

共享 ACP 分工：13 Mac 视区审计、14 服务审计、15 skill 修复、16 Pi 在线核验、17 独立复核、
18 修复复核（完整 ID 前缀 `agent-31819-`）。发现的 discover 设备遗漏、历史读取依赖无关路由、
JSON 布尔/数字比较差异和旧测试双设备假设均有针对性修正及回归。

Emacs `research-test` 各组通过，另单跑最终 MarginNote 13 项通过；启动状态比较 0 差异。
Python 的真实独立 workflow 测试覆盖页身份、增量、人工编辑、冲突及幂等写入。
服务定向测试通过。审计 worker 曾运行全库测试，遇到沙盒回环 EPERM、超时及既有图像布局断言
失败，不能将其表述为全库通过；本轮没有把这些无关模块纳入修改。

## 真实边界

Mac `/context` 有 Meeting 焦点名，但同窗口 `/writing/context` 的可见 surface 是两个文档面板，
没有可见脑图。因而没有生成 Mac 当前脑图 OCR capture，也没有把 iPad 的视区公式移植给 Mac。
已有 Mac 布局矩形变换不等于相机/缩放视区映射；后者继续作为 MN-G7 的未验证能力。
历史目录不是逐卡编辑流水。用户图像、手写原稿和现有 OCR 正文未因本轮基础设施优化而改写。
