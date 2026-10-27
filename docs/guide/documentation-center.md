# Emacs 与 Noema 文档

这里是这套 Emacs 配置及 Noema 的阅读入口。文档描述仓库当前行为；修改设置仍使用
Dashboard 的 `[config]` 或 `M-x config-board`，它们打开同一个原生 Customize 页面。

## Emacs 配置

- [结构总览](../architecture.md)：加载顺序、模块归属和 Emacs ↔ Noema 边界。
- [快速开始](quick-start.md)：安装、恢复和第一次启动。
- [日常使用](daily-usage.md)：Dashboard、按键和常用工作流。
- [设置手册](settings-cookbook.md)：想修改一项设置时该去哪里。
- [开发指南](dev-guide.md)：语言服务、Remote、终端和 Agent 的入口。

## Noema

- [Noema 仓库地图](../noema/architecture.md)：编辑器、Node host、Go kernel 的职责。
- [当前架构](../noema/design/current-architecture.md)：系统怎样协作。
- [研究工作流](../noema/guide/research-workflow.md)：从资料到工作文档和结果。
- [Wiki 工作区](../noema/design/wiki-workspace.md)：链接、搜索和发布边界。
- [Agent 上下文](../noema/design/agent-context.md)：会话引用、冻结输入和回执。

## 配置与维护

- [Config 后端](../design/config-management.md)：注册表、Customize、存储文件与写回。
- [界面开发规范](../design/elegant-ui.md)：Aaron 主题、ETAF、ETAF UI 和浮窗。
- [文档总索引](../README.md)：全部 Emacs 与 Noema 文档。
- [维护手册](../ops/maintenance.md)：包锁、健康检查和恢复。

官方 Emacs 手册使用内置 Info 阅读：`M-x info-emacs-manual`。文档浮窗顶部的
“Emacs 官方手册”也会打开同一个 Info 页面。

要询问这套配置，使用 `M-x +config/ask-agent`，或文档浮窗顶部的
“Ask · 询问配置”。问题在 minibuffer 输入，简短回复也在回显区显示；
后台调用现有 Codex ACP，不打开完整会话窗口；一次提问只显示最终答案。
默认模型是 `gpt-6-luna`，
可在原生 Config 的 `+config-ask-model` 中修改。

文档右上角的红、黄、绿三个圆点依次为关闭、上一页、下一页。链接打开的文档
留在同一个可拖动浮窗里，可以按访问顺序返回或前进。
